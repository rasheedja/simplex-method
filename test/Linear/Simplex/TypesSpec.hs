module Linear.Simplex.TypesSpec (spec) where

import Prelude

import Control.Monad (forM)
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Linear.Simplex.Types

import qualified Debug.Trace as T

termVar :: Term -> Maybe Var
termVar (VarTerm v) = Just v
termVar (CoeffTerm _ v) = Just v
termVar _ = Nothing

exprVars :: Expr -> Set.Set Var
exprVars = Set.fromList . Maybe.catMaybes . map termVar . exprToList

constraintVars :: Constraint -> Set.Set Var
constraintVars (lhs :<= rhs) = exprVars lhs <> exprVars rhs
constraintVars (lhs :>= rhs) = exprVars lhs <> exprVars rhs
constraintVars (lhs :== rhs) = exprVars lhs <> exprVars rhs

simpleConstraintVars :: SimpleConstraint -> Set.Set Var
simpleConstraintVars (lhs :<= rhs) = exprVars lhs
simpleConstraintVars (lhs :>= rhs) = exprVars lhs
simpleConstraintVars (lhs :== rhs) = exprVars lhs

simpleSystemVars :: SimpleSystem -> Set.Set Var
simpleSystemVars = Set.unions . map simpleConstraintVars

-- data Term = ConstTerm SimplexNum | CoeffTerm SimplexNum Var | VarTerm Var -- Consider VarTerm Var - note, we must consider normalizing this: Considered. It makes going to standard form easier due to type safety
--   deriving (Show, Read, Eq, Ord, Generic)

-- TODO: consider type NumberConstraint = GenericConstraint SimplexNum SimplexNum

evalTerm :: VarLitMap -> Term -> SimplexNum
evalTerm _ (ConstTerm c) = c
evalTerm varMap (CoeffTerm c v) = c * (Map.findWithDefault (error $ "evalTerm: " <> show v <> " not found in varMap " <> show varMap) v varMap)
evalTerm varMap (VarTerm v) = Map.findWithDefault (error $ "evalTerm: " <> show v <> " not found in varMap " <> show varMap) v varMap

evalExpr :: VarLitMap -> Expr -> SimplexNum
evalExpr varMap expr = sum $ map (evalTerm varMap) $ exprToList expr

evalConstraint :: VarLitMap -> Constraint -> Bool
evalConstraint varMap (lhs :<= rhs) = evalExpr varMap lhs <= evalExpr varMap rhs
evalConstraint varMap (lhs :>= rhs) = evalExpr varMap lhs >= evalExpr varMap rhs
evalConstraint varMap (lhs :== rhs) = evalExpr varMap lhs == evalExpr varMap rhs

evalSimpleConstraint :: VarLitMap -> SimpleConstraint -> Bool
evalSimpleConstraint varMap (lhs :<= rhs) = evalExpr varMap lhs <= rhs
evalSimpleConstraint varMap (lhs :>= rhs) = evalExpr varMap lhs >= rhs
evalSimpleConstraint varMap (lhs :== rhs) = evalExpr varMap lhs == rhs

evalSimpleSystem :: VarLitMap -> SimpleSystem -> Bool
evalSimpleSystem varMap = all (evalSimpleConstraint varMap)

genVarMap :: [Var] -> Gen VarLitMap
genVarMap vars = do
  varVals <- forM vars $ const arbitrary
  pure $ Map.fromList $ zip vars varVals

spec :: Spec
spec = do
  describe "Term" $ do
    prop "simplifying leads to same evaluation" $ \term -> do
      varMap <- maybe (pure Map.empty) (genVarMap . List.singleton) $ termVar term
      let simplifiedTerm = simplifyTerm term
          termEval = evalTerm varMap term
          simplifiedTermEval = evalTerm varMap simplifiedTerm
      pure $
        counterexample
          ( "term: "
              <> show term
              <> "simplifiedTerm: "
              <> show simplifiedTerm
              <> "\nvarMap: "
              <> show varMap
              <> "\ntermEval: "
              <> show termEval
              <> "\nsimplifiedTermEval: "
              <> show simplifiedTermEval
          ) $
        evalTerm varMap (simplifyTerm term) == evalTerm varMap term
    prop "simplifying twice is the same as simplifying once" $ \term -> do
      let simplifiedTerm = simplifyTerm term
          simplifiedTwiceTerm = simplifyTerm simplifiedTerm
      counterexample
        ( "term: "
            <> show term
            <> "\nsimplifiedTerm: "
            <> show simplifiedTerm
            <> "\nsimplifiedTwiceTerm: "
            <> show simplifiedTwiceTerm
        ) $
        simplifiedTwiceTerm == simplifiedTerm
    prop "negating and evaluating is the same as negating the evaluation" $ \term -> do
      varMap <- maybe (pure $ Map.empty) (genVarMap . List.singleton) $ termVar term
      let
        negatedTerm = negateTerm term
        termEval = evalTerm varMap term
        negatedTermEval = evalTerm varMap negatedTerm
      pure $
        counterexample
          ( "term: "
              <> show term
              <> "\nnegatedTerm: "
              <> show negatedTerm
              <> "\nvarMap: "
              <> show varMap
              <> "\ntermEval: "
              <> show termEval
              <> "\nnegatedTermEval: "
              <> show negatedTermEval
          ) $
          negate termEval == negatedTermEval
    prop "negating  twice is the same as not negating" $ \term -> do
      let simplifiedTerm = simplifyTerm term
          negatedTwiceSimpleTerm = negateTerm (negateTerm simplifiedTerm)
      counterexample
        ( "term: "
            <> show term
            <> "\nsimplifiedTerm: "
            <> show simplifiedTerm
            <> "\nnegatedTwiceSimpleTerm: "
            <> show negatedTwiceSimpleTerm
        ) $
        negatedTwiceSimpleTerm == simplifiedTerm
    prop "zeroConstTerm correctly zeroes constant terms" $ \term -> do
      let termZeroedConsts = zeroConstTerm term
      counterexample
        ( "term: "
            <> show term
            <> "\ntermZeroedConsts: "
            <> show termZeroedConsts
        ) $
        case term of
          ConstTerm _ -> termZeroedConsts == ConstTerm 0
          _ -> termZeroedConsts == term
  describe "Expr" $ do
    prop "simplifying leads to same evaluation" $ \expr -> do
      let vars = Set.toList $ exprVars expr
      varMap <- genVarMap vars
      pure $ evalExpr varMap (simplifyExpr expr) == evalExpr varMap expr
    prop "simplifying twice is the same as simplifying once" $ \expr -> do
      -- expr: (Expr (CoeffTerm (1 % 1) 0) :+ CoeffTerm (1 % 2) (-1)) :+ CoeffTerm (2 % 1) (-1)
      -- 1 = x
      -- -1 = y
      -- 1x + 0.5y + 2y
      -- simplifiedExpr: (Expr (CoeffTerm (1 % 2) (-1)) :+ CoeffTerm (2 % 1) (-1)) :+ VarTerm 0
      -- 0.5y + 2y + x
      -- simplifiedTwiceExpr: Expr (CoeffTerm (5 % 2) (-1)) :+ VarTerm 0
      -- 2.5y + x
      let simplifiedExpr = simplifyExpr expr
      let simplifiedTwiceExpr = simplifyExpr simplifiedExpr
      counterexample
        ( "expr: "
            <> show expr
            <> "\nsimplifiedExpr: "
            <> show simplifiedExpr
            <> "\nsimplifiedTwiceExpr: "
            <> show simplifiedTwiceExpr
        ) $
        simplifiedTwiceExpr == simplifiedExpr
    prop "composing listToExpr and exprToList is the same as id" $ \expr -> do
      let exprConvertedTwice = listToExpr (exprToList expr)
      counterexample
        ( "expr: "
            <> show expr
            <> "\nexprConvertedTwice: "
            <> show exprConvertedTwice
        ) $
        exprConvertedTwice == expr
    prop "summing c terms is the same as evaluating the expr with zero coefficients" $ \expr -> do
      let vars = Set.toList $ exprVars expr
          varMap = Map.fromList $ map (, 0) vars
          constSum = sumExprConstTerms expr
          exprEval = evalExpr varMap expr
      counterexample
        ( "expr: "
            <> show expr
            <> "\nvarMap: "
            <> show varMap
            <> "\nconstSum: "
            <> show constSum
            <> "\nexprEval: "
            <> show exprEval
        ) $
        constSum == exprEval
    prop "negating and evaluating is the same as negating the evaluation" $ \expr -> do
      let vars = Set.toList $ exprVars expr
      varMap <- genVarMap vars
      let negatedExpr = negateExpr expr
          exprEval = evalExpr varMap expr
          exprEvalNegated = negate exprEval
          negatedExprEval = evalExpr varMap negatedExpr
      pure $ counterexample
        ( "expr: "
            <> show expr
            <> "\nnegatedExpr: "
            <> show negatedExpr
            <> "\nvarMap: "
            <> show varMap
            <> "\nexprEval: "
            <> show exprEval
            <> "\nexprEvalNegated: "
            <> show exprEvalNegated
            <> "\nnegatedExprEval: "
            <> show negatedExprEval
        ) $
        exprEvalNegated == negatedExprEval
    prop "negating a simplified expression twice is the same as not negating" $ \expr -> do
      let simplifiedExpr = simplifyExpr expr
          negatedTwiceSimpleExpr = negateExpr (negateExpr simplifiedExpr)
      counterexample
        ( "expr: "
            <> show expr
            <> "\nsimplifiedExpr: "
            <> show simplifiedExpr
            <> "\nnegatedTwiceSimpleExpr: "
            <> show negatedTwiceSimpleExpr
        ) $
        negatedTwiceSimpleExpr == simplifiedExpr
    prop "addExpr is the same as evaluating the sum of the exprs" $ \expr1 expr2 -> do
      let vars = Set.toList $ exprVars expr1 <> exprVars expr2
      varMap <- genVarMap vars
      let addExpr1Expr2 = addExpr expr1 expr2
          addExpr1Expr2Eval = evalExpr varMap addExpr1Expr2
          expr1Eval = evalExpr varMap expr1
          expr2Eval = evalExpr varMap expr2
          sumExpr1EvalExpr2Eval = expr1Eval + expr2Eval
      pure $ counterexample
        ("expr1: "
          <> show expr1
          <> "\nexpr2: "
          <> show expr2
          <> "\nvarMap:"
          <> show varMap
          <> "\naddExpr1Expr2: "
          <> show addExpr1Expr2
          <> "\naddExpr1Expr2Eval: "
          <> show addExpr1Expr2Eval
          <> "\nexpr1Eval: "
          <> show expr1Eval
          <> "\nexpr2Eval: "
          <> show expr2Eval
          <> "\nexpr1EvalPlusExpr2Eval: "
          <> show sumExpr1EvalExpr2Eval
        ) $ addExpr1Expr2Eval == sumExpr1EvalExpr2Eval
    prop "subtractExpr is the same as evaluating the difference of the exprs" $ \expr1 expr2 -> do
      let vars = Set.toList $ exprVars expr1 <> exprVars expr2
      varMap <- genVarMap vars
      let subtractExpr1Expr2 = subtractExpr expr1 expr2
          subtractExpr1Expr2Eval = evalExpr varMap subtractExpr1Expr2
          expr1Eval = evalExpr varMap expr1
          expr2Eval = evalExpr varMap expr2
          diffExpr1EvalExpr2Eval = expr1Eval - expr2Eval
      pure $ counterexample
        ("expr1: "
          <> show expr1
          <> "\nexpr2: "
          <> show expr2
          <> "\nvarMap:"
          <> show varMap
          <> "\nsubtractExpr1Expr2: "
          <> show subtractExpr1Expr2
          <> "\nsubtractExpr1Expr2Eval: "
          <> show subtractExpr1Expr2Eval
          <> "\nexpr1Eval: "
          <> show expr1Eval
          <> "\nexpr2Eval: "
          <> show expr2Eval
          <> "\nexpr1EvalMinusExpr2Eval: "
          <> show diffExpr1EvalExpr2Eval
        ) $ subtractExpr1Expr2Eval == diffExpr1EvalExpr2Eval
    prop "substVarExpr with the same variable is the same as simplfying" $ \expr -> do
      let vars = Set.toList $ exprVars expr
      var <- elements vars
      varMap <- genVarMap vars
      let varReplacement = Expr (VarTerm var)
          exprSubst = substVarExpr var varReplacement expr
          exprSimplified = simplifyExpr expr
          exprSubstEval = evalExpr varMap exprSubst
          exprSimplifiedEval = evalExpr varMap exprSimplified
      pure $
        counterexample
          ( "expr: "
              <> show expr
              <> "\nvar: "
              <> show var
              <> "\nvarMap: "
              <> show varMap
              <> "\nvarReplacement: "
              <> show varReplacement
              <> "\nsubstVarExpr: "
              <> show exprSubst
              <> "\nsimplifyExpr: "
              <> show exprSimplified
              <> "\nexprSubstEval: "
              <> show exprSubstEval
              <> "\nexprSimplifiedEval: "
              <> show exprSimplifiedEval
          ) $
          exprSubstEval == exprSimplifiedEval
    prop "substVarExpr with a constant is the same as evaluating with the variable mapped to the constant" $ \expr c -> do
      let varReplacement = Expr (ConstTerm c)
      let vars = Set.toList $ exprVars expr
      var <- elements vars
      initialVarMap <- genVarMap vars
      let varMap = Map.insert var c initialVarMap
          substitutedExpr = substVarExpr var varReplacement expr
          substitutedExprEval = evalExpr varMap substitutedExpr
          exprEval = evalExpr varMap expr
      pure $
        counterexample
          ( "expr: "
              <> show expr
              <> "\nvar: "
              <> show var
              <> "\nconst: "
              <> show c
              <> "\nvarMap: "
              <> show varMap
              <> "\nvarReplacement: "
              <> show varReplacement
              <> "\nsubstitutedExpr: "
              <> show substitutedExpr
              <> "\nsubstitutedExprEval: "
              <> show substitutedExprEval
              <> "\nexprEval: "
              <> show exprEval
          ) $
          evalExpr varMap (substVarExpr var varReplacement expr) == evalExpr varMap expr
    prop "substVarExpr with an expr is the same as evaluating with the variable mapped to the expr" $ \expr exprReplacement -> do
      let vars = Set.toList $ exprVars expr <> exprVars exprReplacement
      var <- elements vars
      initialVarMap <- genVarMap vars
      let exprReplacementEval = evalExpr initialVarMap exprReplacement
          varMap = Map.insert var exprReplacementEval initialVarMap
          substitutedExpr = substVarExpr var exprReplacement expr
          exprEval = evalExpr varMap expr
          substitutedExprEval = evalExpr initialVarMap substitutedExpr
      pure $
        counterexample
          ( "expr: "
              <> show expr
              <> "\nvar: "
              <> show var
              <> "\nexprReplacement: "
              <> show exprReplacement
              <> "\ninitialVarMap: "
              <> show initialVarMap
              <> "\nexprReplacementEval: "
              <> show exprReplacementEval
              <> "\nvarMap: "
              <> show varMap
              <> "\nsubstExpr: "
              <> show substitutedExpr
              <> "\nexprEval: "
              <> show exprEval
              <> "\nsubstExprEval: "
              <> show substitutedExprEval
          ) $ substitutedExprEval == exprEval
    prop "zeroConstExpr correctly zeroes constant terms in expressions" $ \expr -> sumExprConstTerms (zeroConstExpr expr) == 0
  describe "SimpleConstraint" $ do
    it "substVarSimpleConstraint with a constant is the same as evaluating with the variable mapped to the constant" $ do
      property $ \simpleConstraint c -> do
        let vars = Set.toList $ simpleConstraintVars simpleConstraint
        var <- elements vars
        let varReplacement = Expr (ConstTerm c)
        initialVarMap <- genVarMap vars
        let varMap = Map.insert var c initialVarMap
            substitutedSimpleConstraint = substVarSimpleConstraint var varReplacement simpleConstraint
            substitutedSimpleConstraintEval = evalSimpleConstraint varMap substitutedSimpleConstraint
            simpleConstraintEval = evalSimpleConstraint varMap simpleConstraint
        pure $
          counterexample
            ( "simpleConstraint: "
                <> show simpleConstraint
                <> "\nvar: "
                <> show var
                <> "\nconst: "
                <> show c
                <> "\nvarMap: "
                <> show varMap
                <> "\nvarReplacement: "
                <> show varReplacement
                <> "\nsubstitutedSimpleConstraint: "
                <> show substitutedSimpleConstraint
                <> "\nsubstitutedSimpleConstraintEval: "
                <> show substitutedSimpleConstraintEval
                <> "\nsimpleConstraintEval: "
                <> show simpleConstraintEval
            ) $
            substitutedSimpleConstraintEval == simpleConstraintEval
    it "substVarSimpleConstraint with an expr is the same as evaluating with the variable mapped to the expr" $ do
      property $ \simpleConstraint exprReplacement -> do
        let vars = Set.toList $ simpleConstraintVars simpleConstraint <> exprVars exprReplacement
        var <- elements vars
        initialVarMap <- genVarMap vars
        let exprReplacementEval = evalExpr initialVarMap exprReplacement
            varMap = Map.insert var exprReplacementEval initialVarMap
            substitutedSimpleConstraint = substVarSimpleConstraint var exprReplacement simpleConstraint
            simpleConstraintEval = evalSimpleConstraint varMap simpleConstraint
            substitutedSimpleConstraintEval = evalSimpleConstraint initialVarMap substitutedSimpleConstraint
        pure $
          counterexample
            ( "simpleConstraint: "
                <> show simpleConstraint
                <> "\nvar: "
                <> show var
                <> "\nexprReplacement: "
                <> show exprReplacement
                <> "\ninitialVarMap: "
                <> show initialVarMap
                <> "\nexprReplacementEval: "
                <> show exprReplacementEval
                <> "\nvarMap: "
                <> show varMap
                <> "\nsubstitutedSimpleConstraint: "
                <> show substitutedSimpleConstraint
                <> "\nsimpleConstraintEval: "
                <> show simpleConstraintEval
                <> "\nsubstitutedSimpleConstraintEval: "
                <> show substitutedSimpleConstraintEval
            ) $
            substitutedSimpleConstraintEval == simpleConstraintEval
    it "constraintToSimpleConstraint leads to the same evaluation" $ do
      property $ \constraint -> do
        let vars = Set.toList $ constraintVars constraint
        varMap <- genVarMap vars
        let simpleConstraint = constraintToSimpleConstraint constraint
            constraintEval = evalConstraint varMap constraint
            simpleConstraintEval = evalSimpleConstraint varMap simpleConstraint
        pure $
          counterexample
            ( "constraint: "
                <> show constraint
                <> "\nsimpleConstraint: "
                <> show simpleConstraint
                <> "\ninitialVarMap: "
                <> show varMap
                <> "\nconstraintEval: "
                <> show constraintEval
                <> "\nsimpleConstraintEval"
                <> show simpleConstraintEval
            ) $
            constraintEval == simpleConstraintEval
    it "normalizeSimpleConstraint leads to the same evaluation" $ do
      property $ \simpleConstraint -> do
        let vars = Set.toList $ simpleConstraintVars simpleConstraint
        varMap <- genVarMap vars
        let normalizedSimpleConstraint = normalizeSimpleConstraint simpleConstraint
            simpleConstraintEval = evalSimpleConstraint varMap simpleConstraint
            normalizedSimpleConstraintEval = evalSimpleConstraint varMap normalizedSimpleConstraint
        pure $
          counterexample
            ( "simpleConstraint: "
                <> show simpleConstraint
                <> "\nnormalizedSimpleConstraint: "
                <> show normalizedSimpleConstraint
                <> "\ninitialVarMap: "
                <> show varMap
                <> "\nsimpleConstraintEval: "
                <> show simpleConstraintEval
                <> "\nnormalizedSimpleConstraintEval"
                <> show normalizedSimpleConstraintEval
            ) $
            simpleConstraintEval == normalizedSimpleConstraintEval
    it "simplifyCoeff leads to the same evaluation" $ do
      property $ \simpleConstraint -> do
        let vars = Set.toList $ simpleConstraintVars simpleConstraint
        varMap <- genVarMap vars
        let simplifiedSimpleConstraint = simplifyCoeff simpleConstraint
            simpleConstraintEval = evalSimpleConstraint varMap simpleConstraint
            simplifiedSimpleConstraintEval = evalSimpleConstraint varMap simplifiedSimpleConstraint
        pure $
          counterexample
            ( "simpleConstraint: "
                <> show simpleConstraint
                <> "\nsimplifiedSimpleConstraint: "
                <> show simplifiedSimpleConstraint
                <> "\ninitialVarMap: "
                <> show varMap
                <> "\nsimpleConstraintEval: "
                <> show simpleConstraintEval
                <> "\nsimplifiedSimpleConstraintEval: "
                <> show simplifiedSimpleConstraintEval
            ) $
            simpleConstraintEval == simplifiedSimpleConstraintEval
    it "simplifySimpleConstraint leads to the same evaluation" $ do
      property $ \simpleConstraint -> do
        let vars = Set.toList $ simpleConstraintVars simpleConstraint
        varMap <- genVarMap vars
        let simplifiedSimpleConstraint = simplifySimpleConstraint simpleConstraint
            simpleConstraintEval = evalSimpleConstraint varMap simpleConstraint
            simplifiedSimpleConstraintEval = evalSimpleConstraint varMap simplifiedSimpleConstraint
        pure $
          counterexample
            ( "simpleConstraint: "
                <> show simpleConstraint
                <> "\nsimplifiedSimpleConstraint: "
                <> show simplifiedSimpleConstraint
                <> "\ninitialVarMap: "
                <> show varMap
                <> "\nsimpleConstraintEval: "
                <> show simpleConstraintEval
                <> "\nsimplifiedSimpleConstraintEval: "
                <> show simplifiedSimpleConstraintEval
            ) $
            simpleConstraintEval == simplifiedSimpleConstraintEval
  describe "SimpleSystem" $ do
    it "simplifySimpleSystem leads to the same evaluation" $ do
      property $ \simpleSystem -> do
        let vars = Set.toList $ simpleSystemVars simpleSystem
        varMap <- genVarMap vars
        let simplifiedSimpleSystem = simplifySimpleSystem simpleSystem
            simpleSystemEval = evalSimpleSystem varMap simpleSystem
            simplifiedSimpleSystemEval = evalSimpleSystem varMap simplifiedSimpleSystem
        pure $
          counterexample
            ( "simpleSystem: "
                <> show simpleSystem
                <> "\nsimplifiedSimpleSystem: "
                <> show simplifiedSimpleSystem
                <> "\ninitialVarMap: "
                <> show varMap
                <> "\nsimpleSystemEval: "
                <> show simpleSystemEval
                <> "\nsimplifiedSimpleSystemEval: "
                <> show simplifiedSimpleSystemEval
            ) $
            simpleSystemEval == simplifiedSimpleSystemEval
    it "findHighestVar finds the highest variable in a simple system" $ do
      let simpleSystem1 =
            [ Expr (VarTerm 0) :>= 0
            , Expr (VarTerm 0) :<= 1
            , Expr (VarTerm 1) :>= 0
            , Expr (VarTerm 1) :<= 1
            ]
          simpleSystem100 =
            [ Expr (VarTerm 0) :<= 1
            , Expr (VarTerm 50) :<= 1
            , Expr (VarTerm 100) :<= 1
            ]
          simpleSystem10 =
            [ Expr (VarTerm (-10)) :<= 1
            , Expr (VarTerm 0) :<= 1
            , Expr (VarTerm 10) :<= 1
            ]
          simpleSystemMinus10 =
            [ Expr (VarTerm (-10)) :<= 1
            , Expr (VarTerm (-20)) :<= 1
            ]
          
      findHighestVar simpleSystem1 `shouldBe` 1
      findHighestVar simpleSystem100 `shouldBe` 100
      findHighestVar simpleSystem10 `shouldBe` 10
      findHighestVar simpleSystemMinus10 `shouldBe` (-10)
  describe "Bounds" $ do
    it "validateBounds finds that deriving bounds for a system where -1 <= x <= 1 has valid bounds" $ do
      let simpleSystem =
            [ Expr (VarTerm 0) :>= (-1)
            , Expr (VarTerm 0) :<= 1
            ]
          derivedBounds = deriveBounds simpleSystem
          expectedBounds = Map.fromList [(0, Bounds (Just (-1)) (Just 1))]
      derivedBounds `shouldBe` expectedBounds
      validateBounds derivedBounds `shouldBe` True
    it "validateBounds finds that deriving bounds for a system where 0 <= x <= 1 has valid bounds" $ do
      let simpleSystem =
            [ Expr (VarTerm 0) :>= 0
            , Expr (VarTerm 0) :<= 1
            ]
          derivedBounds = deriveBounds simpleSystem
          expectedBounds = Map.fromList [(0, Bounds (Just 0) (Just 1))]
      derivedBounds `shouldBe` expectedBounds
      validateBounds derivedBounds `shouldBe` True
    it "validateBounds finds that deriving bounds for a system where 1 <= x <= 1 has valid bounds" $ do
      let simpleSystem =
            [ Expr (VarTerm 0) :>= 1
            , Expr (VarTerm 0) :<= 1
            ]
          derivedBounds = deriveBounds simpleSystem
          expectedBounds = Map.fromList [(0, Bounds (Just 1) (Just 1))]
      derivedBounds `shouldBe` expectedBounds
      validateBounds derivedBounds `shouldBe` True
    it "validateBounds finds that deriving bounds for a system where 1 <= x <= 0 has invalid bounds" $ do
      let simpleSystem =
            [ Expr (VarTerm 0) :>= 1
            , Expr (VarTerm 0) :<= 0
            ]
          derivedBounds = deriveBounds simpleSystem
          expectedBounds = Map.fromList [(0, Bounds (Just 1) (Just 0))]
      derivedBounds `shouldBe` expectedBounds
      validateBounds derivedBounds `shouldBe` False
    it "validateBounds finds that deriving bounds for a system where 0 <= x <= 1 and 1 <= y <= 3 has valid bounds" $ do
      let simpleSystem =
            [ Expr (VarTerm 0) :>= 0
            , Expr (VarTerm 0) :<= 1
            , Expr (VarTerm 1) :>= 1
            , Expr (VarTerm 1) :<= 3
            ]
          derivedBounds = deriveBounds simpleSystem
          expectedBounds = Map.fromList [(0, Bounds (Just 0) (Just 1)), (1, Bounds (Just 1) (Just 3))]
      derivedBounds `shouldBe` expectedBounds
      validateBounds derivedBounds `shouldBe` True
    it "validateBounds finds that deriving bounds for a system where 1 <= x <= 0 and 3 <= y <= 1 has invalid bounds" $ do
      let simpleSystem =
            [ Expr (VarTerm 0) :>= 1
            , Expr (VarTerm 0) :<= 0
            , Expr (VarTerm 1) :>= 3
            , Expr (VarTerm 1) :<= 1
            ]
          derivedBounds = deriveBounds simpleSystem
          expectedBounds = Map.fromList [(0, Bounds (Just 1) (Just 0)), (1, Bounds (Just 3) (Just 1))]
      derivedBounds `shouldBe` expectedBounds
      validateBounds derivedBounds `shouldBe` False
    it "validateBounds finds that deriving bounds for a system where 1 <= x <= 0 and 1 <= y <= 3 has invalid bounds" $ do
      let simpleSystem =
            [ Expr (VarTerm 0) :>= 1
            , Expr (VarTerm 0) :<= 0
            , Expr (VarTerm 1) :>= 1
            , Expr (VarTerm 1) :<= 3
            ]
          derivedBounds = deriveBounds simpleSystem
          expectedBounds = Map.fromList [(0, Bounds (Just 1) (Just 0)), (1, Bounds (Just 1) (Just 3))]
      derivedBounds `shouldBe` expectedBounds
      validateBounds derivedBounds `shouldBe` False
    it "validateBounds finds that deriving bounds for a system where 0 <= x <= 1 and 3 <= y <= 1 has invalid bounds" $ do
      let simpleSystem =
            [ Expr (VarTerm 0) :>= 0
            , Expr (VarTerm 0) :<= 1
            , Expr (VarTerm 1) :>= 3
            , Expr (VarTerm 1) :<= 1
            ]
          derivedBounds = deriveBounds simpleSystem
          expectedBounds = Map.fromList [(0, Bounds (Just 0) (Just 1)), (1, Bounds (Just 3) (Just 1))]
      derivedBounds `shouldBe` expectedBounds
      validateBounds derivedBounds `shouldBe` False
    it "removeUselessSystemBounds removes x <= 3 when bounds say x <= 2" $ do
      let simpleSystem =
            [ Expr (VarTerm 0) :<= 2
            , Expr (VarTerm 0) :<= 3
            ]
          bounds = Map.fromList [(0, Bounds (Just 0) (Just 2))]
          simplifiedSimpleSystem = removeUselessSystemBounds simpleSystem bounds
          expectedSimpleSystem = [Expr (VarTerm 0) :<= 2]
      simplifiedSimpleSystem `shouldBe` expectedSimpleSystem
    it "removeUselessSystemBounds does not remove x <= 2 when bounds say x <= 2" $ do
      let simpleSystem =
            [ Expr (VarTerm 0) :<= 2
            ]
          bounds = Map.fromList [(0, Bounds (Just 0) (Just 2))]
          simplifiedSimpleSystem = removeUselessSystemBounds simpleSystem bounds
          expectedSimpleSystem = [Expr (VarTerm 0) :<= 2]
      simplifiedSimpleSystem `shouldBe` expectedSimpleSystem
    it "removeUselessSystemBounds removes x >= 3 when bounds say x >= 4" $ do
      let simpleSystem =
            [ Expr (VarTerm 0) :>= 4
            , Expr (VarTerm 0) :>= 3
            ]
          bounds = Map.fromList [(0, Bounds (Just 4) (Just 5))]
          simplifiedSimpleSystem = removeUselessSystemBounds simpleSystem bounds
          expectedSimpleSystem = [Expr (VarTerm 0) :>= 4]
      simplifiedSimpleSystem `shouldBe` expectedSimpleSystem
    it "removeUselessSystemBounds does not remove x >= 4 when bounds say x >= 4" $ do
      let simpleSystem =
            [ Expr (VarTerm 0) :>= 4
            ]
          bounds = Map.fromList [(0, Bounds (Just 4) (Just 5))]
          simplifiedSimpleSystem = removeUselessSystemBounds simpleSystem bounds
          expectedSimpleSystem = [Expr (VarTerm 0) :>= 4]
      simplifiedSimpleSystem `shouldBe` expectedSimpleSystem
    it "removeUselessSystemBounds does not remove 0 <= x <= 2 when bounds say 0 <= x <= 2" $ do
      let simpleSystem =
            [ Expr (VarTerm 0) :>= 0
            , Expr (VarTerm 0) :<= 2
            ]
          bounds = Map.fromList [(0, Bounds (Just 0) (Just 2))]
          simplifiedSimpleSystem = removeUselessSystemBounds simpleSystem bounds
          expectedSimpleSystem = [Expr (VarTerm 0) :>= 0, Expr (VarTerm 0) :<= 2]
      simplifiedSimpleSystem `shouldBe` expectedSimpleSystem
    it "removeUselessSystemBounds removes upper bound of 0 <= x <= 2 when bounds say 0 <= x <= 1" $ do
      let simpleSystem =
            [ Expr (VarTerm 0) :>= 0
            , Expr (VarTerm 0) :<= 2
            ]
          bounds = Map.fromList [(0, Bounds (Just 0) (Just 1))]
          simplifiedSimpleSystem = removeUselessSystemBounds simpleSystem bounds
          expectedSimpleSystem = [Expr (VarTerm 0) :>= 0]
      simplifiedSimpleSystem `shouldBe` expectedSimpleSystem
    it "removeUselessSystemBounds removes lower bound of 0 <= x <= 2 when bounds say 1 <= x <= 2" $ do
      let simpleSystem =
            [ Expr (VarTerm 0) :>= 0
            , Expr (VarTerm 0) :<= 2
            ]
          bounds = Map.fromList [(0, Bounds (Just 1) (Just 2))]
          simplifiedSimpleSystem = removeUselessSystemBounds simpleSystem bounds
          expectedSimpleSystem = [Expr (VarTerm 0) :<= 2]
      simplifiedSimpleSystem `shouldBe` expectedSimpleSystem
    it "removeUselssSystemBounds only removes constraints of the form x <= c" $ do
      let simpleSystem =
            [ Expr (VarTerm 0) :<= 2
            , Expr (VarTerm 0) :<= 3
            , Expr (CoeffTerm 2 0) :<= 6
            ]
          bounds = Map.fromList [(0, Bounds (Just 0) (Just 2))]
          simplifiedSimpleSystem = removeUselessSystemBounds simpleSystem bounds
          expectedSimpleSystem = [Expr (VarTerm 0) :<= 2, Expr (CoeffTerm 2 0) :<= 6]
      simplifiedSimpleSystem `shouldBe` expectedSimpleSystem
