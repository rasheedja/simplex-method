-- |
-- Module: Linear.Expr.TypesSpec
-- Description: Tests for Linear.Expr.Types
-- Copyright: (c) Junaid Rasheed, 2020-2024
-- License: BSD-3
-- Maintainer: Junaid Rasheed <jrasheed178@gmail.com>
-- Stability: experimental
module Linear.Expr.UtilSpec where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import Linear.Expr.Types (Expr (..))
import Linear.Expr.Util
  ( addExpr
  , exprToList
  , exprVars
  , listToExpr
  , negateExpr
  , simplifyExpr
  , substVarExpr
  , subtractExpr
  , sumExprConstTerms
  , zeroConstExpr
  )
import Linear.Term.Types (Term (..))
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (counterexample, elements)
import TestUtil (evalExpr, genVarMap)

spec :: Spec
spec = do
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
        )
        $ simplifiedTwiceExpr == simplifiedExpr
    prop "composing listToExpr and exprToList is the same as id" $ \expr -> do
      let exprConvertedTwice = listToExpr (exprToList expr)
      counterexample
        ( "expr: "
            <> show expr
            <> "\nexprConvertedTwice: "
            <> show exprConvertedTwice
        )
        $ exprConvertedTwice == expr
    prop "summing c terms is the same as evaluating the expr with zero coefficients" $ \expr -> do
      let vars = Set.toList $ exprVars expr
          varMap = Map.fromList $ map (,0) vars
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
        )
        $ constSum == exprEval
    prop "negating and evaluating is the same as negating the evaluation" $ \expr -> do
      let vars = Set.toList $ exprVars expr
      varMap <- genVarMap vars
      let negatedExpr = negateExpr expr
          exprEval = evalExpr varMap expr
          exprEvalNegated = negate exprEval
          negatedExprEval = evalExpr varMap negatedExpr
      pure
        $ counterexample
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
          )
        $ exprEvalNegated == negatedExprEval
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
        )
        $ negatedTwiceSimpleExpr == simplifiedExpr
    prop "addExpr is the same as evaluating the sum of the exprs" $ \expr1 expr2 -> do
      let vars = Set.toList $ exprVars expr1 <> exprVars expr2
      varMap <- genVarMap vars
      let addExpr1Expr2 = addExpr expr1 expr2
          addExpr1Expr2Eval = evalExpr varMap addExpr1Expr2
          expr1Eval = evalExpr varMap expr1
          expr2Eval = evalExpr varMap expr2
          sumExpr1EvalExpr2Eval = expr1Eval + expr2Eval
      pure
        $ counterexample
          ( "expr1: "
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
          )
        $ addExpr1Expr2Eval == sumExpr1EvalExpr2Eval
    prop "subtractExpr is the same as evaluating the difference of the exprs" $ \expr1 expr2 -> do
      let vars = Set.toList $ exprVars expr1 <> exprVars expr2
      varMap <- genVarMap vars
      let subtractExpr1Expr2 = subtractExpr expr1 expr2
          subtractExpr1Expr2Eval = evalExpr varMap subtractExpr1Expr2
          expr1Eval = evalExpr varMap expr1
          expr2Eval = evalExpr varMap expr2
          diffExpr1EvalExpr2Eval = expr1Eval - expr2Eval
      pure
        $ counterexample
          ( "expr1: "
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
          )
        $ subtractExpr1Expr2Eval == diffExpr1EvalExpr2Eval
    prop "substVarExpr with the same variable is the same as simplfying" $ \expr -> do
      let vars = Set.toList $ exprVars expr
      var <- elements vars
      varMap <- genVarMap vars
      let varReplacement = Expr (VarTerm var :| [])
          exprSubst = substVarExpr var varReplacement expr
          exprSimplified = simplifyExpr expr
          exprSubstEval = evalExpr varMap exprSubst
          exprSimplifiedEval = evalExpr varMap exprSimplified
      pure
        $ counterexample
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
          )
        $ exprSubstEval == exprSimplifiedEval
    prop
      "substVarExpr with a constant is the same as evaluating with the variable mapped to the constant"
      $ \expr c -> do
        let varReplacement = Expr (ConstTerm c :| [])
        let vars = Set.toList $ exprVars expr
        var <- elements vars
        initialVarMap <- genVarMap vars
        let varMap = Map.insert var c initialVarMap
            substitutedExpr = substVarExpr var varReplacement expr
            substitutedExprEval = evalExpr varMap substitutedExpr
            exprEval = evalExpr varMap expr
        pure
          $ counterexample
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
            )
          $ evalExpr varMap (substVarExpr var varReplacement expr) == evalExpr varMap expr
    prop
      "substVarExpr with an expr is the same as evaluating with the variable mapped to the expr"
      $ \expr exprReplacement -> do
        let vars = Set.toList $ exprVars expr <> exprVars exprReplacement
        var <- elements vars
        initialVarMap <- genVarMap vars
        let exprReplacementEval = evalExpr initialVarMap exprReplacement
            varMap = Map.insert var exprReplacementEval initialVarMap
            substitutedExpr = substVarExpr var exprReplacement expr
            exprEval = evalExpr varMap expr
            substitutedExprEval = evalExpr initialVarMap substitutedExpr
        pure
          $ counterexample
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
            )
          $ substitutedExprEval == exprEval
    prop "zeroConstExpr correctly zeroes constant terms in expressions" $ \expr -> sumExprConstTerms (zeroConstExpr expr) == 0
