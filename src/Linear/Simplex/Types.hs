-- |
-- Module      : Linear.Simplex.Types
-- Description : Custom types
-- Copyright   : (c) Junaid Rasheed, 2020-2023
-- License     : BSD-3
-- Maintainer  : jrasheed178@gmail.com
-- Stability   : experimental
module Linear.Simplex.Types where

import Control.Lens hiding (Const)
import Data.Generics.Labels ()
import qualified Data.Set as Set
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import GHC.Generics (Generic)
import Control.Applicative ((<|>))
import GHC.Base (liftA2)

import Test.QuickCheck (Arbitrary(..), genericShrink, oneof)

import qualified Debug.Trace as T

-- Inputs:
-- linear expressions (>=,<=,==) linear expressions
-- Transformed into:
-- linear expressions (>=,<=,==) rational

data Term = ConstTerm SimplexNum | CoeffTerm SimplexNum Var | VarTerm Var -- Consider VarTerm Var - note, we must consider normalizing this: Considered. It makes going to standard form easier due to type safety
  deriving (Show, Read, Eq, Ord, Generic)

instance Arbitrary Term where
  arbitrary = oneof [ ConstTerm <$> arbitrary
                    , CoeffTerm <$> arbitrary <*> arbitrary
                    , VarTerm <$> arbitrary
                    ]

  shrink = genericShrink

-- TODO: Test each function when reasonable
simplifyTerm :: Term -> Term
simplifyTerm (CoeffTerm 0 _) = ConstTerm 0
simplifyTerm (CoeffTerm 1 v) = VarTerm v
simplifyTerm t = t

negateTerm :: Term -> Term
negateTerm (ConstTerm c) = ConstTerm (-c)
negateTerm (CoeffTerm (-1) v) = VarTerm v
negateTerm (CoeffTerm c v) = CoeffTerm (-c) v
negateTerm (VarTerm v) = CoeffTerm (-1) v

-- Consider [Term]
data Expr = Expr Term | Expr :+ Term -- | Expr :+ Expr
  deriving (Show, Read, Eq, Generic)

instance Arbitrary Expr where
  arbitrary = oneof [ Expr <$> arbitrary
                    , liftA2 (:+) arbitrary arbitrary
                    ]

  shrink = genericShrink

-- | Convert an 'Expr' to a list of 'Term's.
exprToList :: Expr -> [Term]
exprToList (Expr t) = [t]
exprToList (e :+ t) = exprToList e ++ [t]

-- | Convert a list of 'Term's to an 'Expr'.
listToExpr :: [Term] -> Expr
listToExpr [] = Expr $ ConstTerm 0
listToExpr (t : ts) = foldl (:+) (Expr t) ts

-- expr: (Expr (CoeffTerm (1 % 1) 0) :+ CoeffTerm (1 % 2) (-1)) :+ CoeffTerm (2 % 1) (-1)
-- 1 = x
-- -1 = y
-- 1x + 0.5y + 2y
-- simplifiedExpr: (Expr (CoeffTerm (1 % 2) (-1)) :+ CoeffTerm (2 % 1) (-1)) :+ VarTerm 0
-- 0.5y + 2y + x
-- simplifiedTwiceExpr: Expr (CoeffTerm (5 % 2) (-1)) :+ VarTerm 0
-- 2.5y + x

-- | Normalize a list of 'Term's where each term is added together.
normalizeTerms :: [Term] -> [Term]
normalizeTerms = L.sort . map simplifyTerm . combineTerms . L.sortBy orderForCombineTerms . map (varToCoeff . simplifyTerm)
  where
    orderForCombineTerms :: Term -> Term -> Ordering
    orderForCombineTerms _ (VarTerm _) = error "Unexpected VarTerm in orderForCombineTerms"
    orderForCombineTerms (VarTerm _) _ = error "Unexpected VarTerm in orderForCombineTerms"
    orderForCombineTerms (ConstTerm c1) (ConstTerm c2) = compare c1 c2
    orderForCombineTerms (CoeffTerm c1 v1) (CoeffTerm c2 v2) =
      case compare v1 v2 of
        EQ -> compare c1 c2
        x -> x
    orderForCombineTerms (ConstTerm _) (CoeffTerm _ _) = LT
    orderForCombineTerms (CoeffTerm _ _) (ConstTerm _) = GT

    varToCoeff :: Term -> Term
    varToCoeff (VarTerm v) = CoeffTerm 1 v
    varToCoeff t = t

    combineTerms :: [Term] -> [Term]
    combineTerms [] = []
    combineTerms [ConstTerm 0] = []
    combineTerms [CoeffTerm 0 _] = []
    combineTerms [x] = [x]
    combineTerms allXs@(x1 : x2 : xs) =
      -- T.trace (show allXs) $
      case (x1, x2) of
        (ConstTerm 0, _) -> combineTerms (x2 : xs)
        (_, ConstTerm 0) -> combineTerms (x1 : xs)
        (CoeffTerm 0 _, _) -> combineTerms (x2 : xs)
        (_, CoeffTerm 0 _) -> combineTerms (x1 : xs)
        (ConstTerm c1, ConstTerm c2) -> if c1 + c2 == 0 then combineTerms xs else combineTerms (ConstTerm (c1 + c2) : xs)
        (CoeffTerm c1 v1, CoeffTerm c2 v2) ->
          if v1 == v2
            then combineTerms (CoeffTerm (c1 + c2) v1 : xs)
            else x1 : combineTerms (x2 : xs)
        _ -> x1 : combineTerms (x2 : xs)

simplifyExpr :: Expr -> Expr
simplifyExpr = listToExpr . normalizeTerms . exprToList

sumExprConstTerms :: Expr -> SimplexNum
sumExprConstTerms (Expr (ConstTerm c)) = c
sumExprConstTerms (Expr (CoeffTerm _ _)) = 0
sumExprConstTerms (Expr (VarTerm _)) = 0
sumExprConstTerms (e :+ t) = sumExprConstTerms e + sumExprConstTerms (Expr t)

zeroConstTerm :: Term -> Term
zeroConstTerm (ConstTerm _) = ConstTerm 0
zeroConstTerm t = t

zeroConstExpr :: Expr -> Expr
zeroConstExpr (Expr t) = Expr (zeroConstTerm t)
zeroConstExpr (e :+ t) = zeroConstExpr e :+ zeroConstTerm t

negateExpr :: Expr -> Expr
negateExpr = listToExpr . map negateTerm . exprToList

addExpr :: Expr -> Expr -> Expr
addExpr e1 e2 =
  -- Safe as Expr :+ Term is the only constructor
  simplifyExpr . listToExpr $ (exprToList e1 <> exprToList e2)

subtractExpr :: Expr -> Expr -> Expr
subtractExpr e1 e2 = addExpr e1 (negateExpr e2)

substVarExpr :: Var -> Expr -> Expr -> Expr
substVarExpr var varReplacement = simplifyExpr . listToExpr . aux . exprToList
  where
    replacementTerms = exprToList varReplacement

    aux :: [Term] -> [Term]
    aux [] = []
    aux (t : ts) = case t of
      (VarTerm tV) -> if tV == var then aux ts ++ replacementTerms else t : aux ts
      (CoeffTerm tC tV) ->
        if tV == var
          then
            let newReplacementTerms =
                  map
                  (
                  simplifyTerm
                  .
                  \case
                    (CoeffTerm rC rV) -> CoeffTerm (tC * rC) rV
                    (VarTerm rV) -> CoeffTerm tC rV
                    (ConstTerm rC) -> ConstTerm (tC * rC)
                  )
                  replacementTerms
            in aux ts ++ newReplacementTerms
          else t : aux ts
      (ConstTerm _) -> t : aux ts

-- substVarExpr :: Var -> Expr -> Expr -> Expr
-- substVarExpr var varReplacement = simplifyExpr . listToExpr . aux . exprToList
--   where
--     replacementTerms = exprToList varReplacement

--     aux :: [Term] -> [Term]
--     aux [] = []
--     aux (t : ts) = case t of
--       (VarTerm tV) -> if tV == var then replacementTerms ++ aux ts else t : aux ts
--       (CoeffTerm tC tV) ->
--         if tV == var
--           then 
--             let newReplacementTerms =
--                   map
--                   (
--                   simplifyTerm
--                   .
--                   \case
--                     (CoeffTerm rC rV) -> CoeffTerm (tC * rC) rV
--                     (VarTerm rV) -> CoeffTerm tC rV
--                     (ConstTerm rC) -> ConstTerm (tC * rC)
--                   )
--                   replacementTerms
--             in newReplacementTerms ++ aux ts
--           else t : aux ts
--       (ConstTerm _) -> t : aux ts

-- 3x + 5y - 2z as a Expr
-- tmpVarTerm = Expr (CoeffTerm 1 3) :+ CoeffTerm 1 5 :+ CoeffTerm 1 (-2)

-- data Expr = Var Var | Const SimplexNum | Expr :+ Expr | Expr :-: Expr | Expr :*: Expr | Expr :/: Expr
--   deriving (Show, Read, Eq, Generic)

-- consider moving to a new file if we want people to be able to change this
type SimplexNum = Rational

data GenericConstraint a b = a :<= b | a :>= b | a :== b
  deriving (Show, Read, Eq, Generic)

instance (Arbitrary a, Arbitrary b) => Arbitrary (GenericConstraint a b) where
  arbitrary = oneof [ liftA2 (:<=) arbitrary arbitrary
                    , liftA2 (:>=) arbitrary arbitrary
                    , liftA2 (:==) arbitrary arbitrary
                    ]
  shrink = genericShrink

getGenericConstraintLHS :: GenericConstraint a b -> a
getGenericConstraintLHS (a :<= _) = a
getGenericConstraintLHS (a :>= _) = a
getGenericConstraintLHS (a :== _) = a

getGenericConstraintRHS :: GenericConstraint a b -> b
getGenericConstraintRHS (_ :<= b) = b
getGenericConstraintRHS (_ :>= b) = b
getGenericConstraintRHS (_ :== b) = b

-- Input
type Constraint = GenericConstraint Expr Expr

-- data TermsOnlyVars = VarTerm' Var | CoeffTerm' SimplexNum Var
--   deriving (Show, Read, Eq, Generic)
-- data ExprVarsOnly = ExprVarsOnly [TermsOnlyVars]

-- Internal TODO: change to Terms :: [Term] or [Term]
-- consider a term type with only variables
-- then have another type SimpleConstraint = GenericConstraint TermsOnlyVars SimplexNum
type SimpleConstraint = GenericConstraint Expr SimplexNum

substVarSimpleConstraint :: Var -> Expr -> SimpleConstraint -> SimpleConstraint
substVarSimpleConstraint var varReplacement (a :<= b) = substVarExpr var varReplacement a :<= b
substVarSimpleConstraint var varReplacement (a :>= b) = substVarExpr var varReplacement a :>= b
substVarSimpleConstraint var varReplacement (a :== b) = substVarExpr var varReplacement a :== b

constraintToSimpleConstraint :: Constraint -> SimpleConstraint
constraintToSimpleConstraint constraint =
  case constraint of
    (a :<= b) -> uncurry (:<=) (calcLhsRhs a b)
    (a :>= b) -> uncurry (:>=) (calcLhsRhs a b)
    (a :== b) -> uncurry (:==) (calcLhsRhs a b)
  where
    calcLhsRhs a b = (lhs, rhs)
      where
        aConsts = sumExprConstTerms a
        bConsts = sumExprConstTerms b
        rhs = bConsts - aConsts

        aWithoutConst = simplifyExpr . zeroConstExpr $ a
        bWithoutConst = simplifyExpr . zeroConstExpr $ b

        lhs = subtractExpr aWithoutConst bWithoutConst
    calcRhs a b = rhs
      where
        aConsts = sumExprConstTerms a
        bConsts = sumExprConstTerms b
        rhs = bConsts - aConsts

        aWithoutConst = simplifyExpr . zeroConstExpr $ a
        bWithoutConst = simplifyExpr . zeroConstExpr $ b

        lhs = subtractExpr aWithoutConst bWithoutConst

-- normalize simple constraints by moving all constants to the right
normalizeSimpleConstraint :: SimpleConstraint -> SimpleConstraint
normalizeSimpleConstraint (expr :<= num) =
  let
    exprList = exprToList expr

    isConstTerm (ConstTerm _) = True
    isConstTerm _ = False

    (sumExprConstTerms, nonConstTerms) = L.partition isConstTerm exprList

    constTermsVal = sum . map (\case (ConstTerm c) -> c; _ -> 0) $ sumExprConstTerms

    newExpr = listToExpr nonConstTerms
    newNum = num - constTermsVal
  in newExpr :<= newNum
normalizeSimpleConstraint (expr :>= num) =
  let
    exprList = exprToList expr

    isConstTerm (ConstTerm _) = True
    isConstTerm _ = False

    (sumExprConstTerms, nonConstTerms) = L.partition isConstTerm exprList

    constTermsVal = sum . map (\case (ConstTerm c) -> c; _ -> 0) $ sumExprConstTerms

    newExpr = listToExpr nonConstTerms
    newNum = num - constTermsVal
  in newExpr :>= newNum
normalizeSimpleConstraint (expr :== num) =
  let
    exprList = exprToList expr

    isConstTerm (ConstTerm _) = True
    isConstTerm _ = False

    (sumExprConstTerms, nonConstTerms) = L.partition isConstTerm exprList

    constTermsVal = sum . map (\case (ConstTerm c) -> c; _ -> 0) $ sumExprConstTerms

    newExpr = listToExpr nonConstTerms
    newNum = num - constTermsVal
  in newExpr :== newNum

-- | Simplify coeff constraints by dividing the coefficient from both sides
simplifyCoeff :: SimpleConstraint -> SimpleConstraint
simplifyCoeff expr@(Expr (CoeffTerm coeff var) :<= num)
  | coeff == 0 = expr
  | coeff > 0 = Expr (VarTerm var) :<= (num / coeff)
  | coeff < 0 = Expr (VarTerm var) :>= (num / coeff)
simplifyCoeff expr@(Expr (CoeffTerm coeff var) :>= num)
  | coeff == 0 = expr
  | coeff > 0 = Expr (VarTerm var) :>= (num / coeff)
  | coeff < 0 = Expr (VarTerm var) :<= (num / coeff)
simplifyCoeff expr@(Expr (CoeffTerm coeff var) :== num) = if coeff == 0 then expr else Expr (VarTerm var) :== (num / coeff)
simplifyCoeff expr = expr

simplifySimpleConstraint :: SimpleConstraint -> SimpleConstraint
simplifySimpleConstraint (expr :<= num) = simplifyCoeff . normalizeSimpleConstraint $ simplifyExpr expr :<= num
simplifySimpleConstraint (expr :>= num) = simplifyCoeff . normalizeSimpleConstraint $ simplifyExpr expr :>= num
simplifySimpleConstraint (expr :== num) = simplifyCoeff . normalizeSimpleConstraint $ simplifyExpr expr :== num

type SimpleSystem = [SimpleConstraint]

simplifySimpleSystem :: SimpleSystem -> SimpleSystem
simplifySimpleSystem = map simplifySimpleConstraint

data StandardFormRow = StandardFormRow
  { lhs :: Expr
  , rhs :: SimplexNum
  }
  deriving (Show, Read, Eq, Generic)

-- | the system with slack variables
type StandardForm = [StandardFormRow]

data Bounds = Bounds
  { lowerBound :: Maybe SimplexNum
  , upperBound :: Maybe SimplexNum
  }
  deriving (Show, Read, Eq, Generic)

type Var = Int

type VarBounds = M.Map Var Bounds

-- | Merge two bounds, very simple
deriveBounds :: SimpleSystem -> VarBounds
deriveBounds = foldr updateBounds M.empty
  where
    updateBounds :: SimpleConstraint -> VarBounds -> VarBounds
    updateBounds (Expr (VarTerm var) :<= num) = M.insertWith mergeBounds var (Bounds Nothing (Just num))
    updateBounds (Expr (VarTerm var) :>= num) = M.insertWith mergeBounds var (Bounds (Just num) Nothing)
    updateBounds (Expr (VarTerm var) :== num) = M.insertWith mergeBounds var (Bounds (Just num) (Just num))
    updateBounds _ = id

    mergeBounds :: Bounds -> Bounds -> Bounds
    mergeBounds (Bounds l1 u1) (Bounds l2 u2) = Bounds (mergeLower l1 l2) (mergeUpper u1 u2)
      where
        mergeLower Nothing b = b
        mergeLower a Nothing = a
        mergeLower (Just a) (Just b) = Just (max a b)

        mergeUpper Nothing b = b
        mergeUpper a Nothing = a
        mergeUpper (Just a) (Just b) = Just (min a b)

validateBounds :: VarBounds -> Bool
validateBounds boundsMap = all soundBounds $ M.toList boundsMap
  where
    soundBounds (_, Bounds lowerBound upperBound) =
      case (lowerBound, upperBound) of
        (Just l, Just u) -> l <= u
        (_, _) -> True

-- Eliminate inequalities which are outside the bounds
-- precondition: no zero coefficients
-- TODO: better name
removeUselessSystemBounds :: SimpleSystem -> VarBounds -> SimpleSystem
removeUselessSystemBounds constraints bounds =
  filter
  ( \case
    (Expr (VarTerm var) :<= num) -> case M.lookup var bounds of
      Just (Bounds _ (Just upper)) -> num <= upper
      _ -> True
    (Expr (VarTerm var) :>= num) -> case M.lookup var bounds of
      Just (Bounds (Just lower) _) -> num >= lower
      _ -> True
    _ -> True
  )
  constraints

findHighestVar :: SimpleSystem -> Var
findHighestVar = maximum . map (maximum . map (\case (CoeffTerm _ v) -> v; _ -> 0) . exprToList . getGenericConstraintLHS)

-- | Eliminate negative lower bounds via substitution
-- Return the system with the eliminated variables and a map of the eliminated variables to their equivalent expressions
-- First step here https://en.wikipedia.org/wiki/Simplex_algorithm#Standard_form
eliminateNonZeroLowerBounds :: SimpleSystem -> M.Map Var Expr -> (M.Map Var Expr, SimpleSystem)
eliminateNonZeroLowerBounds constraints eliminatedVarsMap = aux constraints
  where
    removeConstraint :: SimpleConstraint -> SimpleSystem -> SimpleSystem
    removeConstraint c = filter (/= c)

    aux :: SimpleSystem -> (M.Map Var Expr, SimpleSystem)
    aux [] = (eliminatedVarsMap, constraints)
    aux (c : cs) = case c of
      -- x >= 5
      (Expr (VarTerm var) :>= lowerBound) ->
        if lowerBound == 0
          then aux cs
          else
            let newVar = findHighestVar constraints + 1
                -- y >= 0
                newVarLowerBound = Expr (VarTerm newVar) :>= 0

                -- x = y + 5
                substOldVarWith = Expr (VarTerm newVar) :+ ConstTerm lowerBound

                newConstraints = simplifySimpleSystem $ newVarLowerBound : map (substVarSimpleConstraint var substOldVarWith) constraints
                updatedEliminatedVarsMap = M.insert var substOldVarWith eliminatedVarsMap
            in eliminateNonZeroLowerBounds newConstraints updatedEliminatedVarsMap -- TODO: Make more efficient if needed
      _ -> aux cs

-- Add slack variables...
-- Second step here https://en.wikipedia.org/wiki/Simplex_algorithm#Standard_form
-- Return system of equalities and the slack variables
addSlackVariables :: SimpleSystem -> ([Var], SimpleSystem)
addSlackVariables constraints = aux constraints newVar []
  where
    newVar = findHighestVar constraints + 1

    aux :: SimpleSystem -> Var -> [Var] -> ([Var], SimpleSystem)
    aux [] _ slackVars = (slackVars, [])
    aux (c : cs) nextVar slackVars = case c of
      (expr :<= num) ->
        let slackVar = newVar
            newNextVar = nextVar + 1
            newExpr = expr :+ VarTerm slackVar
            slackVarLowerBound = Expr (VarTerm slackVar) :>= 0
            (newSlackVars, newConstraints) = aux cs newNextVar slackVars
        in (newVar : newSlackVars, newExpr :== num : slackVarLowerBound : newConstraints)
      (expr :>= num) ->
        let slackVar = newVar
            newNextVar = nextVar + 1
            newExpr = expr :+ CoeffTerm (-1) slackVar
            slackVarLowerBound = Expr (VarTerm slackVar) :>= 0
            (newSlackVars, newConstraints) = aux cs newNextVar slackVars
        in (newVar : newSlackVars, newExpr :== num : slackVarLowerBound : newConstraints)
      (expr :== num) ->
        let (newSlackVars, newConstraints) = aux cs nextVar slackVars
        in (newSlackVars, c : newConstraints)

-- Eliminate unrestricted variables (lower bound unknown)
-- Third step here https://en.wikipedia.org/wiki/Simplex_algorithm#Standard_form
-- precondition: VarBounds accurate for SimpleSystem
eliminateUnrestrictedLowerBounds :: SimpleSystem -> VarBounds -> M.Map Var Expr -> (M.Map Var Expr, SimpleSystem)
eliminateUnrestrictedLowerBounds constraints varBoundMap eliminatedVarsMap = aux constraints (M.toList varBoundMap)
  where
    aux :: SimpleSystem -> [(Var, Bounds)] -> (M.Map Var Expr, SimpleSystem)
    aux _ [] = (eliminatedVarsMap, constraints)
    aux cs ((var, Bounds Nothing _): bounds) =
      let newVarPlus = findHighestVar constraints + 1
          newVarMinus = newVarPlus + 1
          newVarPlusLowerBound = Expr (VarTerm newVarPlus) :>= 0
          newVarMinusLowerBound = Expr (VarTerm newVarMinus) :>= 0

          -- oldVar = newVarPlus - newVarMinus
          substOldVarWith = Expr (VarTerm newVarPlus) :+ CoeffTerm (-1) newVarMinus

          newConstraints = simplifySimpleSystem $ newVarPlusLowerBound : newVarMinusLowerBound : map (substVarSimpleConstraint var substOldVarWith) constraints
          -- TODO: Update this name
          updatedEliminatedVarsMap = M.insert var substOldVarWith eliminatedVarsMap
      in eliminateUnrestrictedLowerBounds newConstraints (M.fromList bounds) updatedEliminatedVarsMap

-- data Term = ConstTerm SimplexNum | CoeffTerm SimplexNum Var -- Consider VarTermnewVar
data StandardTerm = StandardTerm SimplexNum Var
  deriving (Show, Read, Eq, Generic)

-- Equality
data StandardConstraint = StandardConstraint [StandardTerm] SimplexNum
  deriving (Show, Read, Eq, Generic)

type StandardSystem = [StandardConstraint]

-- Negative/no lower bounds
-- say x
-- x = x1 - x2
-- x1, x2 >= 0

-- A 'Tableau' where the basic variable may be empty.
-- All non-empty basic vars are slack vars
data SystemWithSlackVarRow = SystemInStandardFormRow
  { mSlackVar :: Maybe Var
  -- ^ This is Nothing iff the row does not have a slack variable
  , row :: TableauRow
  }

type SystemWithSlackVars = [SystemWithSlackVarRow]

data FeasibleSystem = FeasibleSystem
  { dict :: Dict
  , slackVars :: [Var]
  , artificialVars :: [Var]
  , objectiveVar :: Var
  }
  deriving (Show, Read, Eq, Generic)

data Result = Result
  { objectiveVar :: Var
  , varValMap :: VarLitMap
  -- TODO:
  -- Maybe VarLitMap
  -- , feasible :: Bool
  -- , optimisable :: Bool
  }
  deriving (Show, Read, Eq, Generic)

data SimplexMeta = SimplexMeta
  { objective :: ObjectiveFunction
  , feasibleSystem :: Maybe FeasibleSystem
  , optimisedResult :: Maybe Result
  }

type VarLitMap = M.Map Var SimplexNum

-- | List of variables with their 'SimplexNum' coefficients.
--   There is an implicit addition between elements in this list.
--
--   Example: [Var "x" 3, Var "y" -1, Var "z" 1] is equivalent to 3x + (-y) + z.
type VarLitMapSum = VarLitMap

-- | For specifying constraints in a system.
--   The LHS is a 'VarLitMapSum', and the RHS, is a 'SimplexNum' number.
--   LEQ [(1, 2), (2, 1)] 3.5 is equivalent to 2x1 + x2 <= 3.5.
--   Users must only provide positive integer variables.
--
--   Example: LEQ [Var "x" 3, Var "y" -1, Var "x" 1] 12.3 is equivalent to 3x + (-y) + x <= 12.3.

-- data StandardConstraint
--   = LEQ {lhs :: VarLitMapSum, rhs :: SimplexNum}
--   | GEQ {lhs :: VarLitMapSum, rhs :: SimplexNum}
--   | EQ {lhs :: VarLitMapSum, rhs :: SimplexNum}
--   deriving (Show, Read, Eq, Generic)

-- | Create an objective function.
--   We can either 'Max'imize or 'Min'imize a 'VarTermSum'.
data ObjectiveFunction = Max {objective :: VarLitMapSum} | Min {objective :: VarLitMapSum}
  deriving (Show, Read, Eq, Generic)

-- | TODO: Maybe we want this type
-- TODO: A better/alternative name
data Equation = Equation
  { lhs :: VarLitMapSum
  , rhs :: SimplexNum
  }

-- | Value for 'Tableau'. lhs = rhs.
data TableauRow = TableauRow
  { lhs :: VarLitMapSum
  , rhs :: SimplexNum
  }
  deriving (Show, Read, Eq, Generic)

-- | A simplex 'Tableu' of equations.
--   Each entry in the map is a row.
type Tableau = M.Map Var TableauRow

-- | Values for a 'DictEntry'.
data DictValue = DictValue
  { varMapSum :: VarLitMapSum
  , constant :: SimplexNum
  }
  deriving (Show, Read, Eq, Generic)

-- | A simplex 'Dict'
--   One quation represents the objective function.
--   Each pair in the list is one equation in the system we're working with.
-- data Dict = Dict
--   { objective :: DictObjective
--   , entries :: DictEntries
--   }
--   deriving (Show, Read, Eq, Generic)
type Dict = M.Map Var DictValue

data PivotObjective = PivotObjective
  { variable :: Var
  , function :: VarLitMapSum
  , constant :: SimplexNum
  }
  deriving (Show, Read, Eq, Generic)
