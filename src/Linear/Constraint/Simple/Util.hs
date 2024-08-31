-- |
-- Module: Linear.Constraint.Simple.Util
-- Description: Utility functions for simple constraints
-- Copyright: (c) Junaid Rasheed, 2020-2024
-- License: BSD-3
-- Maintainer: jrasheed178@gmail.com
-- Stability: experimental
module Linear.Constraint.Simple.Util where

import qualified Data.List as L
import qualified Data.Set as Set
import Linear.Constraint.Generic.Types
  ( GenericConstraint (..)
  )
import Linear.Constraint.Simple.Types (SimpleConstraint)
import Linear.Constraint.Types (Constraint)
import Linear.Expr.Types (Expr (..), ExprVarsOnly (..))
import Linear.Expr.Util
  ( exprToExprVarsOnly
  , exprToList
  , exprVars
  , exprVarsOnlyToExpr
  , listToExpr
  , simplifyExpr
  , simplifyExprVarsOnly
  , substVarExpr
  , substVarExprVarsOnly
  , subtractExpr
  , sumExprConstTerms
  , zeroConstExpr
  )
import Linear.Term.Types (Term (..), TermVarsOnly (..))
import Linear.Var.Types (Var)

substVarSimpleConstraintExpr ::
  Var -> Expr -> SimpleConstraint -> SimpleConstraint
substVarSimpleConstraintExpr var varReplacement (a :<= b) =
  let newExpr = substVarExpr var varReplacement (exprVarsOnlyToExpr a)
      newConstraint = newExpr :<= Expr [ConstTerm b]
  in  constraintToSimpleConstraint newConstraint
substVarSimpleConstraintExpr var varReplacement (a :>= b) =
  let newExpr = substVarExpr var varReplacement (exprVarsOnlyToExpr a)
      newConstraint = newExpr :>= Expr [ConstTerm b]
  in  constraintToSimpleConstraint newConstraint
substVarSimpleConstraintExpr var varReplacement (a :== b) =
  let newExpr = substVarExpr var varReplacement (exprVarsOnlyToExpr a)
      newConstraint = newExpr :== Expr [ConstTerm b]
  in  constraintToSimpleConstraint newConstraint

substVarSimpleConstraint ::
  Var -> ExprVarsOnly -> SimpleConstraint -> SimpleConstraint
substVarSimpleConstraint var varReplacement (a :<= b) = substVarExprVarsOnly var varReplacement a :<= b
substVarSimpleConstraint var varReplacement (a :>= b) = substVarExprVarsOnly var varReplacement a :>= b
substVarSimpleConstraint var varReplacement (a :== b) = substVarExprVarsOnly var varReplacement a :== b

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

        lhs' = subtractExpr aWithoutConst bWithoutConst
        lhs = case exprToExprVarsOnly lhs' of
          Right exprVarsOnly -> exprVarsOnly
          Left err ->
            error $
              "constraintToSimpleConstraint: lhs is not ExprVarsOnly. Details: " <> err

-- normalize simple constraints by moving all constants to the right
-- normalizeSimpleConstraint :: SimpleConstraint -> SimpleConstraint
-- normalizeSimpleConstraint (expr :<= num) =
--   let exprList = exprToList expr

--       isConstTerm (ConstTerm _) = True
--       isConstTerm _ = False

--       (sumExprConstTerms, nonConstTerms) = L.partition isConstTerm exprList

--       constTermsVal = sum . map (\case (ConstTerm c) -> c; _ -> 0) $ sumExprConstTerms

--       newExpr = listToExpr nonConstTerms
--       newNum = num - constTermsVal
--   in  newExpr :<= newNum
-- normalizeSimpleConstraint (expr :>= num) =
--   let exprList = exprToList expr

--       isConstTerm (ConstTerm _) = True
--       isConstTerm _ = False

--       (sumExprConstTerms, nonConstTerms) = L.partition isConstTerm exprList

--       constTermsVal = sum . map (\case (ConstTerm c) -> c; _ -> 0) $ sumExprConstTerms

--       newExpr = listToExpr nonConstTerms
--       newNum = num - constTermsVal
--   in  newExpr :>= newNum
-- normalizeSimpleConstraint (expr :== num) =
--   let exprList = exprToList expr

--       isConstTerm (ConstTerm _) = True
--       isConstTerm _ = False

--       (sumExprConstTerms, nonConstTerms) = L.partition isConstTerm exprList

--       constTermsVal = sum . map (\case (ConstTerm c) -> c; _ -> 0) $ sumExprConstTerms

--       newExpr = listToExpr nonConstTerms
--       newNum = num - constTermsVal
--   in  newExpr :== newNum

-- | Simplify coeff constraints by dividing the coefficient from both sides
simplifyCoeff :: SimpleConstraint -> SimpleConstraint
simplifyCoeff expr@(ExprVarsOnly [CoeffTermVO coeff var] :<= num)
  | coeff == 0 = expr
  | coeff > 0 = ExprVarsOnly [VarTermVO var] :<= (num / coeff)
  | coeff < 0 = ExprVarsOnly [VarTermVO var] :>= (num / coeff)
simplifyCoeff expr@(ExprVarsOnly [CoeffTermVO coeff var] :>= num)
  | coeff == 0 = expr
  | coeff > 0 = ExprVarsOnly [VarTermVO var] :>= (num / coeff)
  | coeff < 0 = ExprVarsOnly [VarTermVO var] :<= (num / coeff)
simplifyCoeff expr@(ExprVarsOnly [CoeffTermVO coeff var] :== num) =
  if coeff == 0
    then expr
    else ExprVarsOnly [VarTermVO var] :== (num / coeff)
simplifyCoeff expr = expr

simplifySimpleConstraint :: SimpleConstraint -> SimpleConstraint
simplifySimpleConstraint (expr :<= num) = simplifyCoeff $ simplifyExprVarsOnly expr :<= num
simplifySimpleConstraint (expr :>= num) = simplifyCoeff $ simplifyExprVarsOnly expr :>= num
simplifySimpleConstraint (expr :== num) = simplifyCoeff $ simplifyExprVarsOnly expr :== num

simpleConstraintVars :: SimpleConstraint -> Set.Set Var
simpleConstraintVars (expr :<= _) = exprVars . exprVarsOnlyToExpr $ expr
simpleConstraintVars (expr :>= _) = exprVars . exprVarsOnlyToExpr $ expr
simpleConstraintVars (expr :== _) = exprVars . exprVarsOnlyToExpr $ expr
