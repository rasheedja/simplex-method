-- |
-- Module: Linear.Constraint.Simple.Util
-- Description: Utility functions for simple constraints
-- Copyright: (c) Junaid Rasheed, 2020-2024
-- License: BSD-3
-- Maintainer: jrasheed178@gmail.com
-- Stability: experimental
module Linear.Constraint.Simple.Util where

import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Linear.Constraint.Generic.Types
  ( GenericConstraint ((:<=), (:==), (:>=))
  )
import Linear.Constraint.Simple.Types (SimpleConstraint)
import Linear.Constraint.Types (Constraint)
import Linear.Expr.Types (Expr (Expr))
import Linear.Expr.Util
  ( exprToList
  , exprVars
  , listToExpr
  , simplifyExpr
  , substVarExpr
  , subtractExpr
  , sumExprConstTerms
  , zeroConstExpr
  )
import Linear.Term.Types (Term (CoeffTerm, ConstTerm, VarTerm))
import Linear.Var.Types (Var)

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
  let exprList = exprToList expr

      isConstTerm (ConstTerm _) = True
      isConstTerm _ = False

      (sumExprConstTerms, nonConstTerms) = L.partition isConstTerm exprList

      constTermsVal = sum . map (\case (ConstTerm c) -> c; _ -> 0) $ sumExprConstTerms

      newExpr = listToExpr nonConstTerms
      newNum = num - constTermsVal
  in  newExpr :<= newNum
normalizeSimpleConstraint (expr :>= num) =
  let exprList = exprToList expr

      isConstTerm (ConstTerm _) = True
      isConstTerm _ = False

      (sumExprConstTerms, nonConstTerms) = L.partition isConstTerm exprList

      constTermsVal = sum . map (\case (ConstTerm c) -> c; _ -> 0) $ sumExprConstTerms

      newExpr = listToExpr nonConstTerms
      newNum = num - constTermsVal
  in  newExpr :>= newNum
normalizeSimpleConstraint (expr :== num) =
  let exprList = exprToList expr

      isConstTerm (ConstTerm _) = True
      isConstTerm _ = False

      (sumExprConstTerms, nonConstTerms) = L.partition isConstTerm exprList

      constTermsVal = sum . map (\case (ConstTerm c) -> c; _ -> 0) $ sumExprConstTerms

      newExpr = listToExpr nonConstTerms
      newNum = num - constTermsVal
  in  newExpr :== newNum

-- | Simplify coeff constraints by dividing the coefficient from both sides
simplifyCoeff :: SimpleConstraint -> SimpleConstraint
simplifyCoeff expr@(Expr (CoeffTerm coeff var :| []) :<= num)
  | coeff == 0 = expr
  | coeff > 0 = Expr (VarTerm var :| []) :<= (num / coeff)
  | coeff < 0 = Expr (VarTerm var :| []) :>= (num / coeff)
simplifyCoeff expr@(Expr (CoeffTerm coeff var :| []) :>= num)
  | coeff == 0 = expr
  | coeff > 0 = Expr (VarTerm var :| []) :>= (num / coeff)
  | coeff < 0 = Expr (VarTerm var :| []) :<= (num / coeff)
simplifyCoeff expr@(Expr (CoeffTerm coeff var :| []) :== num) = if coeff == 0 then expr else Expr (VarTerm var :| []) :== (num / coeff)
simplifyCoeff expr = expr

simplifySimpleConstraint :: SimpleConstraint -> SimpleConstraint
simplifySimpleConstraint (expr :<= num) = simplifyCoeff . normalizeSimpleConstraint $ simplifyExpr expr :<= num
simplifySimpleConstraint (expr :>= num) = simplifyCoeff . normalizeSimpleConstraint $ simplifyExpr expr :>= num
simplifySimpleConstraint (expr :== num) = simplifyCoeff . normalizeSimpleConstraint $ simplifyExpr expr :== num

simpleConstraintVars :: SimpleConstraint -> Set.Set Var
simpleConstraintVars (expr :<= _) = exprVars expr
simpleConstraintVars (expr :>= _) = exprVars expr
simpleConstraintVars (expr :== _) = exprVars expr
