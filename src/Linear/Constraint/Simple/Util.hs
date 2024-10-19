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
import Linear.Constraint.Simple.Types (SimpleConstraint (..))
import Linear.Constraint.Types (Constraint (..))
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
substVarSimpleConstraintExpr var varReplacement (SimpleConstraint (a :<= b)) =
  let newExpr = substVarExpr var varReplacement (exprVarsOnlyToExpr a)
      newConstraint = newExpr :<= Expr [ConstTerm b]
  in  constraintToSimpleConstraint $ Constraint newConstraint
substVarSimpleConstraintExpr var varReplacement (SimpleConstraint (a :>= b)) =
  let newExpr = substVarExpr var varReplacement (exprVarsOnlyToExpr a)
      newConstraint = newExpr :>= Expr [ConstTerm b]
  in  constraintToSimpleConstraint $ Constraint newConstraint
substVarSimpleConstraintExpr var varReplacement (SimpleConstraint (a :== b)) =
  let newExpr = substVarExpr var varReplacement (exprVarsOnlyToExpr a)
      newConstraint = newExpr :== Expr [ConstTerm b]
  in  constraintToSimpleConstraint $ Constraint newConstraint

substVarSimpleConstraint ::
  Var -> ExprVarsOnly -> SimpleConstraint -> SimpleConstraint
substVarSimpleConstraint var varReplacement (SimpleConstraint (a :<= b)) = SimpleConstraint $ substVarExprVarsOnly var varReplacement a :<= b
substVarSimpleConstraint var varReplacement (SimpleConstraint (a :>= b)) = SimpleConstraint $ substVarExprVarsOnly var varReplacement a :>= b
substVarSimpleConstraint var varReplacement (SimpleConstraint (a :== b)) = SimpleConstraint $ substVarExprVarsOnly var varReplacement a :== b

constraintToSimpleConstraint :: Constraint -> SimpleConstraint
constraintToSimpleConstraint constraint =
  case constraint of
    Constraint (a :<= b) -> SimpleConstraint $ uncurry (:<=) (calcLhsRhs a b)
    Constraint (a :>= b) -> SimpleConstraint $ uncurry (:>=) (calcLhsRhs a b)
    Constraint (a :== b) -> SimpleConstraint $ uncurry (:==) (calcLhsRhs a b)
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

-- | Simplify coeff constraints by dividing the coefficient from both sides
simplifyCoeff :: SimpleConstraint -> SimpleConstraint
simplifyCoeff simpleConstraint@(SimpleConstraint (ExprVarsOnly [CoeffTermVO coeff var] :<= num))
  | coeff == 0 = simpleConstraint
  | coeff > 0 = SimpleConstraint $ ExprVarsOnly [VarTermVO var] :<= (num / coeff)
  | coeff < 0 = SimpleConstraint $ ExprVarsOnly [VarTermVO var] :>= (num / coeff)
simplifyCoeff simpleConstraint@(SimpleConstraint (ExprVarsOnly [CoeffTermVO coeff var] :>= num))
  | coeff == 0 = simpleConstraint
  | coeff > 0 = SimpleConstraint $ ExprVarsOnly [VarTermVO var] :>= (num / coeff)
  | coeff < 0 = SimpleConstraint $ ExprVarsOnly [VarTermVO var] :<= (num / coeff)
simplifyCoeff simpleConstraint@(SimpleConstraint (ExprVarsOnly [CoeffTermVO coeff var] :== num)) =
  if coeff == 0
    then simpleConstraint
    else SimpleConstraint $ ExprVarsOnly [VarTermVO var] :== (num / coeff)
simplifyCoeff simpleConstraint = simpleConstraint

simplifySimpleConstraint :: SimpleConstraint -> SimpleConstraint
simplifySimpleConstraint (SimpleConstraint (expr :<= num)) = simplifyCoeff . SimpleConstraint $ simplifyExprVarsOnly expr :<= num
simplifySimpleConstraint (SimpleConstraint (expr :>= num)) = simplifyCoeff . SimpleConstraint $ simplifyExprVarsOnly expr :>= num
simplifySimpleConstraint (SimpleConstraint (expr :== num)) = simplifyCoeff . SimpleConstraint $ simplifyExprVarsOnly expr :== num

simpleConstraintVars :: SimpleConstraint -> Set.Set Var
simpleConstraintVars (SimpleConstraint (expr :<= _)) = exprVars . exprVarsOnlyToExpr $ expr
simpleConstraintVars (SimpleConstraint (expr :>= _)) = exprVars . exprVarsOnlyToExpr $ expr
simpleConstraintVars (SimpleConstraint (expr :== _)) = exprVars . exprVarsOnlyToExpr $ expr
