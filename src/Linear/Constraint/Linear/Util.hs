-- |
-- Module: Linear.Constraint.Linear.Util
-- Description: Utility functions for linear constraints
-- Copyright: (c) Junaid Rasheed, 2024
-- License: BSD-3
-- Maintainer: Junaid Rasheed <jrasheed178@gmail.com>
-- Stability: experimental
module Linear.Constraint.Linear.Util where

import qualified Data.Set as Set
import Linear.Constraint.Linear.Types (LinearEquation (..))
import Linear.Expr.Types (ExprVarsOnly)
import Linear.Expr.Util
  ( exprVarsOnlyMaxVar
  , exprVarsOnlyVars
  , substVarExprVarsOnly
  )
import Linear.Var.Types (Var)

-- | Get the variables in a linear equation
linearEquationVars :: LinearEquation -> Set.Set Var
linearEquationVars (LinearEquation lhs _) = exprVarsOnlyVars lhs

findHighestVar :: LinearEquation -> Var
findHighestVar (LinearEquation lhs _) = exprVarsOnlyMaxVar lhs

substVarWith ::
  Var -> ExprVarsOnly -> LinearEquation -> LinearEquation
substVarWith var expr (LinearEquation lhs rhs) = LinearEquation (substVarExprVarsOnly var expr lhs) rhs
