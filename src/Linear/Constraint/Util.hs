-- |
-- Module:      Linear.Constraint.Util
-- Description: Utility functions for constraints
-- Copyright:   (c) Junaid Rasheed, 2020-2024
-- License:     BSD-3
-- Maintainer:  jrasheed178@gmail.com
-- Stability:   experimental
module Linear.Constraint.Util where

import qualified Data.Set as Set
import Linear.Constraint.Generic.Types
  ( GenericConstraint ((:<=), (:==), (:>=))
  )
import Linear.Constraint.Types (Constraint (..))
import Linear.Expr.Util (exprVars)
import Linear.Var.Types (Var)

constraintVars :: Constraint -> Set.Set Var
constraintVars (Constraint (lhs :<= rhs)) = exprVars lhs <> exprVars rhs
constraintVars (Constraint (lhs :>= rhs)) = exprVars lhs <> exprVars rhs
constraintVars (Constraint (lhs :== rhs)) = exprVars lhs <> exprVars rhs
