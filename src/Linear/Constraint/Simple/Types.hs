-- |
-- Module:      Linear.Constraint.Simple.Types
-- Description: Types for simple linear constraints
-- Copyright:   (c) Junaid Rasheed, 2020-2024
-- License:     BSD-3
-- Maintainer:  jrasheed178@gmail.com
-- Stability:   experimental
module Linear.Constraint.Simple.Types where

import Linear.Constraint.Generic.Types (GenericConstraint)
import Linear.Expr.Types (ExprVarsOnly)
import Linear.Var.Types (SimplexNum)

type SimpleConstraint = GenericConstraint ExprVarsOnly SimplexNum
