-- |
-- Description: Types for linear constraints.
-- Copyright:   (c) Junaid Rasheed, 2024
-- License:     BSD-3
-- Maintainer:  Junaid Rasheed <jrasheed178@gmail.com>
-- Stability:   experimental
module Linear.Constraint.Linear.Types where

import GHC.Generics (Generic)
import Linear.Expr.Types (ExprVarsOnly)
import Linear.Var.Types (SimplexNum)

-- lhs == rhs
data LinearEquation = LinearEquation
  { lhs :: ExprVarsOnly
  , rhs :: SimplexNum
  }
  deriving (Show, Eq, Read, Generic)
