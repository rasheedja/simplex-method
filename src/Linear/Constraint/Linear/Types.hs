-- |
-- Description: Types for linear constraints.
-- Copyright:   (c) Junaid Rasheed, 2024
-- License:     BSD-3
-- Maintainer:  Junaid Rasheed <jrasheed178@gmail.com>
-- Stability:   experimental
module Linear.Constraint.Linear.Types where

import GHC.Generics (Generic)
import Linear.Expr.Types (Expr)
import Linear.Var.Types (SimplexNum)

-- TODO: Expr -> ExprVarsOnly
-- lhs == rhs
data LinearEquation = LinearEquation
  { lhs :: Expr
  , rhs :: SimplexNum
  }
  deriving (Show, Eq, Read, Generic)

-- class CanBeLinearEquation a where
--   toLinearEquation :: a -> LinearEquation
--   fromLinearEquation :: LinearEquation -> a

-- instance CanBeLinearEquation LinearEquation where
--   toLinearEquation = id
--   fromLinearEquation = id
