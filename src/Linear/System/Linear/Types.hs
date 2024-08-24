-- |
-- Module: Linear.System.Linear.Types
-- Description: Types for linear programming problems
-- Copyright: (c) Junaid Rasheed, 2024
-- License: BSD-3
-- Maintainer: Junaid Rasheed <jrasheed178@gmail.com>
-- Stability: experimental
module Linear.System.Linear.Types where

import GHC.Generics (Generic)
import Linear.Constraint.Linear.Types (LinearEquation)
import Linear.Expr.Types (Expr)

newtype LinearSystem = LinearSystem {unLinearSystem :: [LinearEquation]}
  deriving (Show, Eq, Read, Generic)

class CanBeLinearSystem a where
  toLinearSystem :: a -> LinearSystem

instance CanBeLinearSystem LinearSystem where
  toLinearSystem = id

instance CanBeLinearSystem LinearEquation where
  toLinearSystem id = LinearSystem [id]
