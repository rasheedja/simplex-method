-- |
-- Module:      Linear.Constraint.Types
-- Description: Types for linear constraints
-- Copyright:   (c) Junaid Rasheed, 2020-2024
-- License:     BSD-3
-- Maintainer:  jrasheed178@gmail.com
-- Stability:   experimental
module Linear.Constraint.Types where

import qualified Data.Set as Set
import GHC.Generics (Generic)
import Linear.Constraint.Generic.Types (GenericConstraint)
import Linear.Expr.Types (Expr)

-- Input
type Constraint = GenericConstraint Expr Expr
