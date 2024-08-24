-- |
-- Module: Linear.Simplex.SlackForm.Types
-- Description: Types for augmented (slack) form of linear programming problems
-- Copyright: (c) Junaid Rasheed, 2024
-- License: BSD-3
-- Maintainer: Junaid Rasheed <jrasheed178@gmail.com>
-- Stability: experimental
module Linear.SlackForm.Types where

import GHC.Generics (Generic)
import Linear.Expr.Types (Expr)
import Linear.System.Linear.Types (LinearSystem)
import Linear.Var.Types (SimplexNum, Var)

-- Expr == SimplexNum
data SlackForm = SlackForm
  { maxObjective :: Expr -- TODO: should be ExprVarsOnly
  , constraints :: LinearSystem
  , vars :: [Var] -- all vars are non-negative
  }
  deriving (Show, Eq, Read, Generic)

class CanBeSlackForm a where
  toSlackForm :: a -> SlackForm
