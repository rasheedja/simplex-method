-- |
-- Module: Linear.Simplex.SlackForm.Types
-- Description: Types for augmented (slack) form of linear programming problems
-- Copyright: (c) Junaid Rasheed, 2024
-- License: BSD-3
-- Maintainer: Junaid Rasheed <jrasheed178@gmail.com>
-- Stability: experimental
module Linear.SlackForm.Types where

import qualified Data.Set as Set
import GHC.Generics (Generic)
import Linear.Constraint.Linear.Types (LinearEquation (..))
import Linear.Expr.Types (ExprVarsOnly)
import Linear.Expr.Util (exprVarsOnlyVars)
import Linear.System.Linear.Types (LinearSystem (..))
import Linear.System.Simple.Types
import Linear.Var.Types (SimplexNum, Var)

-- Expr == SimplexNum
-- TODO: think about a better name for this type, CanonicalForm?
data SlackForm = SlackForm
  { maxObjective :: ExprVarsOnly
  , constraints :: LinearSystem
  , vars :: Set.Set Var -- all vars are non-negative
  }
  deriving (Show, Eq, Read, Generic)

class CanBeSlackForm a where
  toSlackForm :: a -> ExprVarsOnly -> SlackForm

instance CanBeSlackForm LinearSystem where
  toSlackForm ls obj =
    SlackForm
      obj
      ls
      (Set.unions $ map (exprVarsOnlyVars . lhs) ls.unLinearSystem)
