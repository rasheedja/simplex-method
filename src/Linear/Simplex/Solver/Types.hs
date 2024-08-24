module Linear.Simplex.Solver.Types where

import qualified Data.Map as Map
import GHC.Generics (Generic)
import Linear.Expr.Types (Expr)
import Linear.System.Linear.Types (CanBeLinearSystem)
import Linear.Var.Types (SimplexNum, Var)

data OptimisationDirection = Minimize | Maximize
  deriving (Show, Eq, GHC.Generics.Generic)

data Objective = Objective
  { expr :: Linear.Expr.Types.Expr -- TODO: this should be ExprVarsOnly
  , direction :: OptimisationDirection
  }
  deriving (Show, Eq, GHC.Generics.Generic)

data Result = Result
  { varMap :: Map.Map Var SimplexNum
  , objVal :: SimplexNum
  }
  deriving (Show, Read, Eq, GHC.Generics.Generic)

class (CanBeLinearSystem s) => Solver s where
  solve :: s -> Objective -> Result
