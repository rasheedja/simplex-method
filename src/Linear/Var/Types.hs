module Linear.Var.Types where

import qualified Data.Map as M
import GHC.Generics (Generic)

type SimplexNum = Rational

type Var = Int

data Bounds = Bounds
  { lowerBound :: Maybe SimplexNum
  , upperBound :: Maybe SimplexNum
  }
  deriving (Show, Read, Eq, Generic)

type VarBounds = M.Map Var Bounds
