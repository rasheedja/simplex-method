module Linear.Var.Types where

import qualified Data.Map as M
import GHC.Generics (Generic)

-- TODO: Consider other names: SimplexCoeff, CoeffType
type SimplexNum = Rational

-- TODO: newtype
type Var = Int

data Bounds = Bounds
  { lowerBound :: Maybe SimplexNum
  , upperBound :: Maybe SimplexNum
  }
  deriving (Show, Read, Eq, Generic)

-- newtype VarBounds = VarBounds { unVarBounds :: M.Map Var Bounds }
--  deriving (Show, Read, Eq, Generic)

type VarBounds = M.Map Var Bounds
