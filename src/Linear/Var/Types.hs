module Linear.Var.Types where

import qualified Data.Map as M
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary)

type SimplexNum = Rational

newtype Var = Var {unVar :: Int}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving newtype (Arbitrary)
