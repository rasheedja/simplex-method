module Linear.Simplex.Standardize where

import Control.Lens
import Data.Generics.Labels ()
import Data.List (sort)
import qualified Data.Map as M
import GHC.Generics (Generic)

import Linear.Simplex.Types

-- Add slack vars, need type of system with only equalities

-- Add artificial vars, can we type check this somehow? Maybe with a phantom type? Is Tableau enough?
