module Linear.Simplex.Standardize where

import Data.List (sort)
import qualified Data.Map as M
import GHC.Generics (Generic)

-- Add slack vars, need type of system with only equalities

-- Add artificial vars, can we type check this somehow? Maybe with a phantom type? Is Tableau enough?
