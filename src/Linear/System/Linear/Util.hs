-- |
-- Module: Linear.System.Linear.Util
-- Description: Utility functions for linear programming systems
-- Copyright: (c) Junaid Rasheed, 2024
-- License: BSD-3
-- Maintainer: Junaid Rasheed <jrasheed178@gmail.com>
-- Stability: experimental
module Linear.System.Linear.Util where

import Linear.Constraint.Linear.Types (LinearEquation (..))
import qualified Linear.Constraint.Linear.Util as CLU
import Linear.System.Linear.Types (LinearSystem (..))
import Linear.Var.Types (Var)

-- | Prepend a linear equation to a linear system
prependLinearEquation :: LinearEquation -> LinearSystem -> LinearSystem
prependLinearEquation eq (LinearSystem eqs) = LinearSystem (eq : eqs)

-- | Append a linear equation to a linear system
appendLinearEquation :: LinearEquation -> LinearSystem -> LinearSystem
appendLinearEquation eq (LinearSystem eqs) = LinearSystem (eqs ++ [eq])

findHighestVar :: LinearSystem -> Maybe Var
findHighestVar (LinearSystem []) = Nothing
findHighestVar (LinearSystem eqs) = Just $ maximum $ map CLU.findHighestVar eqs
