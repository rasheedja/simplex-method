{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Linear.Simplex.Prettify
-- Description : Prettifier for "Linear.Simplex.Types" types
-- Copyright   : (c) Junaid Rasheed, 2020-2023
-- License     : BSD-3
-- Maintainer  : jrasheed178@gmail.com
-- Stability   : experimental
--
-- Converts "Linear.Simplex.Types" types into human-readable 'String's
module Linear.Simplex.Prettify where

import Control.Lens
import Data.Generics.Labels ()
import Data.Map qualified as M
import Data.Ratio
import Linear.Simplex.Types

-- | Convert a 'VarConstMap' into a human-readable 'String'
prettyShowVarConstMap :: VarLitMapSum -> String
prettyShowVarConstMap = aux . M.toList
  where
    aux [] = ""
    aux ((vName, vCoeff) : vs) = prettyShowRational vCoeff ++ " * " ++ show vName ++ " + " ++ aux vs
      where
        prettyShowRational r =
          if r < 0
            then "(" ++ r' ++ ")"
            else r'
          where
            r' = if denominator r == 1 then show (numerator r) else show (numerator r) ++ " / " ++ show (numerator r)

-- | Convert a 'PolyConstraint' into a human-readable 'String'
prettyShowPolyConstraint :: PolyConstraint -> String
prettyShowPolyConstraint (LEQ vcm r) = prettyShowVarConstMap vcm ++ " <= " ++ show r
prettyShowPolyConstraint (GEQ vcm r) = prettyShowVarConstMap vcm ++ " >= " ++ show r
prettyShowPolyConstraint (Linear.Simplex.Types.EQ vcm r) = prettyShowVarConstMap vcm ++ " == " ++ show r

-- | Convert an 'ObjectiveFunction' into a human-readable 'String'
prettyShowObjectiveFunction :: ObjectiveFunction -> String
prettyShowObjectiveFunction (Min vcm) = "min: " ++ prettyShowVarConstMap vcm
prettyShowObjectiveFunction (Max vcm) = "max: " ++ prettyShowVarConstMap vcm
