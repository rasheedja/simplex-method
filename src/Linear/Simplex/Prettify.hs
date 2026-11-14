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

import Data.Generics.Labels ()
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Ratio (denominator, numerator)
import Linear.Simplex.Types
  ( Dict
  , DictValue (..)
  , ObjectiveFunction (..)
  , PolyConstraint (..)
  , SimplexNum
  , VarLitMapSum
  )

-- | Convert a 'VarLitMapSum' into a human-readable 'String'
prettyShowVarLitMapSum :: VarLitMapSum -> String
prettyShowVarLitMapSum = intercalate " + " . map prettyShowTerm . M.toList
  where
    prettyShowTerm (vName, vCoeff) = prettyShowRational vCoeff ++ " * " ++ show vName

-- | Convert a 'SimplexNum' into a human-readable 'String', parenthesising
-- negative values so they can sit inside a sum.
prettyShowRational :: SimplexNum -> String
prettyShowRational r =
  if r < 0 then "(" ++ rendered ++ ")" else rendered
  where
    rendered =
      if denominator r == 1
        then show (numerator r)
        else show (numerator r) ++ " / " ++ show (denominator r)

-- | Convert a 'Dict' into a human-readable 'String', one row per line as
-- @x<basic> = <constant> + <terms>@.
prettyShowDict :: Dict -> String
prettyShowDict = intercalate "\n" . map prettyShowRow . M.toList
  where
    prettyShowRow (basicVar, DictValue {varMapSum, constant}) =
      let constantTerm = prettyShowRational constant
          terms = prettyShowVarLitMapSum varMapSum
      in  "x"
            ++ show basicVar
            ++ " = "
            ++ constantTerm
            ++ if null terms then "" else " + " ++ terms

-- | Convert a 'PolyConstraint' into a human-readable 'String'
prettyShowPolyConstraint :: PolyConstraint -> String
prettyShowPolyConstraint (LEQ vcm r) = prettyShowVarLitMapSum vcm ++ " <= " ++ show r
prettyShowPolyConstraint (GEQ vcm r) = prettyShowVarLitMapSum vcm ++ " >= " ++ show r
prettyShowPolyConstraint (Linear.Simplex.Types.EQ vcm r) = prettyShowVarLitMapSum vcm ++ " == " ++ show r

-- | Convert an 'ObjectiveFunction' into a human-readable 'String'
prettyShowObjectiveFunction :: ObjectiveFunction -> String
prettyShowObjectiveFunction (Min vcm) = "min: " ++ prettyShowVarLitMapSum vcm
prettyShowObjectiveFunction (Max vcm) = "max: " ++ prettyShowVarLitMapSum vcm
