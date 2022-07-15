{-|
Module      : Linear.Prettify
Description : Prettifier for 'Linear.Type' types.
Copyright   : (c) Junaid Rasheed, 2020-2022
License     : BSD-3
Maintainer  : jrasheed178@gmail.com
Stability   : experimental

Converts 'Linear.Type' types into human-readable 'String's 
-}
module Linear.Prettify where

import Linear.Type as T
import Data.Ratio

-- |Convert a 'VarConstMap' into a human-readable 'String'
prettyShowVarConstMap :: VarConstMap -> String
prettyShowVarConstMap [] = ""
prettyShowVarConstMap [(v, c)]  = prettyShowRational c ++ " * x" ++ show v ++ ""
  where
    prettyShowRational r = 
      if r < 0
        then "(" ++ r' ++ ")"
        else r'
      where
        r' = if denominator r == 1 then show (numerator r) else show (numerator r) ++ " / " ++ show (numerator r)

prettyShowVarConstMap ((v, c) : vcs) = prettyShowVarConstMap [(v, c)] ++ " + " ++ prettyShowVarConstMap vcs

-- |Convert a 'PolyConstraint' into a human-readable 'String'
prettyShowPolyConstraint :: PolyConstraint -> String
prettyShowPolyConstraint (LEQ vcm r) = prettyShowVarConstMap vcm ++ " <= " ++ show r
prettyShowPolyConstraint (GEQ vcm r) = prettyShowVarConstMap vcm ++ " >= " ++ show r
prettyShowPolyConstraint (T.EQ vcm r)  = prettyShowVarConstMap vcm ++ " == " ++ show r

-- |Convert an 'ObjectiveFunction' into a human-readable 'String'
prettyShowObjectiveFunction :: ObjectiveFunction -> String
prettyShowObjectiveFunction (Min vcm) = "min: " ++ prettyShowVarConstMap vcm
prettyShowObjectiveFunction (Max vcm) = "max: " ++ prettyShowVarConstMap vcm
