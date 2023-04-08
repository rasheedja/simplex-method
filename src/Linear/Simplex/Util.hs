{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Linear.Simplex.Util
-- Description : Helper functions
-- Copyright   : (c) Junaid Rasheed, 2020-2022
-- License     : BSD-3
-- Maintainer  : jrasheed178@gmail.com
-- Stability   : experimental
--
-- Helper functions for performing the two-phase simplex method.
module Linear.Simplex.Util where

import Data.Bifunctor
import Data.List
import Linear.Simplex.Types
import Prelude hiding (EQ)

-- | Is the given 'ObjectiveFunction' to be 'Max'imized?
isMax :: ObjectiveFunction -> Bool
isMax (Max _) = True
isMax (Min _) = False

-- | Extract the objective ('VarConstMap') from an 'ObjectiveFunction'
getObjective :: ObjectiveFunction -> VarConstMap
getObjective (Max o) = o
getObjective (Min o) = o

-- | Simplifies a system of 'PolyConstraint's by first calling 'simplifyPolyConstraint',
--  then reducing 'LEQ' and 'GEQ' with same LHS and RHS (and other similar situations) into 'EQ',
--  and finally removing duplicate elements using 'nub'.
simplifySystem :: [PolyConstraint] -> [PolyConstraint]
simplifySystem = nub . reduceSystem . map simplifyPolyConstraint
  where
    reduceSystem :: [PolyConstraint] -> [PolyConstraint]
    reduceSystem [] = []
    -- Reduce LEQ with matching GEQ and EQ into EQ
    reduceSystem ((LEQ lhs rhs) : pcs) =
      let matchingConstraints =
            filter
              ( \case
                  GEQ lhs' rhs' -> lhs == lhs' && rhs == rhs'
                  EQ lhs' rhs' -> lhs == lhs' && rhs == rhs'
                  _ -> False
              )
              pcs
       in if null matchingConstraints
            then LEQ lhs rhs : reduceSystem pcs
            else EQ lhs rhs : reduceSystem (pcs \\ matchingConstraints)
    -- Reduce GEQ with matching LEQ and EQ into EQ
    reduceSystem ((GEQ lhs rhs) : pcs) =
      let matchingConstraints =
            filter
              ( \case
                  LEQ lhs' rhs' -> lhs == lhs' && rhs == rhs'
                  EQ lhs' rhs' -> lhs == lhs' && rhs == rhs'
                  _ -> False
              )
              pcs
       in if null matchingConstraints
            then GEQ lhs rhs : reduceSystem pcs
            else EQ lhs rhs : reduceSystem (pcs \\ matchingConstraints)
    -- Reduce EQ with matching LEQ and GEQ into EQ
    reduceSystem ((EQ lhs rhs) : pcs) =
      let matchingConstraints =
            filter
              ( \case
                  LEQ lhs' rhs' -> lhs == lhs' && rhs == rhs'
                  GEQ lhs' rhs' -> lhs == lhs' && rhs == rhs'
                  _ -> False
              )
              pcs
       in if null matchingConstraints
            then EQ lhs rhs : reduceSystem pcs
            else EQ lhs rhs : reduceSystem (pcs \\ matchingConstraints)

-- | Simplify an 'ObjectiveFunction' by first 'sort'ing and then calling 'foldSumVarConstMap' on the 'VarConstMap'.
simplifyObjectiveFunction :: ObjectiveFunction -> ObjectiveFunction
simplifyObjectiveFunction (Max varConstMap) = Max (foldSumVarConstMap (sort varConstMap))
simplifyObjectiveFunction (Min varConstMap) = Min (foldSumVarConstMap (sort varConstMap))

-- | Simplify a 'PolyConstraint' by first 'sort'ing and then calling 'foldSumVarConstMap' on the 'VarConstMap'.
simplifyPolyConstraint :: PolyConstraint -> PolyConstraint
simplifyPolyConstraint (LEQ varConstMap rhs) = LEQ (foldSumVarConstMap (sort varConstMap)) rhs
simplifyPolyConstraint (GEQ varConstMap rhs) = GEQ (foldSumVarConstMap (sort varConstMap)) rhs
simplifyPolyConstraint (EQ varConstMap rhs) = EQ (foldSumVarConstMap (sort varConstMap)) rhs

-- | Add a sorted list of 'VarConstMap's, folding where the variables are equal
foldSumVarConstMap :: [(Integer, Rational)] -> [(Integer, Rational)]
foldSumVarConstMap [] = []
foldSumVarConstMap [(v, c)] = [(v, c)]
foldSumVarConstMap ((v1, c1) : (v2, c2) : vcm) =
  if v1 == v2
    then
      let newC = c1 + c2
       in if newC == 0
            then foldSumVarConstMap vcm
            else foldSumVarConstMap $ (v1, c1 + c2) : vcm
    else (v1, c1) : foldSumVarConstMap ((v2, c2) : vcm)

-- | Get a map of the value of every 'Integer' variable in a 'Tableau'
displayTableauResults :: Tableau -> [(Integer, Rational)]
displayTableauResults = map (\(basicVar, (_, rhs)) -> (basicVar, rhs))

-- | Get a map of the value of every 'Integer' variable in a 'DictionaryForm'
displayDictionaryResults :: DictionaryForm -> [(Integer, Rational)]
displayDictionaryResults dict = displayTableauResults $ dictionaryFormToTableau dict

-- | Map the given 'Integer' variable to the given 'ObjectiveFunction', for entering into 'DictionaryForm'.
createObjectiveDict :: ObjectiveFunction -> Integer -> (Integer, VarConstMap)
createObjectiveDict (Max obj) objectiveVar = (objectiveVar, obj)
createObjectiveDict (Min obj) objectiveVar = (objectiveVar, map (second negate) obj)

-- | Converts a 'Tableau' to 'DictionaryForm'.
--  We do this by isolating the basic variable on the LHS, ending up with all non basic variables and a 'Rational' constant on the RHS.
--  (-1) is used to represent the rational constant.
tableauInDictionaryForm :: Tableau -> DictionaryForm
tableauInDictionaryForm [] = []
tableauInDictionaryForm ((basicVar, (vcm, r)) : rows) =
  (basicVar, (-1, r / basicCoeff) : map (\(v, c) -> (v, negate c / basicCoeff)) nonBasicVars) : tableauInDictionaryForm rows
  where
    basicCoeff = if null basicVars then 1 else snd $ head basicVars
    (basicVars, nonBasicVars) = partition (\(v, _) -> v == basicVar) vcm

-- | Converts a 'DictionaryForm' to a 'Tableau'.
--  This is done by moving all non-basic variables from the right to the left.
--  The rational constant (represented by the 'Integer' variable -1) stays on the right.
--  The basic variables will have a coefficient of 1 in the 'Tableau'.
dictionaryFormToTableau :: DictionaryForm -> Tableau
dictionaryFormToTableau [] = []
dictionaryFormToTableau ((basicVar, row) : rows) =
  (basicVar, ((basicVar, 1) : map (second negate) nonBasicVars, r)) : dictionaryFormToTableau rows
  where
    (rationalConstant, nonBasicVars) = partition (\(v, _) -> v == (-1)) row
    r = if null rationalConstant then 0 else (snd . head) rationalConstant -- If there is no rational constant found in the right side, the rational constant is 0.

-- | If this function is given 'Nothing', return 'Nothing'.
--  Otherwise, we 'lookup' the 'Integer' given in the first item of the pair in the map given in the second item of the pair.
--  This is typically used to extract the value of the 'ObjectiveFunction' after calling 'Linear.Simplex.Simplex.twoPhaseSimplex'.
extractObjectiveValue :: Maybe (Integer, [(Integer, Rational)]) -> Maybe Rational
extractObjectiveValue Nothing = Nothing
extractObjectiveValue (Just (objVar, results)) =
  case lookup objVar results of
    Nothing -> error "Objective not found in results when extracting objective value"
    r -> r
