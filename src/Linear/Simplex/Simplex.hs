{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Linear.Simplex.Simplex
-- Description : Implements the twoPhaseSimplex method
-- Copyright   : (c) Junaid Rasheed, 2020-2022
-- License     : BSD-3
-- Maintainer  : jrasheed178@gmail.com
-- Stability   : experimental
--
-- Module implementing the two-phase simplex method.
-- 'findFeasibleSolution' performs phase one of the two-phase simplex method.
-- 'optimizeFeasibleSystem' performs phase two of the two-phase simplex method.
-- 'twoPhaseSimplex' performs both phases of the two-phase simplex method.
module Linear.Simplex.Simplex (findFeasibleSolution, optimizeFeasibleSystem, twoPhaseSimplex) where

import Data.Bifunctor
import Data.List
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ratio (denominator, numerator, (%))
import Linear.Simplex.Types
import Linear.Simplex.Util
import Prelude hiding (EQ)

-- import Debug.Trace (trace)

trace s a = a

-- | Find a feasible solution for the given system of 'PolyConstraint's by performing the first phase of the two-phase simplex method
--  All 'Integer' variables in the 'PolyConstraint' must be positive.
--  If the system is infeasible, return 'Nothing'
--  Otherwise, return the feasible system in 'DictionaryForm' as well as a list of slack variables, a list artificial variables, and the objective variable.
findFeasibleSolution :: [PolyConstraint] -> Maybe (DictionaryForm, [Integer], [Integer], Integer)
findFeasibleSolution unsimplifiedSystem =
  if null artificialVars -- No artificial vars, we have a feasible system
    then Just (systemWithBasicVarsAsDictionary, slackVars, artificialVars, objectiveVar)
    else case simplexPivot (createObjectiveDict artificialObjective objectiveVar : systemWithBasicVarsAsDictionary) of
      Just phase1Dict ->
        let eliminateArtificialVarsFromPhase1Tableau = map (second (filter (\(v, _) -> v `notElem` artificialVars))) phase1Dict
         in case lookup objectiveVar eliminateArtificialVarsFromPhase1Tableau of
              Nothing -> trace "objective row not found in phase 1 tableau" Nothing -- Should this be an error?
              Just row ->
                if fromMaybe 0 (lookup (-1) row) == 0
                  then Just (eliminateArtificialVarsFromPhase1Tableau, slackVars, artificialVars, objectiveVar)
                  else trace "rhs not zero after phase 1, thus original tableau is infeasible" Nothing
      Nothing -> Nothing
  where
    system = simplifySystem unsimplifiedSystem

    maxVar =
      maximum $
        map
          ( \case
              LEQ vcm _ -> maximum (map fst vcm)
              GEQ vcm _ -> maximum (map fst vcm)
              EQ vcm _ -> maximum (map fst vcm)
          )
          system

    (systemWithSlackVars, slackVars) = systemInStandardForm system maxVar []

    maxVarWithSlackVars = if null slackVars then maxVar else maximum slackVars

    (systemWithBasicVars, artificialVars) = systemWithArtificialVars systemWithSlackVars maxVarWithSlackVars

    finalMaxVar = if null artificialVars then maxVarWithSlackVars else maximum artificialVars

    systemWithBasicVarsAsDictionary = tableauInDictionaryForm systemWithBasicVars

    artificialObjective = createArtificialObjective systemWithBasicVarsAsDictionary artificialVars

    objectiveVar = finalMaxVar + 1

    -- Convert a system of 'PolyConstraint's to standard form; a system of only equations ('EQ').
    -- Add slack vars where necessary.
    -- This may give you an infeasible system if slack vars are negative when original variables are zero.
    -- If a constraint is already EQ, set the basic var to Nothing.
    -- Final system is a list of equalities for the given system.
    -- To be feasible, all vars must be >= 0.
    systemInStandardForm :: [PolyConstraint] -> Integer -> [Integer] -> ([(Maybe Integer, PolyConstraint)], [Integer])
    systemInStandardForm [] _ sVars = ([], sVars)
    systemInStandardForm (EQ v r : xs) maxVar sVars = ((Nothing, EQ v r) : newSystem, newSlackVars)
      where
        (newSystem, newSlackVars) = systemInStandardForm xs maxVar sVars
    systemInStandardForm (LEQ v r : xs) maxVar sVars = ((Just newSlackVar, EQ (v ++ [(newSlackVar, 1)]) r) : newSystem, newSlackVars)
      where
        newSlackVar = maxVar + 1
        (newSystem, newSlackVars) = systemInStandardForm xs newSlackVar (newSlackVar : sVars)
    systemInStandardForm (GEQ v r : xs) maxVar sVars = ((Just newSlackVar, EQ (v ++ [(newSlackVar, -1)]) r) : newSystem, newSlackVars)
      where
        newSlackVar = maxVar + 1
        (newSystem, newSlackVars) = systemInStandardForm xs newSlackVar (newSlackVar : sVars)

    -- Add artificial vars to a system of 'PolyConstraint's.
    -- Artificial vars are added when:
    --  Basic var is Nothing (When the original constraint was already an EQ).
    --  Slack var is equal to a negative value (this is infeasible, all vars need to be >= 0).
    --  Final system will be a feasible artificial system.
    -- We keep track of artificial vars in the second item of the returned pair so they can be eliminated once phase 1 is complete.
    -- If an artificial var would normally be negative, we negate the row so we can keep artificial variables equal to 1
    systemWithArtificialVars :: [(Maybe Integer, PolyConstraint)] -> Integer -> (Tableau, [Integer])
    systemWithArtificialVars [] _ = ([], [])
    systemWithArtificialVars ((mVar, EQ v r) : pcs) maxVar =
      case mVar of
        Nothing ->
          if r >= 0
            then ((newArtificialVar, (v ++ [(newArtificialVar, 1)], r)) : newSystemWithNewMaxVar, newArtificialVar : artificialVarsWithNewMaxVar)
            else ((newArtificialVar, (v ++ [(newArtificialVar, -1)], r)) : newSystemWithNewMaxVar, newArtificialVar : artificialVarsWithNewMaxVar)
        Just basicVar ->
          case lookup basicVar v of
            Just basicVarCoeff ->
              if r == 0
                then ((basicVar, (v, r)) : newSystemWithoutNewMaxVar, artificialVarsWithoutNewMaxVar)
                else
                  if r > 0
                    then
                      if basicVarCoeff >= 0 -- Should only be 1 in the standard call path
                        then ((basicVar, (v, r)) : newSystemWithoutNewMaxVar, artificialVarsWithoutNewMaxVar)
                        else ((newArtificialVar, (v ++ [(newArtificialVar, 1)], r)) : newSystemWithNewMaxVar, newArtificialVar : artificialVarsWithNewMaxVar) -- Slack var is negative, r is positive (when original constraint was GEQ)
                    else -- r < 0

                      if basicVarCoeff <= 0 -- Should only be -1 in the standard call path
                        then ((basicVar, (v, r)) : newSystemWithoutNewMaxVar, artificialVarsWithoutNewMaxVar)
                        else ((newArtificialVar, (v ++ [(newArtificialVar, -1)], r)) : newSystemWithNewMaxVar, newArtificialVar : artificialVarsWithNewMaxVar) -- Slack var is negative, r is negative (when original constraint was LEQ)
      where
        newArtificialVar = maxVar + 1

        (newSystemWithNewMaxVar, artificialVarsWithNewMaxVar) = systemWithArtificialVars pcs newArtificialVar

        (newSystemWithoutNewMaxVar, artificialVarsWithoutNewMaxVar) = systemWithArtificialVars pcs maxVar

    -- Create an artificial objective using the given 'Integer' list of artificialVars and the given 'DictionaryForm'.
    -- The artificial 'ObjectiveFunction' is the negated sum of all artificial vars.
    createArtificialObjective :: DictionaryForm -> [Integer] -> ObjectiveFunction
    createArtificialObjective rows artificialVars = Max negatedSumWithoutArtificialVars
      where
        rowsToAdd = filter (\(i, _) -> i `elem` artificialVars) rows
        negatedRows = map (\(_, vcm) -> map (second negate) vcm) rowsToAdd
        negatedSum = foldSumVarConstMap ((sort . concat) negatedRows)
        negatedSumWithoutArtificialVars = filter (\(v, _) -> v `notElem` artificialVars) negatedSum

-- | Optimize a feasible system by performing the second phase of the two-phase simplex method.
--  We first pass an 'ObjectiveFunction'.
--  Then, the feasible system in 'DictionaryForm' as well as a list of slack variables, a list artificial variables, and the objective variable.
--  Returns a pair with the first item being the 'Integer' variable equal to the 'ObjectiveFunction'
--  and the second item being a map of the values of all 'Integer' variables appearing in the system, including the 'ObjectiveFunction'.
optimizeFeasibleSystem :: ObjectiveFunction -> DictionaryForm -> [Integer] -> [Integer] -> Integer -> Maybe (Integer, [(Integer, Rational)])
optimizeFeasibleSystem unsimplifiedObjFunction phase1Dict slackVars artificialVars objectiveVar =
  if null artificialVars
    then displayResults . dictionaryFormToTableau <$> simplexPivot (createObjectiveDict objFunction objectiveVar : phase1Dict)
    else displayResults . dictionaryFormToTableau <$> simplexPivot (createObjectiveDict phase2ObjFunction objectiveVar : tail phase1Dict)
  where
    objFunction = simplifyObjectiveFunction unsimplifiedObjFunction

    displayResults :: Tableau -> (Integer, [(Integer, Rational)])
    displayResults tableau =
      ( objectiveVar,
        case objFunction of
          Max _ ->
            map
              (second snd)
              $ filter (\(basicVar, _) -> basicVar `notElem` slackVars ++ artificialVars) tableau
          Min _ ->
            map -- We maximized -objVar, so we negate the objVar to get the final value
              (\(basicVar, row) -> if basicVar == objectiveVar then (basicVar, negate (snd row)) else (basicVar, snd row))
              $ filter (\(basicVar, _) -> basicVar `notElem` slackVars ++ artificialVars) tableau
      )

    phase2Objective =
      (foldSumVarConstMap . sort) $
        concatMap
          ( \(var, coeff) ->
              case lookup var phase1Dict of
                Nothing -> [(var, coeff)]
                Just row -> map (second (* coeff)) row
          )
          (getObjective objFunction)

    phase2ObjFunction = if isMax objFunction then Max phase2Objective else Min phase2Objective

-- | Perform the two phase simplex method with a given 'ObjectiveFunction' a system of 'PolyConstraint's.
--  Assumes the 'ObjectiveFunction' and 'PolyConstraint' is not empty.
--  Returns a pair with the first item being the 'Integer' variable equal to the 'ObjectiveFunction'
--  and the second item being a map of the values of all 'Integer' variables appearing in the system, including the 'ObjectiveFunction'.
twoPhaseSimplex :: ObjectiveFunction -> [PolyConstraint] -> Maybe (Integer, [(Integer, Rational)])
twoPhaseSimplex objFunction unsimplifiedSystem =
  case findFeasibleSolution unsimplifiedSystem of
    Just r@(phase1Dict, slackVars, artificialVars, objectiveVar) -> optimizeFeasibleSystem objFunction phase1Dict slackVars artificialVars objectiveVar
    Nothing -> Nothing

-- | Perform the simplex pivot algorithm on a system with basic vars, assume that the first row is the 'ObjectiveFunction'.
simplexPivot :: DictionaryForm -> Maybe DictionaryForm
simplexPivot dictionary =
  trace (show dictionary) $
    case mostPositive (head dictionary) of
      Nothing ->
        trace
          "all neg \n"
          trace
          (show dictionary)
          Just
          dictionary
      Just pivotNonBasicVar ->
        let mPivotBasicVar = ratioTest (tail dictionary) pivotNonBasicVar Nothing Nothing
         in case mPivotBasicVar of
              Nothing -> trace ("Ratio test failed on non-basic var: " ++ show pivotNonBasicVar ++ "\n" ++ show dictionary) Nothing
              Just pivotBasicVar ->
                trace
                  "one pos \n"
                  trace
                  (show dictionary)
                  simplexPivot
                  (pivot pivotBasicVar pivotNonBasicVar dictionary)
  where
    ratioTest :: DictionaryForm -> Integer -> Maybe Integer -> Maybe Rational -> Maybe Integer
    ratioTest [] _ mCurrentMinBasicVar _ = mCurrentMinBasicVar
    ratioTest ((basicVar, lp) : xs) mostNegativeVar mCurrentMinBasicVar mCurrentMin =
      case lookup mostNegativeVar lp of
        Nothing -> ratioTest xs mostNegativeVar mCurrentMinBasicVar mCurrentMin
        Just currentCoeff ->
          let rhs = fromMaybe 0 (lookup (-1) lp)
           in if currentCoeff >= 0 || rhs < 0
                then -- trace (show currentCoeff)
                  ratioTest xs mostNegativeVar mCurrentMinBasicVar mCurrentMin -- rhs was already in right side in original tableau, so should be above zero
                  -- Coeff needs to be negative since it has been moved to the RHS
                else case mCurrentMin of
                  Nothing -> ratioTest xs mostNegativeVar (Just basicVar) (Just (rhs / currentCoeff))
                  Just currentMin ->
                    if (rhs / currentCoeff) >= currentMin
                      then ratioTest xs mostNegativeVar (Just basicVar) (Just (rhs / currentCoeff))
                      else ratioTest xs mostNegativeVar mCurrentMinBasicVar mCurrentMin

    mostPositive :: (Integer, VarConstMap) -> Maybe Integer
    mostPositive (_, lp) =
      case findLargestCoeff lp Nothing of
        Just (largestVar, largestCoeff) ->
          if largestCoeff <= 0
            then Nothing
            else Just largestVar
        Nothing -> trace "No variables in first row when looking for most positive" Nothing
      where
        findLargestCoeff :: VarConstMap -> Maybe (Integer, Rational) -> Maybe (Integer, Rational)
        findLargestCoeff [] mCurrentMax = mCurrentMax
        findLargestCoeff ((var, coeff) : xs) mCurrentMax =
          if var == (-1)
            then findLargestCoeff xs mCurrentMax
            else case mCurrentMax of
              Nothing -> findLargestCoeff xs (Just (var, coeff))
              Just currentMax ->
                if snd currentMax >= coeff
                  then findLargestCoeff xs mCurrentMax
                  else findLargestCoeff xs (Just (var, coeff))

    -- Pivot a dictionary using the two given variables.
    -- The first variable is the leaving (non-basic) variable.
    -- The second variable is the entering (basic) variable.
    -- Expects the entering variable to be present in the row containing the leaving variable.
    -- Expects each row to have a unique basic variable.
    -- Expects each basic variable to not appear on the RHS of any equation.
    pivot :: Integer -> Integer -> DictionaryForm -> DictionaryForm
    pivot leavingVariable enteringVariable rows =
      case lookup enteringVariable basicRow of
        Just nonBasicCoeff ->
          updatedRows
          where
            -- Move entering variable to basis, update other variables in row appropriately
            pivotEquation = (enteringVariable, map (second (/ negate nonBasicCoeff)) ((leavingVariable, -1) : filter ((enteringVariable /=) . fst) basicRow))
            -- Substitute pivot equation into other rows
            updatedRows =
              map
                ( \(basicVar, vMap) ->
                    if leavingVariable == basicVar
                      then pivotEquation
                      else case lookup enteringVariable vMap of
                        Just subsCoeff -> (basicVar, (foldSumVarConstMap . sort) (map (second (subsCoeff *)) (snd pivotEquation) ++ filter ((enteringVariable /=) . fst) vMap))
                        Nothing -> (basicVar, vMap)
                )
                rows
        Nothing -> trace "non basic variable not found in basic row" undefined
      where
        (_, basicRow) = head $ filter ((leavingVariable ==) . fst) rows
