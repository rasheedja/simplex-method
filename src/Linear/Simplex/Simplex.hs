{-# LANGUAGE RecordWildCards #-}

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

import Control.Lens
import Data.Bifunctor
import Data.List
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Ratio (denominator, numerator, (%))
import GHC.Real (Ratio)
import Linear.Simplex.Types
import Linear.Simplex.Util
import Prelude hiding (EQ)

-- import Debug.Trace (trace)

trace s a = a

-- | Find a feasible solution for the given system of 'PolyConstraint's by performing the first phase of the two-phase simplex method
--  All 'Integer' variables in the 'PolyConstraint' must be positive.
--  If the system is infeasible, return 'Nothing'
--  Otherwise, return the feasible system in 'Dict' as well as a list of slack variables, a list artificial variables, and the objective variable.
findFeasibleSolution :: [PolyConstraint] -> Maybe FeasibleSystem
findFeasibleSolution unsimplifiedSystem =
  if null artificialVars -- No artificial vars, we have a feasible system
    then Just $ FeasibleSystem systemWithBasicVarsAsDictionary slackVars artificialVars objectiveVar
    else -- else case simplexPivot (objectiveVar, artificialObjective ^. #objective) systemWithBasicVarsAsDictionary of
    case simplexPivot artificialPivotObjective systemWithBasicVarsAsDictionary of
      Just phase1Dict ->
        let eliminateArtificialVarsFromPhase1Tableau =
              M.map
                ( \DictValue {..} ->
                    DictValue
                      { varMapSum = M.filterWithKey (\k _ -> k `notElem` artificialVars) varMapSum
                      , ..
                      }
                )
                phase1Dict
        in  case M.lookup objectiveVar eliminateArtificialVarsFromPhase1Tableau of
              Nothing -> trace "objective row not found in phase 1 tableau" Nothing -- Should this be an error?
              Just row ->
                if row ^. #constant == 0
                  then
                    Just $
                      FeasibleSystem
                        { dict = eliminateArtificialVarsFromPhase1Tableau
                        , slackVars = slackVars
                        , artificialVars = artificialVars
                        , objectiveVar = objectiveVar
                        }
                  else trace "rhs not zero after phase 1, thus original tableau is infeasible" Nothing
      Nothing -> Nothing
  where
    system = simplifySystem unsimplifiedSystem

    maxVar =
      maximum $
        map
          ( \case
              LEQ vcm _ -> maximum (map fst $ M.toList vcm)
              GEQ vcm _ -> maximum (map fst $ M.toList vcm)
              EQ vcm _ -> maximum (map fst $ M.toList vcm)
          )
          system

    (systemWithSlackVars, slackVars) = systemInStandardForm system maxVar []

    maxVarWithSlackVars = if null slackVars then maxVar else maximum slackVars

    (systemWithBasicVars, artificialVars) = systemWithArtificialVars systemWithSlackVars maxVarWithSlackVars

    finalMaxVar = if null artificialVars then maxVarWithSlackVars else maximum artificialVars

    systemWithBasicVarsAsDictionary = tableauInDictionaryForm systemWithBasicVars

    artificialPivotObjective = createArtificialPivotObjective systemWithBasicVarsAsDictionary artificialVars

    objectiveVar = finalMaxVar + 1

    -- Convert a system of 'PolyConstraint's to standard form; a system of only equations ('EQ').
    -- Add slack vars where necessary.
    -- This may give you an infeasible system if slack vars are negative when original variables are zero.
    -- If a constraint is already EQ, set the basic var to Nothing.
    -- Final system is a list of equalities for the given system.
    -- To be feasible, all vars must be >= 0.
    -- TODO: Maybe add equation type, and make this an equation
    -- TODO: If we add an equation type, some other types should become aliases (e.g., TableauRow)
    -- TODO: Proposed return type: ([(Maybe Var, PolyConstraint)], [Var])
    -- (I.e., Partial tableau with slack vars)
    -- TODO: Look into an intermediary system type
    -- Have a type level guarantee for slack vars with 1/-1 coeff
    systemInStandardForm :: [PolyConstraint] -> Var -> [Var] -> ([(Maybe Var, PolyConstraint)], [Var])
    systemInStandardForm [] _ sVars = ([], sVars)
    systemInStandardForm (EQ v r : xs) maxVar sVars = ((Nothing, EQ v r) : newSystem, newSlackVars)
      where
        (newSystem, newSlackVars) = systemInStandardForm xs maxVar sVars
    systemInStandardForm (LEQ v r : xs) maxVar sVars = ((Just newSlackVar, EQ (M.insert newSlackVar 1 v) r) : newSystem, newSlackVars)
      where
        newSlackVar = maxVar + 1
        (newSystem, newSlackVars) = systemInStandardForm xs newSlackVar (newSlackVar : sVars)
    systemInStandardForm (GEQ v r : xs) maxVar sVars = ((Just newSlackVar, EQ (M.insert newSlackVar (-1) v) r) : newSystem, newSlackVars)
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
    systemWithArtificialVars :: [(Maybe Var, PolyConstraint)] -> Var -> (Tableau, [Var])
    systemWithArtificialVars [] _ = (M.empty, [])
    systemWithArtificialVars ((mVar, EQ v r) : pcs) maxVar =
      case mVar of
        Nothing ->
          if r >= 0
            then (M.insert newArtificialVar (TableauRow {lhs = M.insert newArtificialVar 1 v, rhs = r}) newSystemWithNewMaxVar, newArtificialVar : artificialVarsWithNewMaxVar)
            else (M.insert newArtificialVar (TableauRow {lhs = M.insert newArtificialVar (-1) v, rhs = r}) newSystemWithNewMaxVar, newArtificialVar : artificialVarsWithNewMaxVar)
        Just basicVar ->
          case M.lookup basicVar v of
            Just basicVarCoeff ->
              if r == 0
                then (M.insert basicVar (TableauRow {lhs = v, rhs = r}) newSystemWithoutNewMaxVar, artificialVarsWithoutNewMaxVar)
                else
                  if r > 0
                    then
                      if basicVarCoeff >= 0 -- Should only be 1 in the standard call path
                        then (M.insert basicVar (TableauRow {lhs = v, rhs = r}) newSystemWithoutNewMaxVar, artificialVarsWithoutNewMaxVar)
                        else (M.insert newArtificialVar (TableauRow {lhs = M.insert newArtificialVar 1 v, rhs = r}) newSystemWithNewMaxVar, newArtificialVar : artificialVarsWithNewMaxVar) -- Slack var is negative, r is positive (when original constraint was GEQ)
                    else -- r < 0

                      if basicVarCoeff <= 0 -- Should only be -1 in the standard call path
                        then (M.insert basicVar (TableauRow {lhs = v, rhs = r}) newSystemWithoutNewMaxVar, artificialVarsWithoutNewMaxVar)
                        else (M.insert newArtificialVar (TableauRow {lhs = M.insert newArtificialVar (-1) v, rhs = r}) newSystemWithNewMaxVar, newArtificialVar : artificialVarsWithNewMaxVar) -- Slack var is negative, r is negative (when original constraint was LEQ)
            Nothing -> error "1" -- undefined
      where
        newArtificialVar = maxVar + 1

        (newSystemWithNewMaxVar, artificialVarsWithNewMaxVar) = systemWithArtificialVars pcs newArtificialVar

        (newSystemWithoutNewMaxVar, artificialVarsWithoutNewMaxVar) = systemWithArtificialVars pcs maxVar
    systemWithArtificialVars _ _ = error "2" -- undefined

    -- phase1PivotObjective :: PivotObjective
    -- phase1PivotObjective =
    --   PivotObjective
    --     { variable  = objectiveVar
    --     , function  = if isMax objFunction then objFunction ^. #objective else M.map negate (objFunction ^. #objective)
    --     , constants = M.empty
    --     }

    -- Create an artificial objective using the given 'Integer' list of artificialVars and the given 'DictionaryForm'.
    -- The artificial 'ObjectiveFunction' is the negated sum of all artificial vars.
    -- createArtificialObjective :: DictionaryForm -> [Integer] -> ObjectiveFunction
    -- createArtificialObjective rows artificialVars = Max negatedSumWithoutArtificialVars
    --   where
    --     rowsToAdd = filter (\(i, _) -> i `elem` artificialVars) rows
    --     negatedRows = map (\(_, vcm) -> map (second negate) vcm) rowsToAdd
    --     negatedSum = foldSumVarConstMap ((sort . concat) negatedRows)
    --     negatedSumWithoutArtificialVars = filter (\(v, _) -> v `notElem` artificialVars) negatedSum

    createArtificialPivotObjective :: Dict -> [Var] -> PivotObjective
    createArtificialPivotObjective rows artificialVars =
      PivotObjective
        { variable = objectiveVar
        , function = foldVarLitMap $ map (^. #varMapSum) negatedRowsWithoutArtificialVars
        }
      where
        -- test2 = foldr (+) 0 test

        -- test3 :: [Ratio Integer]
        -- test3 = undefined

        -- test = map (^. #constant) negatedRowsWithoutArtificialVars
        -- Filter out non-artificial entries
        rowsToAdd = M.filterWithKey (\k _ -> k `elem` artificialVars) rows
        -- Negate rows, discard keys and artificial vars since the pivot objective does not care about them
        negatedRowsWithoutArtificialVars =
          map
            ( \(_, DictValue {..}) ->
                DictValue
                  { varMapSum = M.map negate $ M.filterWithKey (\k _ -> k `notElem` artificialVars) varMapSum
                  , constant = negate constant
                  }
            )
            $ M.toList rowsToAdd

-- | Optimize a feasible system by performing the second phase of the two-phase simplex method.
--  We first pass an 'ObjectiveFunction'.
--  Then, the feasible system in 'DictionaryForm' as well as a list of slack variables, a list artificial variables, and the objective variable.
--  Returns a pair with the first item being the 'Integer' variable equal to the 'ObjectiveFunction'
--  and the second item being a map of the values of all 'Integer' variables appearing in the system, including the 'ObjectiveFunction'.
optimizeFeasibleSystem :: ObjectiveFunction -> FeasibleSystem -> Maybe Result
optimizeFeasibleSystem unsimplifiedObjFunction (FeasibleSystem {dict = phase1Dict, ..}) =
  if null artificialVars
    then -- then displayResults . dictionaryFormToTableau <$> simplexPivot (createObjectiveDict objFunction objectiveVar : phase1Dict)
    -- else displayResults . dictionaryFormToTableau <$> simplexPivot (createObjectiveDict phase2ObjFunction objectiveVar : tail phase1Dict)
      displayResults . dictionaryFormToTableau <$> simplexPivot phase1PivotObjective phase1Dict
    else displayResults . dictionaryFormToTableau <$> simplexPivot phase2PivotObjective phase1Dict
  where
    objFunction = simplifyObjectiveFunction unsimplifiedObjFunction

    displayResults :: Tableau -> Result
    displayResults tableau =
      Result
        { objectiveVar = objectiveVar
        , varValMap = extractVarVals
        }
      where
        extractVarVals =
          let tableauWithOriginalVars =
                M.filterWithKey
                  ( \basicVarName _ ->
                      basicVarName `notElem` slackVars ++ artificialVars
                  )
                  tableau
          in  case objFunction of
                Max _ ->
                  M.map
                    ( \tableauRow ->
                        tableauRow ^. #rhs
                    )
                    tableauWithOriginalVars
                Min _ ->
                  M.mapWithKey -- We maximized -objVar, so we negate the objVar to get the final value
                    ( \basicVarName tableauRow ->
                        if basicVarName == objectiveVar
                          then negate $ tableauRow ^. #rhs
                          else tableauRow ^. #rhs
                    )
                    tableauWithOriginalVars

    phase1PivotObjective :: PivotObjective
    phase1PivotObjective =
      PivotObjective
        { variable = objectiveVar
        , function = if isMax objFunction then objFunction ^. #objective else M.map negate (objFunction ^. #objective)
        -- , constants = M.empty
        }

    -- TODO: New type for Phase2Objective
    phase2PivotObjective :: PivotObjective
    phase2PivotObjective =
      PivotObjective
        { variable = objectiveVar
        , function = calcVarMap
        -- , constants = calcConstants
        }
      where
        -- type VarLitMapSum = M.Map Var SimplexNum
        -- (foldSumVarConstMap . sort) $

        -- combineMaps =
        --   M.mapWithKey
        --   (\var varValMap ->
        --     case M.lookup var calcConstants of
        --       Just constant ->
        --         DictValue
        --           { varMapSum = M.singleton var varValMap
        --           , constant = constant
        --           }
        --       Nothing -> error "Bad branch"
        --   )
        --   $ calcVarMap

        calcConstants :: VarLitMap
        calcConstants =
          M.mapWithKey
            ( \var coeff ->
                case M.lookup var phase1Dict of
                  Nothing -> 0
                  Just row -> (row ^. #constant) * coeff
            )
            $ objFunction ^. #objective

        calcVarMap :: VarLitMapSum
        calcVarMap =
          M.fromList
            $ concatMap
              ( \(var, coeff) ->
                  let multiplyWith = if isMax objFunction then coeff else -coeff
                  in  case M.lookup var phase1Dict of
                        Nothing ->
                          -- DictValue
                          --   { varMapSum = M.singleton var coeff
                          --   , constant = 0
                          --   }
                          [(var, multiplyWith)]
                        Just row -> map (second (* multiplyWith)) (M.toList $ row ^. #varMapSum)
                        -- row
                        -- & #varMapSum %~ M.map (* coeff)
                        -- & #constant %~ (* coeff) -- TODO: Apply 0
                        -- map (second (* coeff)) row
              )
            $ M.toList (objFunction ^. #objective)

-- phase2ObjFunction =
-- undefined
-- if isMax objFunction then Max phase2ObjectiveRow else Min phase2ObjectiveRow

-- | Perform the two phase simplex method with a given 'ObjectiveFunction' a system of 'PolyConstraint's.
--  Assumes the 'ObjectiveFunction' and 'PolyConstraint' is not empty.
--  Returns a pair with the first item being the 'Integer' variable equal to the 'ObjectiveFunction'
--  and the second item being a map of the values of all 'Integer' variables appearing in the system, including the 'ObjectiveFunction'.
twoPhaseSimplex :: ObjectiveFunction -> [PolyConstraint] -> Maybe Result
twoPhaseSimplex objFunction unsimplifiedSystem =
  case findFeasibleSolution unsimplifiedSystem of
    Just feasibleSystem -> optimizeFeasibleSystem objFunction feasibleSystem
    Nothing -> Nothing

-- | Perform the simplex pivot algorithm on a system with basic vars, assume that the first row is the 'ObjectiveFunction'.
simplexPivot :: PivotObjective -> Dict -> Maybe Dict
simplexPivot objective@(PivotObjective {variable = objectiveVar, function = objectiveVal, ..}) dictionary =
  trace (show dictionary) $
    case mostPositive objectiveVal of
      Nothing ->
        trace
          "all neg \n"
          trace
          (show dictionary)
          Just
          dictionary
      Just pivotNonBasicVar ->
        let mPivotBasicVar = ratioTest dictionary pivotNonBasicVar Nothing Nothing
        in  case mPivotBasicVar of
              Nothing -> trace ("Ratio test failed on non-basic var: " ++ show pivotNonBasicVar ++ "\n" ++ show dictionary) Nothing
              Just pivotBasicVar ->
                trace
                  "one pos \n"
                  trace
                  (show dictionary)
                  simplexPivot
                  objective
                  (pivot pivotBasicVar pivotNonBasicVar dictionary)
  where
    ratioTest :: Dict -> Var -> Maybe Var -> Maybe Rational -> Maybe Var
    ratioTest dict = aux (M.toList dict)
      where
        aux :: [(Var, DictValue)] -> Var -> Maybe Var -> Maybe Rational -> Maybe Var
        aux [] _ mCurrentMinBasicVar _ = mCurrentMinBasicVar
        aux (x@(basicVar, dictEquation) : xs) mostNegativeVar mCurrentMinBasicVar mCurrentMin =
          case M.lookup mostNegativeVar (dictEquation ^. #varMapSum) of
            Nothing -> aux xs mostNegativeVar mCurrentMinBasicVar mCurrentMin
            Just currentCoeff ->
              let dictEquationConstant = dictEquation ^. #constant
              in  if currentCoeff >= 0 || dictEquationConstant < 0
                    then -- trace (show currentCoeff)
                      aux xs mostNegativeVar mCurrentMinBasicVar mCurrentMin -- constant was already in right side in original tableau, so should be above zero
                      -- Coeff needs to be negative since it has been moved to the RHS
                    else case mCurrentMin of
                      Nothing -> aux xs mostNegativeVar (Just basicVar) (Just (dictEquationConstant / currentCoeff))
                      Just currentMin ->
                        if (dictEquationConstant / currentCoeff) >= currentMin
                          then aux xs mostNegativeVar (Just basicVar) (Just (dictEquationConstant / currentCoeff))
                          else aux xs mostNegativeVar mCurrentMinBasicVar mCurrentMin

    mostPositive :: VarLitMapSum -> Maybe Var
    mostPositive varLitMap =
      case findLargestCoeff (M.toList varLitMap) Nothing of
        Just (largestVarName, largestVarCoeff) ->
          if largestVarCoeff <= 0
            then Nothing
            else Just largestVarName
        Nothing -> trace "No variables in first row when looking for most positive" Nothing
      where
        findLargestCoeff :: [(Var, SimplexNum)] -> Maybe (Var, SimplexNum) -> Maybe (Var, SimplexNum)
        findLargestCoeff [] mCurrentMax = mCurrentMax
        findLargestCoeff (v@(vName, vCoeff) : vs) mCurrentMax =
          case mCurrentMax of
            Nothing -> findLargestCoeff vs (Just v)
            Just (_, currentMaxCoeff) ->
              if currentMaxCoeff >= vCoeff
                then findLargestCoeff vs mCurrentMax
                else findLargestCoeff vs (Just v)

    -- Pivot a dictionary using the two given variables.
    -- The first variable is the leaving (non-basic) variable.
    -- The second variable is the entering (basic) variable.
    -- Expects the entering variable to be present in the row containing the leaving variable.
    -- Expects each row to have a unique basic variable.
    -- Expects each basic variable to not appear on the RHS of any equation.
    pivot :: Var -> Var -> Dict -> Dict
    pivot leavingVariable enteringVariable dict =
      -- case basicRow ^. #equation ^? folded . filtered (\vt -> enteringVariable == vt ^. #name) of
      case M.lookup enteringVariable (dictEntertingRow ^. #varMapSum) of
        Just enteringVariableCoeff ->
          updatedRows
          where
            -- Move entering variable to basis, update other variables in row appropriately
            pivotEnteringRow :: DictValue
            pivotEnteringRow =
              dictEntertingRow
                & #varMapSum
                  %~ ( \basicEquation ->
                        uncurry
                          M.insert
                          newEnteringVarTerm
                          (filterOutEnteringVarTerm basicEquation)
                          & traverse
                            %~ divideByNegatedEnteringVariableCoeff
                     )
                & #constant
                  %~ divideByNegatedEnteringVariableCoeff
              where
                newEnteringVarTerm = (leavingVariable, -1)
                divideByNegatedEnteringVariableCoeff = (/ negate enteringVariableCoeff)

            -- Substitute pivot equation into other rows
            updatedRows :: Dict
            updatedRows = M.mapWithKey f dict
              where
                -- Dict
                -- { objective = f $ dict ^. #objectiveFunction
                -- , entries = fmap f $ dict ^. #entries
                -- }

                f entryVar entryVal =
                  if leavingVariable == entryVar
                    then pivotEnteringRow
                    else case M.lookup enteringVariable (entryVal ^. #varMapSum) of
                      Just subsCoeff ->
                        entryVal
                          & #varMapSum
                            %~ ( combineVarLitMapSums
                                  (pivotEnteringRow ^. #varMapSum <&> (subsCoeff *))
                                  . filterOutEnteringVarTerm
                               )
                      Nothing -> entryVal
        Nothing -> error "pivot: non basic variable not found in basic row"
      where
        -- \| The entering row, i.e., the row in the dict which is the value of
        -- leavingVariable.
        dictEntertingRow =
          fromMaybe
            (error "pivot: Basic variable not found in Dict")
            $ M.lookup leavingVariable dict

        -- basicDictEquation = basicDictEntry ^. #rhs

        filterOutEnteringVarTerm = M.filterWithKey (\vName _ -> vName /= enteringVariable)
