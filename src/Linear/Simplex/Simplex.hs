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

import Prelude hiding (EQ)

import Control.Lens
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger
import Data.Bifunctor
import Data.List
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Ratio (denominator, numerator, (%))
import qualified Data.Text as Text
import GHC.Real (Ratio)
import Linear.Simplex.Types
import Linear.Simplex.Util

-- | Find a feasible solution for the given system of 'PolyConstraint's by performing the first phase of the two-phase simplex method
--  All variables in the 'PolyConstraint' must be positive.
--  If the system is infeasible, return 'Nothing'
--  Otherwise, return the feasible system in 'Dict' as well as a list of slack variables, a list artificial variables, and the objective variable.
findFeasibleSolution :: (MonadIO m, MonadLogger m) => [PolyConstraint] -> m (Maybe FeasibleSystem)
findFeasibleSolution unsimplifiedSystem = do
  logMsg LevelInfo $ "findFeasibleSolution: Looking for solution for " <> showT unsimplifiedSystem
  if null artificialVars -- No artificial vars, we have a feasible system
    then do
      logMsg LevelInfo "findFeasibleSolution: Feasible solution found with no artificial vars"
      pure . Just $ FeasibleSystem systemWithBasicVarsAsDictionary slackVars artificialVars objectiveVar
    else do
      logMsg LevelInfo $ "findFeasibleSolution: Needed to create artificial vars. System with artificial vars (in Tableau form) = " <> showT systemWithBasicVars
      mPhase1Dict <- simplexPivot artificialPivotObjective systemWithBasicVarsAsDictionary
      case mPhase1Dict of
        Just phase1Dict -> do
          logMsg LevelInfo $ "findFeasibleSolution: System after pivoting with objective" <> showT artificialPivotObjective <> ": " <> showT phase1Dict
          let eliminateArtificialVarsFromPhase1Tableau =
                M.map
                  ( \DictValue {..} ->
                      DictValue
                        { varMapSum = M.filterWithKey (\k _ -> k `notElem` artificialVars) varMapSum
                        , ..
                        }
                  )
                  phase1Dict
          case M.lookup objectiveVar eliminateArtificialVarsFromPhase1Tableau of
            Nothing -> do
              logMsg LevelWarn $ "findFeasibleSolution: Objective row not found after eliminatiing artificial vars. This is unexpected. System without artificial vars (in Dict form) = " <> showT eliminateArtificialVarsFromPhase1Tableau
              -- If the objecitve row is not found, the system is feasible iff
              -- the artificial vars sum to zero. The value of an artificial
              -- variable is 0 if non-basic, and the RHS of the row if basic
              let artificialVarsVals = map (\v -> maybe 0 (.constant) (M.lookup v eliminateArtificialVarsFromPhase1Tableau)) artificialVars
              let artificialVarsValsSum = sum artificialVarsVals
              if artificialVarsValsSum == 0
                then do
                  logMsg LevelInfo $ "findFeasibleSolution: Artifical variables sum up to 0, thus original tableau is feasible. System without artificial vars (in Dict form) = " <> showT eliminateArtificialVarsFromPhase1Tableau
                  pure . Just $
                    FeasibleSystem
                      { dict = eliminateArtificialVarsFromPhase1Tableau
                      , slackVars = slackVars
                      , artificialVars = artificialVars
                      , objectiveVar = objectiveVar
                      }
                else do
                  logMsg LevelInfo $ "findFeasibleSolution: Artifical variables sum up to " <> showT artificialVarsValsSum <> ", thus original tableau is infeasible. System without artificial vars (in Dict form) = " <> showT eliminateArtificialVarsFromPhase1Tableau
                  pure Nothing
            Just row ->
              if row.constant == 0
                then do
                  logMsg LevelInfo $ "findFeasibleSolution: Objective RHS is zero after pivoting, thus original tableau is feasible. feasible system (in Dict form) = " <> showT eliminateArtificialVarsFromPhase1Tableau
                  pure . Just $
                    FeasibleSystem
                      { dict = eliminateArtificialVarsFromPhase1Tableau
                      , slackVars = slackVars
                      , artificialVars = artificialVars
                      , objectiveVar = objectiveVar
                      }
                else do
                  unless (row.constant < 0) $ do
                    let errMsg = "findFeasibleSolution: Objective RHS is negative after pivoting. This should be impossible. System without artificial vars (in Dict form) = " <> show eliminateArtificialVarsFromPhase1Tableau
                    logMsg LevelError $ Text.pack errMsg
                    error errMsg
                  logMsg LevelInfo $ "findFeasibleSolution: Objective RHS not zero after phase 1, thus original tableau is infeasible. System without artificial vars (in Dict form) = " <> showT eliminateArtificialVarsFromPhase1Tableau
                  pure Nothing
        Nothing -> do
          logMsg LevelInfo $ "findFeasibleSolution: Infeasible solution found, could not pivot with objective " <> showT artificialPivotObjective <> " over system (in Dict form) = " <> showT systemWithBasicVarsAsDictionary
          pure Nothing
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
    systemWithArtificialVars _ _ = error "systemWithArtificialVars: given system includes non-EQ constraints"

    -- \| Takes a 'Dict' and a '[Var]' as input and returns a 'PivotObjective'.
    -- The 'Dict' represents the tableau of a linear program with artificial
    -- variables, and '[Var]' represents the artificial variables.

    -- The function first filters out the rows of the tableau that correspond
    -- to the artificial variables, and negates them. It then computes the sum
    -- of the negated rows, which represents the 'PivotObjective'.
    createArtificialPivotObjective :: Dict -> [Var] -> PivotObjective
    createArtificialPivotObjective rows artificialVars =
      PivotObjective
        { variable = objectiveVar
        , function = foldVarLitMap $ map (.varMapSum) negatedRowsWithoutArtificialVars
        , constant = sum $ map (.constant) negatedRowsWithoutArtificialVars
        }
      where
        -- Filter out non-artificial entries
        rowsToAdd = M.filterWithKey (\k _ -> k `elem` artificialVars) rows
        negatedRows = M.map (\(DictValue rowVarMapSum rowConstant) -> DictValue (M.map negate rowVarMapSum) (negate rowConstant)) rowsToAdd
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
optimizeFeasibleSystem :: (MonadIO m, MonadLogger m) => ObjectiveFunction -> FeasibleSystem -> m (Maybe Result)
optimizeFeasibleSystem objFunction fsys@(FeasibleSystem {dict = phase1Dict, ..}) = do
  logMsg LevelInfo $ "optimizeFeasibleSystem: Optimizing feasible system " <> showT fsys <> " with objective " <> showT objFunction
  if null artificialVars
    then do
      logMsg LevelInfo $ "optimizeFeasibleSystem: No artificial vars, system is feasible. Pivoting system (in dict form) " <> showT phase1Dict <> " with objective " <> showT normalObjective
      fmap (displayResults . dictionaryFormToTableau) <$> simplexPivot normalObjective phase1Dict
    else do
      logMsg LevelInfo $ "optimizeFeasibleSystem: Artificial vars present. Pivoting system (in dict form) " <> showT phase1Dict <> " with objective " <> showT adjustedObjective
      fmap (displayResults . dictionaryFormToTableau) <$> simplexPivot adjustedObjective phase1Dict
  where
    -- \| displayResults takes a 'Tableau' and returns a 'Result'. The 'Tableau'
    -- represents the final tableau of a linear program after the simplex
    -- algorithm has been applied. The 'Result' contains the value of the
    -- objective variable and a map of the values of all variables appearing
    -- in the system, including the objective variable.
    --
    -- The function first filters out the rows of the tableau that correspond
    -- to the slack and artificial variables. It then extracts the values of
    -- the remaining variables and stores them in a map. If the objective
    -- function is a maximization problem, the map contains the values of the
    -- variables as they appear in the final tableau. If the objective function
    -- is a minimization problem, the map contains the values of the variables
    -- as they appear in the final tableau, except for the objective variable,
    -- which is negated.
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
                        tableauRow.rhs
                    )
                    tableauWithOriginalVars
                Min _ ->
                  M.mapWithKey -- We maximized -objVar, so we negate the objVar to get the final value
                    ( \basicVarName tableauRow ->
                        if basicVarName == objectiveVar
                          then negate $ tableauRow.rhs
                          else tableauRow.rhs
                    )
                    tableauWithOriginalVars

    -- \| Objective to use when optimising the linear program if no artificial
    -- variables were necessary in the first phase. It is essentially the original
    -- objective function, with a potential change of sign based on the type of
    -- problem (Maximization or Minimization).
    normalObjective :: PivotObjective
    normalObjective =
      PivotObjective
        { variable = objectiveVar
        , function = if isMax objFunction then objFunction.objective else M.map negate objFunction.objective
        , constant = 0
        }

    -- \| Objective to use when optimising the linear program if artificial
    -- variables were necessary in the first phase. It is an adjustment to the
    -- original objective function, where the linear coefficients are modified
    -- by back-substitution of the values of the artificial variables.
    adjustedObjective :: PivotObjective
    adjustedObjective =
      PivotObjective
        { variable = objectiveVar
        , function = calcVarMap
        , constant = calcConstants
        }
      where
        -- \| Compute the adjustment to the constant term of the objective
        -- function. It adds up the products of the original coefficients and
        -- the corresponding constant term (rhs) of each artificial variable
        -- in the phase 1 'Dict'.
        calcConstants :: SimplexNum
        calcConstants =
          sum
            $ map
              ( \(var, coeff) ->
                  let multiplyWith = if isMax objFunction then coeff else -coeff
                  in  case M.lookup var phase1Dict of
                        Nothing -> 0
                        Just row -> row.constant * multiplyWith
              )
            $ M.toList objFunction.objective

        -- \| Compute the adjustment to the coefficients of the original
        -- variables in the objective function. It performs back-substitution
        -- of the variables in the original objective function using the
        -- current value of each artificial variable in the phase 1 'Dict'.
        calcVarMap :: VarLitMapSum
        calcVarMap =
          foldVarLitMap $
            map
              ( M.fromList
                  . ( \(var, coeff) ->
                        let multiplyWith = if isMax objFunction then coeff else -coeff
                        in  case M.lookup var phase1Dict of
                              Nothing ->
                                [(var, multiplyWith)]
                              Just row -> map (second (* multiplyWith)) (M.toList $ row.varMapSum)
                    )
              )
              (M.toList objFunction.objective)

-- | Perform the two phase simplex method with a given 'ObjectiveFunction' a system of 'PolyConstraint's.
--  Assumes the 'ObjectiveFunction' and 'PolyConstraint' is not empty.
--  Returns a pair with the first item being the 'Integer' variable equal to the 'ObjectiveFunction'
--  and the second item being a map of the values of all 'Integer' variables appearing in the system, including the 'ObjectiveFunction'.
twoPhaseSimplex :: (MonadIO m, MonadLogger m) => ObjectiveFunction -> [PolyConstraint] -> m (Maybe Result)
twoPhaseSimplex objFunction unsimplifiedSystem = do
  logMsg LevelInfo $ "twoPhaseSimplex: Solving system " <> showT unsimplifiedSystem <> " with objective " <> showT objFunction
  phase1Result <- findFeasibleSolution unsimplifiedSystem
  case phase1Result of
    Just feasibleSystem -> do
      logMsg LevelInfo $ "twoPhaseSimplex: Feasible system found for " <> showT unsimplifiedSystem <> "; Feasible system: " <> showT feasibleSystem
      optimizedSystem <- optimizeFeasibleSystem objFunction feasibleSystem
      logMsg LevelInfo $ "twoPhaseSimplex: Optimized system found for " <> showT unsimplifiedSystem <> "; Optimized system: " <> showT optimizedSystem
      pure optimizedSystem
    Nothing -> do
      logMsg LevelInfo $ "twoPhaseSimplex: Phase 1 gives infeasible result for " <> showT unsimplifiedSystem
      pure Nothing

-- | Perform the simplex pivot algorithm on a system with basic vars, assume that the first row is the 'ObjectiveFunction'.
simplexPivot :: (MonadIO m, MonadLogger m) => PivotObjective -> Dict -> m (Maybe Dict)
simplexPivot objective@(PivotObjective {variable = objectiveVar, function = objectiveFunc, constant = objectiveConstant}) dictionary = do
  logMsg LevelInfo $ "simplexPivot: Pivoting with objective " <> showT objective <> " over system (in Dict form) = " <> showT dictionary
  case mostPositive objectiveFunc of
    Nothing -> do
      logMsg LevelInfo $ "simplexPivot: Pivoting complete as no positive variables found in objective " <> showT objective <> " over system (in Dict form) = " <> showT dictionary
      pure $ Just (insertPivotObjectiveToDict objective dictionary)
    Just pivotNonBasicVar -> do
      logMsg LevelInfo $ "simplexPivot: Non-basic pivoting variable in objective, determined by largest coefficient = " <> showT pivotNonBasicVar
      let mPivotBasicVar = ratioTest dictionary pivotNonBasicVar Nothing Nothing
      case mPivotBasicVar of
        Nothing -> do
          logMsg LevelInfo $ "simplexPivot: Ratio test failed with non-basic variable = " <> showT pivotNonBasicVar <> " over system (in Dict form) = " <> showT dictionary
          pure Nothing
        Just pivotBasicVar -> do
          logMsg LevelInfo $ "simplexPivot: Basic pivoting variable determined by ratio test = " <> showT pivotBasicVar
          let pivotResult = pivot pivotBasicVar pivotNonBasicVar (insertPivotObjectiveToDict objective dictionary)
              pivotedObj =
                let pivotedObjEntry = fromMaybe (error "simplexPivot: Can't find objective after pivoting") $ M.lookup objectiveVar pivotResult
                in  objective & #function .~ pivotedObjEntry.varMapSum & #constant .~ pivotedObjEntry.constant
              pivotedDict = M.delete objectiveVar pivotResult
          logMsg LevelInfo $ "simplexPivot: Pivoting complete, pivoted objective = " <> showT pivotedObj <> " over system (in Dict form) = " <> showT pivotedDict
          simplexPivot
            pivotedObj
            pivotedDict
  where
    ratioTest :: Dict -> Var -> Maybe Var -> Maybe Rational -> Maybe Var
    ratioTest dict = aux (M.toList dict)
      where
        aux :: [(Var, DictValue)] -> Var -> Maybe Var -> Maybe Rational -> Maybe Var
        aux [] _ mCurrentMinBasicVar _ = mCurrentMinBasicVar
        aux (x@(basicVar, dictEquation) : xs) mostNegativeVar mCurrentMinBasicVar mCurrentMin =
          case M.lookup mostNegativeVar dictEquation.varMapSum of
            Nothing -> aux xs mostNegativeVar mCurrentMinBasicVar mCurrentMin
            Just currentCoeff ->
              let dictEquationConstant = dictEquation.constant
              in  if currentCoeff >= 0 || dictEquationConstant < 0
                    then aux xs mostNegativeVar mCurrentMinBasicVar mCurrentMin
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
        Nothing -> Nothing
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
      case M.lookup enteringVariable (dictEntertingRow.varMapSum) of
        Just enteringVariableCoeff ->
          updatedRows
          where
            -- Move entering variable to basis, update other variables in row appropriately
            pivotEnteringRow :: DictValue
            pivotEnteringRow =
              dictEntertingRow
                & #varMapSum
                  %~ ( \basicEquation ->
                        -- uncurry
                        M.insert
                          leavingVariable
                          (-1)
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
            updatedRows =
              M.fromList $ map (uncurry f2) $ M.toList dict
              where
                f entryVar entryVal =
                  if leavingVariable == entryVar
                    then pivotEnteringRow
                    else case M.lookup enteringVariable (entryVal.varMapSum) of
                      Just subsCoeff ->
                        entryVal
                          & #varMapSum
                            .~ combineVarLitMapSums
                              (pivotEnteringRow.varMapSum <&> (subsCoeff *))
                              (filterOutEnteringVarTerm (entryVal.varMapSum))
                          & #constant
                            .~ ((subsCoeff * (pivotEnteringRow.constant)) + entryVal.constant)
                      Nothing -> entryVal

                f2 :: Var -> DictValue -> (Var, DictValue)
                f2 entryVar entryVal =
                  if leavingVariable == entryVar
                    then (enteringVariable, pivotEnteringRow)
                    else case M.lookup enteringVariable (entryVal.varMapSum) of
                      Just subsCoeff ->
                        ( entryVar
                        , entryVal
                            & #varMapSum
                              .~ combineVarLitMapSums
                                (pivotEnteringRow.varMapSum <&> (subsCoeff *))
                                (filterOutEnteringVarTerm (entryVal.varMapSum))
                            & #constant
                              .~ ((subsCoeff * (pivotEnteringRow.constant)) + entryVal.constant)
                        )
                      Nothing -> (entryVar, entryVal)
        Nothing -> error "pivot: non basic variable not found in basic row"
      where
        -- \| The entering row, i.e., the row in the dict which is the value of
        -- leavingVariable.
        dictEntertingRow =
          fromMaybe
            (error "pivot: Basic variable not found in Dict")
            $ M.lookup leavingVariable dict

        filterOutEnteringVarTerm = M.filterWithKey (\vName _ -> vName /= enteringVariable)
