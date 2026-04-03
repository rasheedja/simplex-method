-- |
-- Module      : Linear.Simplex.Simplex.TwoPhase
-- Description : Implements the twoPhaseSimplex method
-- Copyright   : (c) Junaid Rasheed, 2020-2023
-- License     : BSD-3
-- Maintainer  : jrasheed178@gmail.com
-- Stability   : experimental
--
-- | Module implementing the two-phase simplex method.
-- 'findFeasibleSolution' performs phase one of the two-phase simplex method.
-- 'optimizeFeasibleSystem' performs phase two of the two-phase simplex method.
-- 'twoPhaseSimplex' performs both phases of the two-phase simplex method.
-- 'twoPhaseSimplex' supports variable domains via its 'VarDomainMap' argument.
module Linear.Simplex.Solver.TwoPhase
  ( findFeasibleSolution
  , optimizeFeasibleSystem
  , twoPhaseSimplex
  -- Internal functions exported for testing
  , preprocess
  , postprocess
  , computeObjective
  , collectAllVars
  , generateTransform
  , getTransform
  , applyTransforms
  , applyTransform
  , applyShiftToObjective
  , applyShiftToConstraint
  , applySplitToObjective
  , applySplitToConstraint
  , unapplyTransformsToVarMap
  , unapplyTransformToVarMap
  ) where

import Prelude hiding (EQ)

import Control.Lens ((%~), (&), (.~), (<&>))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LogLevel (LevelError, LevelInfo, LevelWarn), MonadLogger)
import Data.Bifunctor (second)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Ratio (denominator, numerator, (%))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import GHC.Real (Ratio)
import Linear.Simplex.Types
  ( Dict
  , DictValue (..)
  , FeasibleSystem (..)
  , ObjectiveFunction (..)
  , ObjectiveResult (..)
  , OptimisationOutcome (..)
  , PivotObjective (..)
  , PolyConstraint (..)
  , SimplexNum
  , SimplexResult (..)
  , Tableau
  , TableauRow (..)
  , Var
  , VarDomain (..)
  , VarDomainMap (..)
  , VarLitMap
  , VarLitMapSum
  , VarTransform (..)
  , nonNegative
  , unbounded
  )
import Linear.Simplex.Util
  ( combineVarLitMapSums
  , dictionaryFormToTableau
  , foldVarLitMap
  , insertPivotObjectiveToDict
  , isMax
  , logMsg
  , showT
  , simplifySystem
  , tableauInDictionaryForm
  )

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
      logMsg LevelInfo $
        "findFeasibleSolution: Needed to create artificial vars. System with artificial vars (in Tableau form) "
          <> showT systemWithBasicVars
      mPhase1Dict <- simplexPivot artificialPivotObjective systemWithBasicVarsAsDictionary
      case mPhase1Dict of
        Just phase1Dict -> do
          logMsg LevelInfo $
            "findFeasibleSolution: System after pivoting with objective"
              <> showT artificialPivotObjective
              <> ": "
              <> showT phase1Dict
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
              logMsg LevelWarn $
                "findFeasibleSolution: Objective row not found after eliminatiing artificial vars. This is unexpected. System without artificial vars (in Dict form) "
                  <> showT eliminateArtificialVarsFromPhase1Tableau
              -- If the objecitve row is not found, the system is feasible iff
              -- the artificial vars sum to zero. The value of an artificial
              -- variable is 0 if non-basic, and the RHS of the row if basic
              let artificialVarsVals = map (\v -> maybe 0 (.constant) (M.lookup v eliminateArtificialVarsFromPhase1Tableau)) artificialVars
              let artificialVarsValsSum = sum artificialVarsVals
              if artificialVarsValsSum == 0
                then do
                  logMsg LevelInfo $
                    "findFeasibleSolution: Artifical variables sum up to 0, thus original tableau is feasible. System without artificial vars (in Dict form) "
                      <> showT eliminateArtificialVarsFromPhase1Tableau
                  pure . Just $
                    FeasibleSystem
                      { dict = eliminateArtificialVarsFromPhase1Tableau
                      , slackVars = slackVars
                      , artificialVars = artificialVars
                      , objectiveVar = objectiveVar
                      }
                else do
                  logMsg LevelInfo $
                    "findFeasibleSolution: Artifical variables sum up to "
                      <> showT artificialVarsValsSum
                      <> ", thus original tableau is infeasible. System without artificial vars (in Dict form) "
                      <> showT eliminateArtificialVarsFromPhase1Tableau
                  pure Nothing
            Just row ->
              if row.constant == 0
                then do
                  logMsg LevelInfo $
                    "findFeasibleSolution: Objective RHS is zero after pivoting, thus original tableau is feasible. feasible system (in Dict form) "
                      <> showT eliminateArtificialVarsFromPhase1Tableau
                  pure . Just $
                    FeasibleSystem
                      { dict = eliminateArtificialVarsFromPhase1Tableau
                      , slackVars = slackVars
                      , artificialVars = artificialVars
                      , objectiveVar = objectiveVar
                      }
                else do
                  unless (row.constant < 0) $ do
                    let errMsg =
                          "findFeasibleSolution: Objective RHS is negative after pivoting. This should be impossible. System without artificial vars (in Dict form) "
                            <> show eliminateArtificialVarsFromPhase1Tableau
                    logMsg LevelError $ Text.pack errMsg
                    error errMsg
                  logMsg LevelInfo $
                    "findFeasibleSolution: Objective RHS not zero after phase 1, thus original tableau is infeasible. System without artificial vars (in Dict form) "
                      <> showT eliminateArtificialVarsFromPhase1Tableau
                  pure Nothing
        Nothing -> do
          logMsg LevelInfo $
            "findFeasibleSolution: Infeasible solution found, could not pivot with objective "
              <> showT artificialPivotObjective
              <> " over system (in Dict form) "
              <> showT systemWithBasicVarsAsDictionary
          pure Nothing
  where
    system = simplifySystem unsimplifiedSystem

    maxVar =
      if null system
        then 0
        else
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
            then
              ( M.insert newArtificialVar (TableauRow {lhs = M.insert newArtificialVar 1 v, rhs = r}) newSystemWithNewMaxVar
              , newArtificialVar : artificialVarsWithNewMaxVar
              )
            else
              ( M.insert newArtificialVar (TableauRow {lhs = M.insert newArtificialVar (-1) v, rhs = r}) newSystemWithNewMaxVar
              , newArtificialVar : artificialVarsWithNewMaxVar
              )
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
                        else
                          ( M.insert newArtificialVar (TableauRow {lhs = M.insert newArtificialVar 1 v, rhs = r}) newSystemWithNewMaxVar
                          , newArtificialVar : artificialVarsWithNewMaxVar -- Slack var is negative, r is positive (when original constraint was GEQ)
                          )
                    else -- r < 0
                      if basicVarCoeff <= 0 -- Should only be -1 in the standard call path
                        then (M.insert basicVar (TableauRow {lhs = v, rhs = r}) newSystemWithoutNewMaxVar, artificialVarsWithoutNewMaxVar)
                        else
                          ( M.insert newArtificialVar (TableauRow {lhs = M.insert newArtificialVar (-1) v, rhs = r}) newSystemWithNewMaxVar
                          , newArtificialVar : artificialVarsWithNewMaxVar -- Slack var is negative, r is negative (when original constraint was LEQ)
                          )
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
--  Then, the feasible system in 'Dict' form as well as a list of slack variables, a list artificial variables, and the objective variable.
--  Returns 'Optimal' with variable values if an optimal solution is found, or 'Unbounded' if the objective is unbounded.
optimizeFeasibleSystem :: (MonadIO m, MonadLogger m) => ObjectiveFunction -> FeasibleSystem -> m OptimisationOutcome
optimizeFeasibleSystem objFunction fsys@(FeasibleSystem {dict = phase1Dict, ..}) = do
  logMsg LevelInfo $
    "optimizeFeasibleSystem: Optimizing feasible system " <> showT fsys <> " with objective " <> showT objFunction
  mResult <-
    if null artificialVars
      then do
        logMsg LevelInfo $
          "optimizeFeasibleSystem: No artificial vars, system is feasible. Pivoting system (in dict form) "
            <> showT phase1Dict
            <> " with objective "
            <> showT normalObjective
        simplexPivot normalObjective phase1Dict
      else do
        logMsg LevelInfo $
          "optimizeFeasibleSystem: Artificial vars present. Pivoting system (in dict form) "
            <> showT phase1Dict
            <> " with objective "
            <> showT adjustedObjective
        simplexPivot adjustedObjective phase1Dict
  case mResult of
    Nothing -> do
      logMsg LevelInfo "optimizeFeasibleSystem: Objective is unbounded (ratio test failed)"
      pure Unbounded
    Just resultDict -> do
      let result = displayResults (dictionaryFormToTableau resultDict)
      logMsg LevelInfo $ "optimizeFeasibleSystem: Found optimal solution: " <> showT result
      pure result
  where
    -- \| displayResults takes a 'Tableau' and returns an 'OptimisationOutcome'. The 'Tableau'
    -- represents the final tableau of a linear program after the simplex
    -- algorithm has been applied. The 'OptimisationOutcome' contains the values of all
    -- variables appearing in the system.
    --
    -- The function first filters out the rows of the tableau that correspond
    -- to the slack and artificial variables. It then extracts the values of
    -- the remaining variables and stores them in a map. If the objective
    -- function is a maximization problem, the map contains the values of the
    -- variables as they appear in the final tableau. If the objective function
    -- is a minimization problem, the map contains the values of the variables
    -- as they appear in the final tableau, except for the objective variable,
    -- which is negated.
    displayResults :: Tableau -> OptimisationOutcome
    displayResults tableau =
      Optimal extractVarVals
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

-- | Perform the two phase simplex method with variable domain information.
-- Variables not in the VarDomainMap are assumed to be Unbounded (no lower bound).
-- This function applies necessary transformations before solving and unapplies them after.
-- The returned SimplexResult contains:
--   * The feasible system (Nothing if infeasible)
--   * Results for each objective function (empty if infeasible)
twoPhaseSimplex ::
  (MonadIO m, MonadLogger m) => VarDomainMap -> [ObjectiveFunction] -> [PolyConstraint] -> m SimplexResult
twoPhaseSimplex domainMap objFunctions constraints = do
  logMsg LevelInfo $
    "twoPhaseSimplex: Solving system with domain map " <> showT domainMap
  -- Collect original variables before any transformations
  let originalVars = collectAllVars objFunctions constraints
  let (transformedObjs, transformedConstraints, transforms) = preprocess objFunctions domainMap constraints
  logMsg LevelInfo $
    "twoPhaseSimplex: Applied transforms "
      <> showT transforms
      <> "; Transformed objectives: "
      <> showT transformedObjs
      <> "; Transformed constraints: "
      <> showT transformedConstraints
  mFeasibleSystem <- findFeasibleSolution transformedConstraints
  case mFeasibleSystem of
    Nothing -> do
      logMsg LevelInfo "twoPhaseSimplex: No feasible solution found in phase 1"
      pure $ SimplexResult Nothing []
    Just feasibleSystem -> do
      logMsg LevelInfo $
        "twoPhaseSimplex: Feasible system found for transformed system; Feasible system: "
          <> showT feasibleSystem
      objResults <- optimizeAllObjectives originalVars transforms feasibleSystem (zip objFunctions transformedObjs)
      logMsg LevelInfo $ "twoPhaseSimplex: All objective results: " <> showT objResults
      pure $ SimplexResult (Just feasibleSystem) objResults

-- | Optimize all objective functions over the given feasible system.
-- Returns a list of ObjectiveResult, one for each objective function.
-- The originalVars set is used to filter the result to only include original decision variables.
optimizeAllObjectives ::
  (MonadIO m, MonadLogger m) =>
  -- | Original decision variables
  Set.Set Var ->
  [VarTransform] ->
  FeasibleSystem ->
  -- | (original, transformed) objective pairs
  [(ObjectiveFunction, ObjectiveFunction)] ->
  m [ObjectiveResult]
optimizeAllObjectives originalVars transforms feasibleSystem objPairs =
  mapM optimizeOne objPairs
  where
    optimizeOne (origObj, transformedObj) = do
      outcome <- optimizeFeasibleSystem transformedObj feasibleSystem
      let postprocessedOutcome = postprocess originalVars transforms outcome
      pure $ ObjectiveResult origObj postprocessedOutcome

-- | Postprocess the optimisation outcome by unapplying variable transformations
-- and filtering to only include the original decision variables.
-- For Optimal outcomes, unapplies transforms to get variable values in original space.
-- For Unbounded outcomes, passes through unchanged.
postprocess :: Set.Set Var -> [VarTransform] -> OptimisationOutcome -> OptimisationOutcome
postprocess _originalVars _transforms Unbounded = Unbounded
postprocess originalVars transforms (Optimal varVals) =
  let -- Unapply transforms to get variable values in original space
      unappliedVarVals = unapplyTransformsToVarMap transforms varVals
      -- Filter to only include original decision variables
      filteredVarVals = M.filterWithKey (\k _ -> Set.member k originalVars) unappliedVarVals
  in  Optimal filteredVarVals

-- | Compute the value of an objective function given variable values.
computeObjective :: ObjectiveFunction -> M.Map Var SimplexNum -> SimplexNum
computeObjective objFunction varVals =
  let coeffs = case objFunction of
        Max m -> m
        Min m -> m
  in  sum $ map (\(var, coeff) -> coeff * M.findWithDefault 0 var varVals) (M.toList coeffs)

-- | Preprocess the system by applying variable transformations based on domain information.
-- Returns the transformed objectives, constraints, and the list of transforms applied.
preprocess ::
  [ObjectiveFunction] ->
  VarDomainMap ->
  [PolyConstraint] ->
  ([ObjectiveFunction], [PolyConstraint], [VarTransform])
preprocess objFunctions (VarDomainMap domainMap) constraints =
  let -- Collect all variables in the system (from all objectives and constraints)
      allVars = collectAllVars objFunctions constraints
      -- Find the maximum variable to generate fresh variables
      maxVar = if Set.null allVars then 0 else Set.findMax allVars
      -- Generate transforms for each variable based on its domain
      -- Variables not in domainMap are treated as Unbounded
      (transforms, _) = foldr (generateTransform domainMap) ([], maxVar) (Set.toList allVars)
      -- Apply transforms to get the transformed system
      transformedObjs = map (\obj -> fst $ applyTransforms transforms obj constraints) objFunctions
      (_, transformedConstraints) = case objFunctions of
        [] -> (Max M.empty, applyTransformsToConstraints transforms constraints)
        (obj : _) -> applyTransforms transforms obj constraints
  in  (transformedObjs, transformedConstraints, transforms)

-- | Apply transforms to constraints only (used when there are no objectives)
applyTransformsToConstraints :: [VarTransform] -> [PolyConstraint] -> [PolyConstraint]
applyTransformsToConstraints transforms constraints =
  snd $ applyTransforms transforms (Max M.empty) constraints

-- | Collect all variables appearing in the objective functions and constraints
collectAllVars :: [ObjectiveFunction] -> [PolyConstraint] -> Set Var
collectAllVars objFunctions constraints =
  let objVars = Set.unions $ map getObjVars objFunctions
      constraintVars = Set.unions $ map getConstraintVars constraints
  in  Set.union objVars constraintVars
  where
    getObjVars :: ObjectiveFunction -> Set Var
    getObjVars (Max m) = M.keysSet m
    getObjVars (Min m) = M.keysSet m

    getConstraintVars :: PolyConstraint -> Set Var
    getConstraintVars (LEQ m _) = M.keysSet m
    getConstraintVars (GEQ m _) = M.keysSet m
    getConstraintVars (EQ m _) = M.keysSet m

-- | Generate a transform for a variable based on its domain.
-- Takes the domain map, the variable, and the current (transforms, nextFreshVar).
-- Returns updated (transforms, nextFreshVar).
generateTransform :: M.Map Var VarDomain -> Var -> ([VarTransform], Var) -> ([VarTransform], Var)
generateTransform domainMap var (transforms, nextFreshVar) =
  let domain = M.findWithDefault unbounded var domainMap
      (newTransforms, varOffset) = getTransform nextFreshVar var domain
  in  (newTransforms ++ transforms, nextFreshVar + varOffset)

-- | Determine what transforms are needed for a variable given its domain.
-- Returns a list of transforms and the number of fresh variables consumed.
getTransform :: Var -> Var -> VarDomain -> ([VarTransform], Var)
getTransform nextFreshVar var (Bounded mLower mUpper) =
  let -- Handle lower bound
      (lowerTransforms, varOffset) = case mLower of
        Nothing -> ([], 0) -- No lower bound: will need Split
        Just l
          | l == 0 -> ([], 0) -- NonNegative: no transform needed
          | l > 0 -> ([AddLowerBound var l], 0) -- Positive lower bound: add constraint
          | otherwise -> ([Shift var nextFreshVar l], 1) -- Negative lower bound: shift

      -- Handle upper bound (if present)
      upperTransforms = case mUpper of
        Nothing -> []
        Just u -> [AddUpperBound var u]

      -- If no lower bound (Nothing), we need Split transformation
      -- Split replaces the variable, so upper bound would apply to the original var
      -- which gets expressed as posVar - negVar
      (finalTransforms, finalOffset) = case mLower of
        Nothing ->
          -- Unbounded: split the variable
          -- Note: upperTransforms will still be added and will apply to the original variable
          -- expression (posVar - negVar) via the constraint system
          (Split var nextFreshVar (nextFreshVar + 1) : upperTransforms, 2)
        Just _ ->
          (lowerTransforms ++ upperTransforms, varOffset)
  in  (finalTransforms, finalOffset)

-- | Apply all transforms to the objective function and constraints.
applyTransforms :: [VarTransform] -> ObjectiveFunction -> [PolyConstraint] -> (ObjectiveFunction, [PolyConstraint])
applyTransforms transforms objFunction constraints =
  foldr applyTransform (objFunction, constraints) transforms

-- | Apply a single transform to the objective function and constraints.
applyTransform :: VarTransform -> (ObjectiveFunction, [PolyConstraint]) -> (ObjectiveFunction, [PolyConstraint])
applyTransform transform (objFunction, constraints) =
  case transform of
    -- AddLowerBound: Add a GEQ constraint for the variable
    AddLowerBound v bound ->
      (objFunction, GEQ (M.singleton v 1) bound : constraints)
    -- AddUpperBound: Add a LEQ constraint for the variable
    AddUpperBound v bound ->
      (objFunction, LEQ (M.singleton v 1) bound : constraints)
    -- Shift: originalVar = shiftedVar + shiftBy (where shiftBy < 0)
    -- Substitute: wherever we see originalVar, replace with shiftedVar
    -- and adjust the RHS by -coeff * shiftBy
    Shift origVar shiftedVar shiftBy ->
      ( applyShiftToObjective origVar shiftedVar shiftBy objFunction
      , map (applyShiftToConstraint origVar shiftedVar shiftBy) constraints
      )
    -- Split: originalVar = posVar - negVar
    -- Substitute: wherever we see originalVar with coeff c,
    -- replace with posVar with coeff c and negVar with coeff -c
    Split origVar posVar negVar ->
      ( applySplitToObjective origVar posVar negVar objFunction
      , map (applySplitToConstraint origVar posVar negVar) constraints
      )

-- | Apply shift transformation to objective function.
-- originalVar = shiftedVar + shiftBy
-- So coefficient of originalVar becomes coefficient of shiftedVar.
-- The constant term changes but objectives don't have constants that affect optimization.
applyShiftToObjective :: Var -> Var -> SimplexNum -> ObjectiveFunction -> ObjectiveFunction
applyShiftToObjective origVar shiftedVar _shiftBy objFunction =
  case objFunction of
    Max m -> Max (substituteVar origVar shiftedVar m)
    Min m -> Min (substituteVar origVar shiftedVar m)
  where
    substituteVar :: Var -> Var -> VarLitMapSum -> VarLitMapSum
    substituteVar oldVar newVar m =
      case M.lookup oldVar m of
        Nothing -> m
        Just coeff -> M.insert newVar coeff (M.delete oldVar m)

-- | Apply shift transformation to a constraint.
-- originalVar = shiftedVar + shiftBy
-- For constraint: sum(c_i * x_i) REL rhs
-- If x_j = originalVar with coeff c_j:
--   c_j * originalVar = c_j * (shiftedVar + shiftBy) = c_j * shiftedVar + c_j * shiftBy
-- So new constraint: (replace originalVar with shiftedVar) REL (rhs - c_j * shiftBy)
applyShiftToConstraint :: Var -> Var -> SimplexNum -> PolyConstraint -> PolyConstraint
applyShiftToConstraint origVar shiftedVar shiftBy constraint =
  case constraint of
    LEQ m rhs ->
      let (newMap, rhsAdjust) = substituteVarInMap origVar shiftedVar shiftBy m
      in  LEQ newMap (rhs - rhsAdjust)
    GEQ m rhs ->
      let (newMap, rhsAdjust) = substituteVarInMap origVar shiftedVar shiftBy m
      in  GEQ newMap (rhs - rhsAdjust)
    EQ m rhs ->
      let (newMap, rhsAdjust) = substituteVarInMap origVar shiftedVar shiftBy m
      in  EQ newMap (rhs - rhsAdjust)
  where
    substituteVarInMap :: Var -> Var -> SimplexNum -> VarLitMapSum -> (VarLitMapSum, SimplexNum)
    substituteVarInMap oldVar newVar shift m =
      case M.lookup oldVar m of
        Nothing -> (m, 0)
        Just coeff -> (M.insert newVar coeff (M.delete oldVar m), coeff * shift)

-- | Apply split transformation to objective function.
-- originalVar = posVar - negVar
-- coefficient c of originalVar becomes c for posVar and -c for negVar
applySplitToObjective :: Var -> Var -> Var -> ObjectiveFunction -> ObjectiveFunction
applySplitToObjective origVar posVar negVar objFunction =
  case objFunction of
    Max m -> Max (splitVar origVar posVar negVar m)
    Min m -> Min (splitVar origVar posVar negVar m)
  where
    splitVar :: Var -> Var -> Var -> VarLitMapSum -> VarLitMapSum
    splitVar oldVar pVar nVar m =
      case M.lookup oldVar m of
        Nothing -> m
        Just coeff -> M.insert pVar coeff (M.insert nVar (-coeff) (M.delete oldVar m))

-- | Apply split transformation to a constraint.
-- originalVar = posVar - negVar
-- coefficient c of originalVar becomes c for posVar and -c for negVar
applySplitToConstraint :: Var -> Var -> Var -> PolyConstraint -> PolyConstraint
applySplitToConstraint origVar posVar negVar constraint =
  case constraint of
    LEQ m rhs -> LEQ (splitVarInMap origVar posVar negVar m) rhs
    GEQ m rhs -> GEQ (splitVarInMap origVar posVar negVar m) rhs
    EQ m rhs -> EQ (splitVarInMap origVar posVar negVar m) rhs
  where
    splitVarInMap :: Var -> Var -> Var -> VarLitMapSum -> VarLitMapSum
    splitVarInMap oldVar pVar nVar m =
      case M.lookup oldVar m of
        Nothing -> m
        Just coeff -> M.insert pVar coeff (M.insert nVar (-coeff) (M.delete oldVar m))

-- | Unapply transforms to convert a variable value map back to original variables.
unapplyTransformsToVarMap :: [VarTransform] -> VarLitMap -> VarLitMap
unapplyTransformsToVarMap transforms valMap =
  -- Apply transforms in reverse order (since we applied them with foldr)
  foldl (flip unapplyTransformToVarMap) valMap transforms

-- | Unapply a single transform to convert a variable value map back to original variables.
unapplyTransformToVarMap :: VarTransform -> VarLitMap -> VarLitMap
unapplyTransformToVarMap transform valMap =
  case transform of
    -- AddLowerBound: No variable substitution was done, nothing to unapply
    AddLowerBound {} -> valMap
    -- AddUpperBound: No variable substitution was done, nothing to unapply
    AddUpperBound {} -> valMap
    -- Shift: originalVar = shiftedVar + shiftBy
    -- So originalVar's value = shiftedVar's value + shiftBy
    Shift origVar shiftedVar shiftBy ->
      let shiftedVal = M.findWithDefault 0 shiftedVar valMap
          origVal = shiftedVal + shiftBy
      in  M.insert origVar origVal (M.delete shiftedVar valMap)
    -- Split: originalVar = posVar - negVar
    -- So originalVar's value = posVar's value - negVar's value
    Split origVar posVar negVar ->
      let posVal = M.findWithDefault 0 posVar valMap
          negVal = M.findWithDefault 0 negVar valMap
          origVal = posVal - negVal
      in  M.insert origVar origVal (M.delete posVar (M.delete negVar valMap))

-- | Perform the simplex pivot algorithm on a system with basic vars, assume that the first row is the 'ObjectiveFunction'.
simplexPivot :: (MonadIO m, MonadLogger m) => PivotObjective -> Dict -> m (Maybe Dict)
simplexPivot objective@(PivotObjective {variable = objectiveVar, function = objectiveFunc, constant = objectiveConstant}) dictionary = do
  logMsg LevelInfo $
    "simplexPivot: Pivoting with objective " <> showT objective <> " over system (in Dict form) " <> showT dictionary
  case mostPositive objectiveFunc of
    Nothing -> do
      logMsg LevelInfo $
        "simplexPivot: Pivoting complete as no positive variables found in objective "
          <> showT objective
          <> " over system (in Dict form) "
          <> showT dictionary
      pure $ Just (insertPivotObjectiveToDict objective dictionary)
    Just pivotNonBasicVar -> do
      logMsg LevelInfo $
        "simplexPivot: Non-basic pivoting variable in objective, determined by largest coefficient = " <> showT pivotNonBasicVar
      let mPivotBasicVar = ratioTest dictionary pivotNonBasicVar Nothing Nothing
      case mPivotBasicVar of
        Nothing -> do
          logMsg LevelInfo $
            "simplexPivot: Ratio test failed with non-basic variable "
              <> showT pivotNonBasicVar
              <> " over system (in Dict form) "
              <> showT dictionary
          pure Nothing
        Just pivotBasicVar -> do
          logMsg LevelInfo $ "simplexPivot: Basic pivoting variable determined by ratio test " <> showT pivotBasicVar
          logMsg LevelInfo $
            "simplexPivot: Pivoting with basic var "
              <> showT pivotBasicVar
              <> ", non-basic var "
              <> showT pivotNonBasicVar
              <> ", objective "
              <> showT objective
              <> " over system (in Dict form) "
              <> showT dictionary
          let pivotResult = pivot pivotBasicVar pivotNonBasicVar (insertPivotObjectiveToDict objective dictionary)
              pivotedObj =
                let pivotedObjEntry = fromMaybe (error "simplexPivot: Can't find objective after pivoting") $ M.lookup objectiveVar pivotResult
                in  objective & #function .~ pivotedObjEntry.varMapSum & #constant .~ pivotedObjEntry.constant
              pivotedDict = M.delete objectiveVar pivotResult
          logMsg LevelInfo $
            "simplexPivot: Pivoted, Recursing with new pivoting objective "
              <> showT pivotedObj
              <> " for new pivoted system (in Dict form) "
              <> showT pivotedDict
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
