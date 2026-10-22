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
  , findFeasibleSolutionReserving
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
  , shiftVarInMap
  , splitVarInMap
  , unapplyTransformsToVarMap
  , unapplyTransformToVarMap
  ) where

import Prelude hiding (EQ)

import Control.Lens ((%~), (&), (.~), (<&>))
import Control.Monad (unless)
import Control.Monad.Logger (LogLevel (LevelDebug, LevelError, LevelInfo), MonadLogger)
import Data.Bifunctor (second)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Ratio (denominator, numerator, (%))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import GHC.Real (Ratio)
import Linear.Simplex.Prettify
  ( prettyShowDict
  , prettyShowObjectiveFunction
  , prettyShowRational
  )
import Linear.Simplex.Types
  ( Dict
  , DictValue (..)
  , FeasibleSystem (..)
  , ObjectiveFunction (..)
  , ObjectiveResult (..)
  , OptimisationOutcome (..)
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
  , foldVarLitMap
  , isMax
  , logMsg
  , showT
  , simplifySystem
  , tableauInDictionaryForm
  )

-- | Find a feasible solution for the given system of 'PolyConstraint's.
--  No variable ids are reserved beyond those appearing in the constraints.
findFeasibleSolution :: (MonadLogger m) => [PolyConstraint] -> m (Maybe FeasibleSystem)
findFeasibleSolution = findFeasibleSolutionReserving Set.empty

-- | Is the given 'PolyConstraint' constant, i.e. does it involve no variable at all?
--  A constant constraint has only zero coefficients on its left-hand side (an
--  empty left-hand side included), so it is either a tautology like 0 <= 1 or a
--  contradiction like 0 <= -1.
isConstantConstraint :: PolyConstraint -> Bool
isConstantConstraint constraint = all (== 0) (M.elems constraint.lhs)

-- | Can the given constant constraint never hold?
isContradiction :: PolyConstraint -> Bool
isContradiction (LEQ _ r) = 0 > r
isContradiction (GEQ _ r) = 0 < r
isContradiction (EQ _ r) = 0 /= r

-- | The constant constraints of the given system that cannot hold for any
--  variable assignment.
contradictingConstantConstraints :: [PolyConstraint] -> [PolyConstraint]
contradictingConstantConstraints =
  filter isContradiction . filter isConstantConstraint . simplifySystem

-- | Find a feasible solution for the given system of 'PolyConstraint's by performing the first phase of the two-phase simplex method
--  All variables in the 'PolyConstraint' must be positive.
--  Slack and artificial variables are allocated above every variable in the given reserved set as well as every variable
--  in the constraints, so a variable that only appears in an objective function never shares an id with them.
--  If the system is infeasible, return 'Nothing'
--  Otherwise, return the feasible system in 'Dict' as well as a list of slack variables, a list artificial variables, and the objective variable.
findFeasibleSolutionReserving :: (MonadLogger m) => Set Var -> [PolyConstraint] -> m (Maybe FeasibleSystem)
findFeasibleSolutionReserving reservedVars unsimplifiedSystem = case contradictingConstantConstraints unsimplifiedSystem of
  contradicting : _ -> do
    logMsg LevelInfo $
      "findFeasibleSolution: Constant constraint "
        <> showT contradicting
        <> " cannot hold for any variable assignment, thus original system is infeasible"
    pure Nothing
  [] -> runPhaseOne reservedVars unsimplifiedSystem

-- | Perform phase one for a system whose constant constraints have been decided
--  by 'findFeasibleSolutionReserving'. Slack and artificial ids are allocated above
--  every reserved id and every constraint variable.
runPhaseOne :: (MonadLogger m) => Set Var -> [PolyConstraint] -> m (Maybe FeasibleSystem)
runPhaseOne reservedVars unsimplifiedSystem = do
  logMsg LevelInfo $
    "findFeasibleSolution: Looking for a solution for "
      <> showT (length variableConstraints)
      <> " constraints"
  logMsg LevelDebug $ "findFeasibleSolution: Constraints " <> showT variableConstraints
  if null artificialVars -- No artificial vars, we have a feasible system
    then do
      logMsg LevelInfo "findFeasibleSolution: Feasible solution found with no artificial vars"
      pure . Just $ FeasibleSystem systemWithBasicVarsAsDictionary slackVars artificialVars objectiveVar
    else do
      logMsg LevelInfo $
        "findFeasibleSolution: Needed to create "
          <> showT (length artificialVars)
          <> " artificial vars for "
          <> showT (length systemWithBasicVars)
          <> " rows"
      logMsg LevelDebug $ "findFeasibleSolution: System in tableau form " <> showT systemWithBasicVars
      -- The phase-one objective is an ordinary row of the dictionary, inserted
      -- once here so 'simplexPivot' never has to take it back out and put it
      -- back in on every iteration.
      let phase1System = M.insert objectiveVar artificialObjectiveRow systemWithBasicVarsAsDictionary
      mPhase1Dict <- simplexPivot 1 objectiveVar phase1System
      case mPhase1Dict of
        Just phase1Dict -> do
          logMsg LevelDebug $
            "findFeasibleSolution: System after pivoting (in Dict form, objective row included):\n"
              <> Text.pack (prettyShowDict phase1Dict)
          let eliminateArtificialVarsFromPhase1Tableau =
                M.map
                  ( \DictValue {..} ->
                      DictValue
                        { varMapSum = M.withoutKeys varMapSum artificialVarSet
                        , ..
                        }
                  )
                  phase1Dict
          -- 'simplexPivot' returns the objective row it was given, and the 'M.map'
          -- above preserves keys, so the objective row is guaranteed to be present here.
          let row =
                fromMaybe
                  (error "findFeasibleSolution: objective row missing after phase one")
                  (M.lookup objectiveVar eliminateArtificialVarsFromPhase1Tableau)
          if row.constant == 0
            then do
              logMsg LevelInfo $
                "findFeasibleSolution: Objective RHS is zero after pivoting, thus original tableau is feasible"
              logMsg LevelDebug $
                "findFeasibleSolution: Feasible system (in Dict form):\n"
                  <> Text.pack (prettyShowDict eliminateArtificialVarsFromPhase1Tableau)
              pure . Just $
                FeasibleSystem
                  { dict = removeArtificialBasics phase1Dict
                  , slackVars = slackVars
                  , artificialVars = artificialVars
                  , objectiveVar = objectiveVar
                  }
            else do
              unless (row.constant < 0) $ do
                let errMsg =
                      "findFeasibleSolution: Objective RHS is positive after pivoting. This should be impossible. System without artificial vars (in Dict form) "
                        <> show eliminateArtificialVarsFromPhase1Tableau
                logMsg LevelError $ Text.pack errMsg
                error errMsg
              logMsg LevelInfo $
                "findFeasibleSolution: Objective RHS not zero after phase 1, thus original tableau is infeasible"
              logMsg LevelDebug $
                "findFeasibleSolution: System without artificial vars (in Dict form):\n"
                  <> Text.pack (prettyShowDict eliminateArtificialVarsFromPhase1Tableau)
              pure Nothing
        Nothing -> do
          logMsg LevelInfo $
            "findFeasibleSolution: Infeasible solution found, could not pivot with objective row x"
              <> showT objectiveVar
          logMsg LevelDebug $
            "findFeasibleSolution: System that could not be pivoted (in Dict form):\n"
              <> Text.pack (prettyShowDict phase1System)
          pure Nothing
  where
    -- Called only after the phase-one optimum has been verified to be zero.
    -- A remaining artificial basic must stay zero in the original problem.
    -- Pivot it out on any nonzero non-artificial coefficient, of either sign:
    -- its zero constant makes the pivot degenerate and preserves feasibility.
    -- A row with no such coefficient is redundant once artificials are zero.
    removeArtificialBasics :: Dict -> Dict
    removeArtificialBasics dictionary =
      M.map (\row -> row & #varMapSum %~ (`M.withoutKeys` artificialVarSet)) $
        foldl removeBasic dictionary artificialVars
      where
        removeBasic dict artificialVar =
          case M.lookup artificialVar dict of
            Nothing -> dict
            Just row
              | row.constant /= 0 -> error "removeArtificialBasics: artificial basic variable is nonzero"
              | otherwise ->
                  case M.lookupMin (M.filter (/= 0) (M.withoutKeys row.varMapSum artificialVarSet)) of
                    Nothing -> M.delete artificialVar dict
                    Just (enteringVar, _) -> pivot artificialVar enteringVar dict

    system = simplifySystem unsimplifiedSystem

    -- Constant tautologies are dropped: they constrain nothing, and keeping
    -- them would leave slack variables for rows without any variable.
    variableConstraints :: [PolyConstraint]
    variableConstraints = filter (not . isConstantConstraint) system

    -- Slack and artificial variables are allocated from 'maxVar' upwards, so 'maxVar' must be at least
    -- every variable id in the system, including the reserved ids that may only occur in an objective.
    maxVar = fromMaybe 0 $ Set.lookupMax $ reservedVars <> collectAllVars [] variableConstraints

    (systemWithSlackVars, slackVars) = systemInStandardForm variableConstraints maxVar []

    maxVarWithSlackVars = if null slackVars then maxVar else maximum slackVars

    (systemWithBasicVars, artificialVars) = systemWithArtificialVars systemWithSlackVars maxVarWithSlackVars

    artificialVarSet = Set.fromList artificialVars

    finalMaxVar = if null artificialVars then maxVarWithSlackVars else maximum artificialVars

    systemWithBasicVarsAsDictionary = tableauInDictionaryForm systemWithBasicVars

    artificialObjectiveRow = createArtificialObjectiveRow systemWithBasicVarsAsDictionary artificialVars

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

    -- \| Takes a 'Dict' and a '[Var]' as input and returns the phase-one
    -- objective row, to be inserted into that 'Dict' under 'objectiveVar'.
    -- The 'Dict' represents the tableau of a linear program with artificial
    -- variables, and '[Var]' represents the artificial variables.

    -- The function first filters out the rows of the tableau that correspond
    -- to the artificial variables, and negates them. It then computes the sum
    -- of the negated rows, which is the objective row.
    createArtificialObjectiveRow :: Dict -> [Var] -> DictValue
    createArtificialObjectiveRow rows artificialVars =
      DictValue
        { varMapSum = foldVarLitMap $ map (.varMapSum) negatedRowsWithoutArtificialVars
        , constant = sum $ map (.constant) negatedRowsWithoutArtificialVars
        }
      where
        -- Filter out non-artificial entries
        artificialVarSet = Set.fromList artificialVars
        rowsToAdd = M.restrictKeys rows artificialVarSet
        -- Negate rows, discard keys and artificial vars since the pivot objective does not care about them
        negatedRowsWithoutArtificialVars =
          map
            ( \(_, DictValue {..}) ->
                DictValue
                  { varMapSum = M.map negate $ M.withoutKeys varMapSum artificialVarSet
                  , constant = negate constant
                  }
            )
            $ M.toList rowsToAdd

-- | Optimize a feasible system by performing the second phase of the two-phase simplex method.
--  We first pass an 'ObjectiveFunction'.
--  Then, the feasible system in 'Dict' form as well as a list of slack variables, a list artificial variables, and the objective variable.
--  Returns 'Optimal' with variable values if an optimal solution is found, or 'Unbounded' if the objective is unbounded.
optimizeFeasibleSystem :: (MonadLogger m) => ObjectiveFunction -> FeasibleSystem -> m OptimisationOutcome
optimizeFeasibleSystem objFunction (FeasibleSystem {dict = phase1Dict, ..}) = do
  logMsg LevelInfo $
    "optimizeFeasibleSystem: Optimizing with objective " <> Text.pack (prettyShowObjectiveFunction objFunction)
  logMsg LevelDebug $ "optimizeFeasibleSystem: Feasible system (in Dict form):\n" <> Text.pack (prettyShowDict phase1Dict)
  let phase2System =
        M.insert
          objectiveVar
          (if null artificialVars then normalObjectiveRow else adjustedObjectiveRow)
          phase1Dict
  mResult <-
    if null artificialVars
      then do
        logMsg LevelInfo "optimizeFeasibleSystem: No artificial vars, system is feasible"
        logMsg LevelDebug "optimizeFeasibleSystem: Pivoting with the original objective row"
        simplexPivot 1 objectiveVar phase2System
      else do
        logMsg LevelInfo "optimizeFeasibleSystem: Artificial vars present"
        logMsg LevelDebug "optimizeFeasibleSystem: Pivoting with the adjusted objective row"
        simplexPivot 1 objectiveVar phase2System
  case mResult of
    Nothing -> do
      logMsg LevelInfo "optimizeFeasibleSystem: Objective is unbounded (ratio test failed)"
      pure Unbounded
    Just resultDict -> do
      let result = displayResults resultDict
      logMsg LevelInfo $ "optimizeFeasibleSystem: Found optimal solution: " <> showT result
      pure result
  where
    -- Extract basic-variable values directly from the final dictionary.
    -- Omit slack/artificial variables and restore the minimization objective sign.
    displayResults :: Dict -> OptimisationOutcome
    displayResults resultDict =
      Optimal $ M.mapWithKey valueOf originalRows
      where
        originalRows = M.filterWithKey (\var _ -> var `notElem` slackVars ++ artificialVars) resultDict
        valueOf var row =
          if not (isMax objFunction) && var == objectiveVar
            then negate row.constant
            else row.constant

    -- \| Objective to use when optimising the linear program if no artificial
    -- variables were necessary in the first phase. It is essentially the original
    -- objective function, with a potential change of sign based on the type of
    -- problem (Maximization or Minimization).
    normalObjectiveRow :: DictValue
    normalObjectiveRow =
      DictValue
        { varMapSum = if isMax objFunction then objFunction.objective else M.map negate objFunction.objective
        , constant = 0
        }

    -- \| Objective to use when optimising the linear program if artificial
    -- variables were necessary in the first phase. It is an adjustment to the
    -- original objective function, where the linear coefficients are modified
    -- by back-substitution of the values of the artificial variables.
    adjustedObjectiveRow :: DictValue
    adjustedObjectiveRow =
      DictValue
        { varMapSum = calcVarMap
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
  (MonadLogger m) => VarDomainMap -> [ObjectiveFunction] -> [PolyConstraint] -> m SimplexResult
twoPhaseSimplex domainMap objFunctions constraints = do
  logMsg LevelInfo $
    "twoPhaseSimplex: Solving system with "
      <> showT (length objFunctions)
      <> " objectives and "
      <> showT (length constraints)
      <> " constraints"
  logMsg LevelDebug $
    "twoPhaseSimplex: Solving system with domain map " <> showT domainMap
  -- Collect original variables before any transformations
  let originalVars = collectAllVars objFunctions constraints
  let (transformedObjs, transformedConstraints, transforms) = preprocess objFunctions domainMap constraints
  logMsg LevelInfo $
    "twoPhaseSimplex: Applied " <> showT (length transforms) <> " variable transforms"
  logMsg LevelDebug $
    "twoPhaseSimplex: Applied transforms "
      <> showT transforms
      <> "; Transformed objectives: "
      <> showT transformedObjs
      <> "; Transformed constraints: "
      <> showT transformedConstraints
  -- Reserve every variable that appears in an objective, including variables that
  -- appear in no constraint and variables introduced by the transforms, so the
  -- slack and artificial ids allocated in phase one cannot collide with them.
  let objectiveVars = Set.unions $ map (M.keysSet . (.objective)) transformedObjs
  mFeasibleSystem <- findFeasibleSolutionReserving objectiveVars transformedConstraints
  case mFeasibleSystem of
    Nothing -> do
      logMsg LevelInfo "twoPhaseSimplex: No feasible solution found in phase 1"
      pure $ SimplexResult Nothing []
    Just feasibleSystem -> do
      logMsg LevelInfo "twoPhaseSimplex: Feasible system found for transformed system"
      logMsg LevelDebug $
        "twoPhaseSimplex: Feasible system (in Dict form):\n"
          <> Text.pack (prettyShowDict feasibleSystem.dict)
      objResults <- optimizeAllObjectives originalVars transforms feasibleSystem (zip objFunctions transformedObjs)
      logMsg LevelDebug $ "twoPhaseSimplex: All objective results: " <> showT objResults
      pure $ SimplexResult (Just feasibleSystem) objResults

-- | Optimize all objective functions over the given feasible system.
-- Returns a list of ObjectiveResult, one for each objective function.
-- The originalVars set is used to filter the result to only include original decision variables.
optimizeAllObjectives ::
  (MonadLogger m) =>
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
      filteredVarVals = M.restrictKeys unappliedVarVals originalVars
  in  Optimal filteredVarVals

-- | Compute the value of an objective function given variable values.
computeObjective :: ObjectiveFunction -> M.Map Var SimplexNum -> SimplexNum
computeObjective objFunction varVals =
  sum $ map (\(var, coeff) -> coeff * M.findWithDefault 0 var varVals) (M.toList objFunction.objective)

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
      (transforms, _) = foldr (generateTransform domainMap) ([], maxVar + 1) (Set.toList allVars)
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
  Set.unions $
    map (M.keysSet . (.objective)) objFunctions
      ++ map (M.keysSet . (.lhs)) constraints

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
      -- Any nonzero lower bound is handled by a shift, so the bound becomes the
      -- implicit shiftedVar >= 0: an upper bound then stays a LEQ row, whose
      -- slack is feasible at the origin, and no artificial variable is needed.
      (lowerTransforms, varOffset) = case mLower of
        Nothing -> ([], 0) -- No lower bound: will need Split
        Just l
          | l == 0 -> ([], 0) -- NonNegative: no transform needed
          | otherwise -> ([Shift var nextFreshVar l], 1) -- Nonzero lower bound: shift

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
    -- AddUpperBound: Add a LEQ constraint for the variable
    AddUpperBound v bound ->
      (objFunction, LEQ (M.singleton v 1) bound : constraints)
    -- Shift: originalVar = shiftedVar + shiftBy (where shiftBy /= 0)
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
  objFunction {objective = fst $ shiftVarInMap origVar shiftedVar 0 objFunction.objective}

-- | Apply shift transformation to a constraint.
-- originalVar = shiftedVar + shiftBy
-- For constraint: sum(c_i * x_i) REL rhs
-- If x_j = originalVar with coeff c_j:
--   c_j * originalVar = c_j * (shiftedVar + shiftBy) = c_j * shiftedVar + c_j * shiftBy
-- So new constraint: (replace originalVar with shiftedVar) REL (rhs - c_j * shiftBy)
applyShiftToConstraint :: Var -> Var -> SimplexNum -> PolyConstraint -> PolyConstraint
applyShiftToConstraint origVar shiftedVar shiftBy constraint =
  let (newMap, rhsAdjust) = shiftVarInMap origVar shiftedVar shiftBy constraint.lhs
  in  constraint & #lhs .~ newMap & #rhs %~ subtract rhsAdjust

-- | Apply split transformation to objective function.
-- originalVar = posVar - negVar
-- coefficient c of originalVar becomes c for posVar and -c for negVar
applySplitToObjective :: Var -> Var -> Var -> ObjectiveFunction -> ObjectiveFunction
applySplitToObjective origVar posVar negVar objFunction =
  objFunction {objective = splitVarInMap origVar posVar negVar objFunction.objective}

-- | Apply split transformation to a constraint.
-- originalVar = posVar - negVar
-- coefficient c of originalVar becomes c for posVar and -c for negVar
applySplitToConstraint :: Var -> Var -> Var -> PolyConstraint -> PolyConstraint
applySplitToConstraint origVar posVar negVar constraint =
  constraint & #lhs %~ splitVarInMap origVar posVar negVar

-- | Substitute a shifted variable and return the constant offset introduced.
-- The replacement variable must be fresh.
shiftVarInMap :: Var -> Var -> SimplexNum -> VarLitMapSum -> (VarLitMapSum, SimplexNum)
shiftVarInMap oldVar newVar shift coeffs =
  case M.lookup oldVar coeffs of
    Nothing -> (coeffs, 0)
    Just coeff -> (M.insert newVar coeff (M.delete oldVar coeffs), coeff * shift)

-- | Substitute oldVar = posVar - negVar in a coefficient map.
-- The replacement variables must be distinct and fresh.
splitVarInMap :: Var -> Var -> Var -> VarLitMapSum -> VarLitMapSum
splitVarInMap oldVar posVar negVar coeffs =
  case M.lookup oldVar coeffs of
    Nothing -> coeffs
    Just coeff -> M.insert posVar coeff (M.insert negVar (-coeff) (M.delete oldVar coeffs))

-- | Unapply transforms to convert a variable value map back to original variables.
unapplyTransformsToVarMap :: [VarTransform] -> VarLitMap -> VarLitMap
unapplyTransformsToVarMap transforms valMap =
  -- Apply transforms in reverse order (since we applied them with foldr)
  foldl (flip unapplyTransformToVarMap) valMap transforms

-- | Unapply a single transform to convert a variable value map back to original variables.
unapplyTransformToVarMap :: VarTransform -> VarLitMap -> VarLitMap
unapplyTransformToVarMap transform valMap =
  case transform of
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

-- | Pivot a feasible dictionary using Bland's anti-cycling rule in both phases.
-- Choose the least-index improving non-basic variable, then the least-index
-- basic variable among minimum-ratio ties. The fixed 'Var' ordering and exact
-- rational arithmetic ensure degenerate pivots cannot cycle. The objective row
-- is not a nonnegative basic variable and must not constrain the ratio test.
--
-- The first 'Var' is the objective variable. Its row must already be in the
-- dictionary: the objective is an ordinary row that is read, pivoted and
-- returned like any other, so the caller inserts it once before pivoting.
--
-- The 'Int' is the pivot number. Callers start it at 1 and each recursive pivot
-- increments it, so a trace can be followed by pivot instead of by wall-clock
-- time. 'LevelInfo' gets exactly one line per pivot; the objective constant on
-- that line is the value the pivot starts from.
simplexPivot :: (MonadLogger m) => Int -> Var -> Dict -> m (Maybe Dict)
simplexPivot pivotNumber objectiveVar dictionary = do
  logMsg LevelDebug $
    "simplexPivot: pivot "
      <> showT pivotNumber
      <> " starting from system (in Dict form, objective row included):\n"
      <> Text.pack (prettyShowDict dictionary)
  case M.lookupMin (M.filter (> 0) objectiveFunc) of
    Nothing -> do
      logMsg LevelInfo $
        "simplexPivot: pivot "
          <> showT pivotNumber
          <> ": no positive objective coefficient left, optimal with objective constant "
          <> Text.pack (prettyShowRational objectiveConstant)
      pure $ Just dictionary
    Just (pivotNonBasicVar, _) ->
      case ratioTest dictionary pivotNonBasicVar of
        Nothing -> do
          logMsg LevelInfo $
            "simplexPivot: pivot "
              <> showT pivotNumber
              <> ": entering "
              <> showT pivotNonBasicVar
              <> ", ratio test failed, objective is unbounded"
          pure Nothing
        Just pivotBasicVar -> do
          logMsg LevelInfo $
            "simplexPivot: pivot "
              <> showT pivotNumber
              <> ": entering "
              <> showT pivotNonBasicVar
              <> ", leaving "
              <> showT pivotBasicVar
              <> ", objective constant "
              <> Text.pack (prettyShowRational objectiveConstant)
          let pivotedDict = pivot pivotBasicVar pivotNonBasicVar dictionary
          logMsg LevelDebug $
            "simplexPivot: pivot "
              <> showT (pivotNumber + 1)
              <> " continuing from pivoted system (in Dict form, objective row included):\n"
              <> Text.pack (prettyShowDict pivotedDict)
          simplexPivot
            (pivotNumber + 1)
            objectiveVar
            pivotedDict
  where
    objectiveRow =
      fromMaybe
        (error "simplexPivot: objective row missing from the dictionary")
        (M.lookup objectiveVar dictionary)

    objectiveFunc = objectiveRow.varMapSum

    objectiveConstant = objectiveRow.constant

    ratioTest :: Dict -> Var -> Maybe Var
    ratioTest dict enteringVar =
      case ratios of
        [] -> Nothing
        -- Pair ordering first minimizes the step, then the leaving variable.
        _ -> Just (snd (minimum ratios))
      where
        ratios =
          [ (row.constant / negate coeff, basicVar)
          | (basicVar, row) <- M.toList dict
          , basicVar /= objectiveVar
          , Just coeff <- [M.lookup enteringVar row.varMapSum]
          , coeff < 0
          , row.constant >= 0
          ]

-- Pivot a dictionary using the two given variables.
-- The first variable is the leaving basic variable.
-- The second variable is the entering non-basic variable.
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
            divideByNegatedEnteringVariableCoeff = (/ negate enteringVariableCoeff)

        -- Substitute pivot equation into other rows
        updatedRows :: Dict
        updatedRows =
          M.fromList $ map (uncurry updateRow) $ M.toList dict
          where
            updateRow :: Var -> DictValue -> (Var, DictValue)
            updateRow entryVar entryVal =
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

    filterOutEnteringVarTerm = M.delete enteringVariable
