-- | Shared helpers for the solver specs: run a solve with logging switched off
-- and assert on the single-objective result shapes that most tests expect.
module Linear.Simplex.Solver.SpecSupport
  ( expectInfeasible
  , expectOptimal
  , expectOptimalValues
  , expectOutcomes
  , expectUnbounded
  , solveNonNeg
  , solveWith
  ) where

import Control.Monad.Logger (runNoLoggingT)
import qualified Data.Map as M

import Test.Hspec (Expectation, expectationFailure, shouldBe)

import Linear.Simplex.Solver.TwoPhase (collectAllVars, computeObjective, twoPhaseSimplex)
import Linear.Simplex.Types
  ( ObjectiveFunction
  , ObjectiveResult (..)
  , OptimisationOutcome (..)
  , PolyConstraint
  , SimplexNum
  , SimplexResult (..)
  , VarDomainMap (..)
  , VarLitMap
  , nonNegative
  )

-- | Solve with every variable non-negative, the domain the textbooks use.
solveNonNeg :: [ObjectiveFunction] -> [PolyConstraint] -> IO SimplexResult
solveNonNeg objectives constraints =
  solveWith (VarDomainMap $ M.fromSet (const nonNegative) (collectAllVars objectives constraints)) objectives constraints

-- | Solve with an explicit domain map, without emitting any log output.
solveWith :: VarDomainMap -> [ObjectiveFunction] -> [PolyConstraint] -> IO SimplexResult
solveWith domainMap objectives constraints =
  runNoLoggingT $ twoPhaseSimplex domainMap objectives constraints

-- | Assert the run produced exactly one optimal objective result whose variable
-- map and objective value are the expected ones.
expectOptimal :: ObjectiveFunction -> VarLitMap -> SimplexNum -> SimplexResult -> Expectation
expectOptimal obj expectedValues expectedObjectiveValue result =
  expectOptimalValues result $ \values -> do
    values `shouldBe` expectedValues
    computeObjective obj values `shouldBe` expectedObjectiveValue

-- | Assert the run produced exactly one optimal objective result, then apply the
-- test's own checks to the variable values it reports.
expectOptimalValues :: SimplexResult -> (VarLitMap -> Expectation) -> Expectation
expectOptimalValues result check =
  case result of
    SimplexResult (Just _) [ObjectiveResult _ (Optimal values)] -> check values
    _ -> expectationFailure $ "Expected a single optimal objective result, got " ++ show result

-- | Assert the single objective came back unbounded.
expectUnbounded :: SimplexResult -> Expectation
expectUnbounded result =
  case result of
    SimplexResult (Just _) [ObjectiveResult _ Unbounded] -> pure ()
    _ -> expectationFailure $ "Expected a single unbounded objective result, got " ++ show result

-- | Assert phase one found no feasible system.
expectInfeasible :: SimplexResult -> Expectation
expectInfeasible result =
  case result of
    SimplexResult Nothing _ -> pure ()
    _ -> expectationFailure $ "Expected an infeasible system, got " ++ show result

-- | Assert a feasible system with one outcome per check, in objective order.
expectOutcomes :: [OptimisationOutcome -> Expectation] -> SimplexResult -> Expectation
expectOutcomes checks (SimplexResult (Just _) results)
  | length results == length checks =
      sequence_ $ zipWith (\objectiveResult check -> check objectiveResult.outcome) results checks
expectOutcomes checks result =
  expectationFailure $ "Expected " ++ show (length checks) ++ " objective outcomes, got " ++ show result
