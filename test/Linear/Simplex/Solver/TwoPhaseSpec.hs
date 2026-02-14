{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Linear.Simplex.Solver.TwoPhaseSpec where

import Prelude hiding (EQ)

import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Maybe (isJust)
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Ratio

import Text.InterpolatedString.Perl6

import Test.Hspec
import Test.Hspec.Expectations.Contrib (annotate)
import Test.QuickCheck

import Linear.Simplex.Prettify
import Linear.Simplex.Solver.TwoPhase
import Linear.Simplex.Types
import Linear.Simplex.Util

-- | Compute the objective value from variable assignments.
-- For Max: sum of (coeff * varValue) for each variable
-- For Min: same calculation (the value represents the optimal objective value)
computeObjValue :: ObjectiveFunction -> VarLitMap -> SimplexNum
computeObjValue (Max coeffs) varMap = sum [c * M.findWithDefault 0 v varMap | (v, c) <- M.toList coeffs]
computeObjValue (Min coeffs) varMap = sum [c * M.findWithDefault 0 v varMap | (v, c) <- M.toList coeffs]

-- | Expected result for a single objective optimization
data ExpectedResult 
  = ExpectInfeasible
  -- ^ System has no feasible solution
  | ExpectUnbounded
  -- ^ System is feasible but unbounded (no finite optimum)
  | ExpectOptimal (Maybe SimplexNum) VarLitMap
  -- ^ Optimal solution found with optional expected objective value and variable values
  deriving (Show, Eq)

-- | Helper to run a test case for a system where all vars
-- are non-negative and verify we get the expected result.
runTest :: (ObjectiveFunction, [PolyConstraint]) -> ExpectedResult -> IO ()
runTest (obj, constraints) expectedResult = do
  let prettyObj = prettyShowObjectiveFunction obj
      prettyConstraints = map prettyShowPolyConstraint constraints
      allVars = collectAllVars [obj] constraints
      domainMap = VarDomainMap $ M.fromSet (const nonNegative) allVars
  SimplexResult mFeasibleSystem objResults <-
    runStdoutLoggingT $
    filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
      twoPhaseSimplex domainMap [obj] constraints
  let actualResult = case (mFeasibleSystem, objResults) of
        (Nothing, _) -> ExpectInfeasible
        (Just _, []) -> ExpectInfeasible  -- Should not happen with one objective
        (Just _, [ObjectiveResult _ Unbounded]) -> ExpectUnbounded
        (Just _, [ObjectiveResult _ (Optimal varVals)]) -> ExpectOptimal Nothing varVals
        (Just _, _) -> error "Unexpected: multiple results for single objective"
      actualObjVal = case actualResult of
        ExpectOptimal _ varVals -> extractObjectiveValue obj (ObjectiveResult obj (Optimal varVals))
        _ -> Nothing
      expectedObjVal = case expectedResult of
        ExpectOptimal (Just ov) _ -> Just ov
        ExpectOptimal Nothing varVals -> extractObjectiveValue obj (ObjectiveResult obj (Optimal varVals))
        _ -> Nothing
  annotate
    [qc|

Objective Function (Non-prettified): {obj}
Constraints        (Non-prettified): {constraints}
====================================
Objective Function     (Prettified): {prettyObj}
Constraints            (Prettified): {prettyConstraints}
====================================
Expected Result                    : {expectedResult}
Actual Result                      : {actualResult}
Expected Objective Value           : {expectedObjVal}
Actual Objective Value             : {actualObjVal}
    |]
    $ do
      -- Compare variable maps (ignoring objective value field in ExpectOptimal)
      let stripObjVal (ExpectOptimal _ vm) = ExpectOptimal Nothing vm
          stripObjVal other = other
      stripObjVal actualResult `shouldBe` stripObjVal expectedResult
      -- When an expected objective value is provided, verify it matches
      case expectedResult of
        ExpectOptimal (Just _) _ -> actualObjVal `shouldBe` expectedObjVal
        _ -> pure ()

spec :: Spec
spec = do
  describe "twoPhaseSimplex" $ do
    -- From page 50 of 'Linear and Integer Programming Made Easy'
    describe "From 'Linear and Integer Programming Made Easy' (page 50)" $ do
      it "Max 3x₁ + 5x₂ with LEQ constraints: obj=29, x₁=3, x₂=4" $ do
        let testCase =
              ( Max (M.fromList [(1, 3), (2, 5)])
              , [ LEQ (M.fromList [(1, 3), (2, 1)]) 15
                , LEQ (M.fromList [(1, 1), (2, 1)]) 7
                , LEQ (M.fromList [(2, 1)]) 4
                , LEQ (M.fromList [(1, -1), (2, 2)]) 6
                ]
              )
        runTest testCase (ExpectOptimal (Just 29) (M.fromList [(1, 3), (2, 4)]))

      it "Min 3x₁ + 5x₂ with LEQ constraints: obj=0" $ do
        let testCase =
              ( Min (M.fromList [(1, 3), (2, 5)])
              , [ LEQ (M.fromList [(1, 3), (2, 1)]) 15
                , LEQ (M.fromList [(1, 1), (2, 1)]) 7
                , LEQ (M.fromList [(2, 1)]) 4
                , LEQ (M.fromList [(1, -1), (2, 2)]) 6
                ]
              )
        runTest testCase (ExpectOptimal (Just 0) M.empty)

      it "Max 3x₁ + 5x₂ with GEQ constraints: unbounded" $ do
        let testCase =
              ( Max (M.fromList [(1, 3), (2, 5)])
              , [ GEQ (M.fromList [(1, 3), (2, 1)]) 15
                , GEQ (M.fromList [(1, 1), (2, 1)]) 7
                , GEQ (M.fromList [(2, 1)]) 4
                , GEQ (M.fromList [(1, -1), (2, 2)]) 6
                ]
              )
        runTest testCase ExpectUnbounded

      it "Min 3x₁ + 5x₂ with GEQ constraints: obj=237/7, x₁=24/7, x₂=33/7" $ do
        let testCase =
              ( Min (M.fromList [(1, 3), (2, 5)])
              , [ GEQ (M.fromList [(1, 3), (2, 1)]) 15
                , GEQ (M.fromList [(1, 1), (2, 1)]) 7
                , GEQ (M.fromList [(2, 1)]) 4
                , GEQ (M.fromList [(1, -1), (2, 2)]) 6
                ]
              )
        runTest testCase (ExpectOptimal (Just (237 % 7)) (M.fromList [(1, 24 % 7), (2, 33 % 7)]))

    -- From https://www.eng.uwaterloo.ca/~syde05/phase1.pdf (requires two phases)
    describe "From eng.uwaterloo.ca phase1.pdf (requires two phases)" $ do
      it "Max x₁ - x₂ + x₃ with LEQ constraints: obj=3/5, x₂=14/5, x₃=17/5" $ do
        let testCase =
              ( Max (M.fromList [(1, 1), (2, -1), (3, 1)])
              , [ LEQ (M.fromList [(1, 2), (2, -1), (3, 2)]) 4
                , LEQ (M.fromList [(1, 2), (2, -3), (3, 1)]) (-5)
                , LEQ (M.fromList [(1, -1), (2, 1), (3, -2)]) (-1)
                ]
              )
        runTest testCase (ExpectOptimal (Just (3 % 5)) (M.fromList [(2, 14 % 5), (3, 17 % 5)]))

      it "Min x₁ - x₂ + x₃ with LEQ constraints: unbounded" $ do
        let testCase =
              ( Min (M.fromList [(1, 1), (2, -1), (3, 1)])
              , [ LEQ (M.fromList [(1, 2), (2, -1), (3, 2)]) 4
                , LEQ (M.fromList [(1, 2), (2, -3), (3, 1)]) (-5)
                , LEQ (M.fromList [(1, -1), (2, 1), (3, -2)]) (-1)
                ]
              )
        runTest testCase ExpectUnbounded

      it "Max x₁ - x₂ + x₃ with GEQ constraints: obj=1, x₁=3, x₂=2" $ do
        let testCase =
              ( Max (M.fromList [(1, 1), (2, -1), (3, 1)])
              , [ GEQ (M.fromList [(1, 2), (2, -1), (3, 2)]) 4
                , GEQ (M.fromList [(1, 2), (2, -3), (3, 1)]) (-5)
                , GEQ (M.fromList [(1, -1), (2, 1), (3, -2)]) (-1)
                ]
              )
        runTest testCase (ExpectOptimal (Just 1) (M.fromList [(1, 3), (2, 2)]))

      it "Min x₁ - x₂ + x₃ with GEQ constraints: obj=-1/4, x₁=17/4, x₂=9/2" $ do
        let testCase =
              ( Min (M.fromList [(1, 1), (2, -1), (3, 1)])
              , [ GEQ (M.fromList [(1, 2), (2, -1), (3, 2)]) 4
                , GEQ (M.fromList [(1, 2), (2, -3), (3, 1)]) (-5)
                , GEQ (M.fromList [(1, -1), (2, 1), (3, -2)]) (-1)
                ]
              )
        runTest testCase (ExpectOptimal (Just ((-1) % 4)) (M.fromList [(1, 17 % 4), (2, 9 % 2)]))

    -- From page 49 of 'Linear and Integer Programming Made Easy' (requires two phases)
    describe "From 'Linear and Integer Programming Made Easy' (page 49, requires two phases)" $ do
      it "Min x₁ + x₂ + 2x₃ + x₄ with EQ constraints: obj=5, x₃=2, x₄=1" $ do
        let testCase =
              ( Min (M.fromList [(1, 1), (2, 1), (3, 2), (4, 1)])
              , [ EQ (M.fromList [(1, 1), (3, 2), (4, -2)]) 2
                , EQ (M.fromList [(2, 1), (3, 1), (4, 4)]) 6
                ]
              )
        runTest testCase (ExpectOptimal (Just 5) (M.fromList [(3, 2), (4, 1)]))

      it "Max x₁ + x₂ + 2x₃ + x₄ with EQ constraints: obj=8, x₁=2, x₂=6" $ do
        let testCase =
              ( Max (M.fromList [(1, 1), (2, 1), (3, 2), (4, 1)])
              , [ EQ (M.fromList [(1, 1), (3, 2), (4, -2)]) 2
                , EQ (M.fromList [(2, 1), (3, 1), (4, 4)]) 6
                ]
              )
        runTest testCase (ExpectOptimal (Just 8) (M.fromList [(1, 2), (2, 6)]))

    -- From page 52 of 'Linear and Integer Programming Made Easy'
    describe "From 'Linear and Integer Programming Made Easy' (page 52)" $ do
      it "Max -2x₃ + 2x₄ + x₅ with EQ constraints: obj=20, x₃=6, x₄=16" $ do
        let testCase =
              ( Max (M.fromList [(3, -2), (4, 2), (5, 1)])
              , [ EQ (M.fromList [(3, -2), (4, 1), (5, 1)]) 4
                , EQ (M.fromList [(3, 3), (4, -1), (5, 2)]) 2
                ]
              )
        runTest testCase (ExpectOptimal (Just 20) (M.fromList [(3, 6), (4, 16)]))

      it "Min -2x₃ + 2x₄ + x₅ with EQ constraints: obj=6, x₄=2, x₅=2" $ do
        let testCase =
              ( Min (M.fromList [(3, -2), (4, 2), (5, 1)])
              , [ EQ (M.fromList [(3, -2), (4, 1), (5, 1)]) 4
                , EQ (M.fromList [(3, 3), (4, -1), (5, 2)]) 2
                ]
              )
        runTest testCase (ExpectOptimal (Just 6) (M.fromList [(4, 2), (5, 2)]))

    -- From page 59 of 'Linear and Integer Programming Made Easy' (requires two phases)
    describe "From 'Linear and Integer Programming Made Easy' (page 59, requires two phases)" $ do
      it "Max 2x₁ + x₂: obj=150, x₂=150" $ do
        let testCase =
              ( Max (M.fromList [(1, 2), (2, 1)])
              , [ LEQ (M.fromList [(1, 4), (2, 1)]) 150
                , LEQ (M.fromList [(1, 2), (2, -3)]) (-40)
                ]
              )
        runTest testCase (ExpectOptimal (Just 150) (M.fromList [(2, 150)]))

      it "Min 2x₁ + x₂: obj=40/3, x₂=40/3" $ do
        let testCase =
              ( Min (M.fromList [(1, 2), (2, 1)])
              , [ LEQ (M.fromList [(1, 4), (2, 1)]) 150
                , LEQ (M.fromList [(1, 2), (2, -3)]) (-40)
                ]
              )
        runTest testCase (ExpectOptimal (Just (40 % 3)) (M.fromList [(2, 40 % 3)]))

      it "Max 2x₁ + x₂ with GEQ constraints: unbounded" $ do
        let testCase =
              ( Max (M.fromList [(1, 2), (2, 1)])
              , [ GEQ (M.fromList [(1, 4), (2, 1)]) 150
                , GEQ (M.fromList [(1, 2), (2, -3)]) (-40)
                ]
              )
        runTest testCase ExpectUnbounded

      it "Min 2x₁ + x₂ with GEQ constraints: obj=75, x₁=75/2" $ do
        let testCase =
              ( Min (M.fromList [(1, 2), (2, 1)])
              , [ GEQ (M.fromList [(1, 4), (2, 1)]) 150
                , GEQ (M.fromList [(1, 2), (2, -3)]) (-40)
                ]
              )
        runTest testCase (ExpectOptimal (Just 75) (M.fromList [(1, 75 % 2)]))

    -- From page 59 of 'Linear and Integer Programming Made Easy'
    describe "From 'Linear and Integer Programming Made Easy' (page 59)" $ do
      it "Min -6x₁ - 4x₂ + 2x₃: obj=-120, x₁=20" $ do
        let testCase =
              ( Min (M.fromList [(1, -6), (2, -4), (3, 2)])
              , [ LEQ (M.fromList [(1, 1), (2, 1), (3, 4)]) 20
                , LEQ (M.fromList [(2, -5), (3, 5)]) 100
                , LEQ (M.fromList [(1, 1), (3, 1), (1, 1)]) 400
                ]
              )
        runTest testCase (ExpectOptimal (Just (-120)) (M.fromList [(1, 20)]))

      it "Max -6x₁ - 4x₂ + 2x₃: obj=10, x₃=5" $ do
        let testCase =
              ( Max (M.fromList [(1, -6), (2, -4), (3, 2)])
              , [ LEQ (M.fromList [(1, 1), (2, 1), (3, 4)]) 20
                , LEQ (M.fromList [(2, -5), (3, 5)]) 100
                , LEQ (M.fromList [(1, 1), (3, 1), (1, 1)]) 400
                ]
              )
        runTest testCase (ExpectOptimal (Just 10) (M.fromList [(3, 5)]))

      it "Min -6x₁ - 4x₂ + 2x₃ with GEQ constraints: unbounded" $ do
        let testCase =
              ( Min (M.fromList [(1, -6), (2, -4), (3, 2)])
              , [ GEQ (M.fromList [(1, 1), (2, 1), (3, 4)]) 20
                , GEQ (M.fromList [(2, -5), (3, 5)]) 100
                , GEQ (M.fromList [(1, 1), (3, 1), (1, 1)]) 400
                ]
              )
        runTest testCase ExpectUnbounded

      it "Max -6x₁ - 4x₂ + 2x₃ with GEQ constraints: unbounded" $ do
        let testCase =
              ( Max (M.fromList [(1, -6), (2, -4), (3, 2)])
              , [ GEQ (M.fromList [(1, 1), (2, 1), (3, 4)]) 20
                , GEQ (M.fromList [(2, -5), (3, 5)]) 100
                , GEQ (M.fromList [(1, 1), (3, 1), (1, 1)]) 400
                ]
              )
        runTest testCase ExpectUnbounded

    -- From page 59 of 'Linear and Integer Programming Made Easy'
    describe "From 'Linear and Integer Programming Made Easy' (page 59)" $ do
      it "Max 3x₁ + 5x₂ + 2x₃: obj=250, x₂=50" $ do
        let testCase =
              ( Max (M.fromList [(1, 3), (2, 5), (3, 2)])
              , [ LEQ (M.fromList [(1, 5), (2, 1), (3, 4)]) 50
                , LEQ (M.fromList [(1, 1), (2, -1), (3, 1)]) 150
                , LEQ (M.fromList [(1, 2), (2, 1), (3, 2)]) 100
                ]
              )
        runTest testCase (ExpectOptimal (Just 250) (M.fromList [(2, 50)]))

      it "Min 3x₁ + 5x₂ + 2x₃: obj=0" $ do
        let testCase =
              ( Min (M.fromList [(1, 3), (2, 5), (3, 2)])
              , [ LEQ (M.fromList [(1, 5), (2, 1), (3, 4)]) 50
                , LEQ (M.fromList [(1, 1), (2, -1), (3, 1)]) 150
                , LEQ (M.fromList [(1, 2), (2, 1), (3, 2)]) 100
                ]
              )
        runTest testCase (ExpectOptimal (Just 0) M.empty)

      it "Max 3x₁ + 5x₂ + 2x₃ with GEQ constraints: unbounded" $ do
        let testCase =
              ( Max (M.fromList [(1, 3), (2, 5), (3, 2)])
              , [ GEQ (M.fromList [(1, 5), (2, 1), (3, 4)]) 50
                , GEQ (M.fromList [(1, 1), (2, -1), (3, 1)]) 150
                , GEQ (M.fromList [(1, 2), (2, 1), (3, 2)]) 100
                ]
              )
        runTest testCase ExpectUnbounded

      it "Min 3x₁ + 5x₂ + 2x₃ with GEQ constraints: obj=300, x₃=150" $ do
        let testCase =
              ( Min (M.fromList [(1, 3), (2, 5), (3, 2)])
              , [ GEQ (M.fromList [(1, 5), (2, 1), (3, 4)]) 50
                , GEQ (M.fromList [(1, 1), (2, -1), (3, 1)]) 150
                , GEQ (M.fromList [(1, 2), (2, 1), (3, 2)]) 100
                ]
              )
        runTest testCase (ExpectOptimal (Just 300) (M.fromList [(3, 150)]))

    describe "Simple single/two variable tests" $ do
      it "Max x₁ with x₁ <= 15: obj=15, x₁=15" $ do
        let testCase =
              ( Max (M.fromList [(1, 1)])
              , [ LEQ (M.fromList [(1, 1)]) 15
                ]
              )
        runTest testCase (ExpectOptimal (Just 15) (M.fromList [(1, 15)]))

      it "Max 2x₁ with mixed constraints: obj=20, x₁=10, x₂=10" $ do
        let testCase =
              ( Max (M.fromList [(1, 2)])
              , [ LEQ (M.fromList [(1, 2)]) 20
                , GEQ (M.fromList [(2, 1)]) 10
                ]
              )
        runTest testCase (ExpectOptimal (Just 20) (M.fromList [(1, 10), (2, 10)]))

      it "Min x₁ with x₁ <= 15: obj=0" $ do
        let testCase =
              ( Min (M.fromList [(1, 1)])
              , [ LEQ (M.fromList [(1, 1)]) 15
                ]
              )
        runTest testCase (ExpectOptimal (Just 0) M.empty)

      it "Min 2x₁ with mixed constraints: obj=0, x₂=10" $ do
        let testCase =
              ( Min (M.fromList [(1, 2)])
              , [ LEQ (M.fromList [(1, 2)]) 20
                , GEQ (M.fromList [(2, 1)]) 10
                ]
              )
        runTest testCase (ExpectOptimal (Just 0) (M.fromList [(2, 10)]))

    describe "Infeasibility tests" $ do
      it "Conflicting bounds x₁ <= 15 and x₁ >= 15.01: infeasible" $ do
        let testCase =
              ( Max (M.fromList [(1, 1)])
              , [ LEQ (M.fromList [(1, 1)]) 15
                , GEQ (M.fromList [(1, 1)]) 15.01
                ]
              )
        runTest testCase ExpectInfeasible

      it "Conflicting bounds with additional constraint: infeasible" $ do
        let testCase =
              ( Max (M.fromList [(1, 1)])
              , [ LEQ (M.fromList [(1, 1)]) 15
                , GEQ (M.fromList [(1, 1)]) 15.01
                , GEQ (M.fromList [(2, 1)]) 10
                ]
              )
        runTest testCase ExpectInfeasible

      it "Min x₁ with duplicate GEQ constraints: obj=0, x₂=1" $ do
        let testCase =
              ( Min (M.fromList [(1, 1)])
              , [ GEQ (M.fromList [(1, 1), (2, 1)]) 1
                , GEQ (M.fromList [(1, 1), (2, 1)]) 1
                ]
              )
        runTest testCase (ExpectOptimal (Just 0) (M.fromList [(2, 1 % 1)]))

      it "Conflicting x₁+x₂ >= 2 and x₁+x₂ <= 1: infeasible" $ do
        let testCase =
              ( Min (M.fromList [(1, 1)])
              , [ GEQ (M.fromList [(1, 1), (2, 1)]) 2
                , LEQ (M.fromList [(1, 1), (2, 1)]) 1
                ]
              )
        runTest testCase ExpectInfeasible

    describe "LEQ/GEQ reduction bug tests" $ do
      it "testLeqGeqBugMin1: obj=3, x₁=3, x₂=3" $ do
        let testCase =
              ( Min (M.fromList [(1, 1)])
              , [ GEQ (M.fromList [(1, 1)]) 3
                , LEQ (M.fromList [(1, 1)]) 3
                , GEQ (M.fromList [(2, 1)]) 3
                , LEQ (M.fromList [(2, 1)]) 3
                ]
              )
        runTest testCase (ExpectOptimal (Just 3) (M.fromList [(1, 3), (2, 3)]))

      it "testLeqGeqBugMax1: obj=3, x₁=3, x₂=3" $ do
        let testCase =
              ( Min (M.fromList [(1, 1)])
              , [ GEQ (M.fromList [(1, 1)]) 3
                , LEQ (M.fromList [(1, 1)]) 3
                , GEQ (M.fromList [(2, 1)]) 3
                , LEQ (M.fromList [(2, 1)]) 3
                ]
              )
        runTest testCase (ExpectOptimal (Just 3) (M.fromList [(1, 3), (2, 3)]))

      it "testLeqGeqBugMin2: obj=3, x₁=3, x₂=3" $ do
        let testCase =
              ( Min (M.fromList [(1, 1)])
              , [ GEQ (M.fromList [(1, 1)]) 3
                , LEQ (M.fromList [(1, 1)]) 3
                , GEQ (M.fromList [(2, 1)]) 3
                , LEQ (M.fromList [(2, 1)]) 3
                ]
              )
        runTest testCase (ExpectOptimal (Just 3) (M.fromList [(1, 3), (2, 3)]))

      it "testLeqGeqBugMax2: obj=3, x₁=3, x₂=3" $ do
        let testCase =
              ( Min (M.fromList [(1, 1)])
              , [ GEQ (M.fromList [(1, 1)]) 3
                , LEQ (M.fromList [(1, 1)]) 3
                , GEQ (M.fromList [(2, 1)]) 3
                , LEQ (M.fromList [(2, 1)]) 3
                ]
              )
        runTest testCase (ExpectOptimal (Just 3) (M.fromList [(1, 3), (2, 3)]))

    -- PolyPaver-style tests with shared parameters
    describe "PolyPaver-style tests (feasible region [0,2.5]²)" $ do
      let x1l = 0.0; x1r = 2.5; x2l = 0.0; x2r = 2.5
          dx1l = -1; dx1r = -0.9; dx2l = -0.9; dx2r = -0.8
          yl = 4; yr = 5
          mkConstraints obj =
            ( obj
            , [ LEQ (M.fromList [(1, dx1l), (2, dx2l), (3, (-1))]) (-yl + dx1l * x1l + dx2l * x2l)
              , GEQ (M.fromList [(1, dx1r), (2, dx2r), (3, (-1))]) (-yr + dx1r * x1l + dx2r * x2l)
              , GEQ (M.fromList [(1, 1)]) x1l
              , LEQ (M.fromList [(1, 1)]) x1r
              , GEQ (M.fromList [(2, 1)]) x2l
              , LEQ (M.fromList [(2, 1)]) x2r
              , LEQ (M.fromList [(3, 1)]) 0
              ]
            )

      it "Min x₁: x₁=7/4, x₂=5/2" $ do
        runTest (mkConstraints (Min (M.fromList [(1, 1)]))) 
                (ExpectOptimal (Just (7 % 4)) (M.fromList [(1, 7 % 4), (2, 5 % 2), (3, 0)]))

      it "Max x₁: x₁=5/2, x₂=5/3" $ do
        runTest (mkConstraints (Max (M.fromList [(1, 1)]))) 
                (ExpectOptimal (Just (5 % 2)) (M.fromList [(1, 5 % 2), (2, 5 % 3), (3, 0)]))

      it "Min x₂: x₂=5/3" $ do
        runTest (mkConstraints (Min (M.fromList [(2, 1)]))) 
                (ExpectOptimal (Just (5 % 3)) (M.fromList [(2, 5 % 3), (1, 5 % 2), (3, 0)]))

      it "Max x₂: x₂=5/2" $ do
        runTest (mkConstraints (Max (M.fromList [(2, 1)]))) 
                (ExpectOptimal (Just (5 % 2)) (M.fromList [(2, 5 % 2), (1, 5 % 2), (3, 0)]))

    describe "PolyPaver-style tests (infeasible region [0,1.5]²)" $ do
      let x1l = 0.0; x1r = 1.5; x2l = 0.0; x2r = 1.5
          dx1l = -1; dx1r = -0.9; dx2l = -0.9; dx2r = -0.8
          yl = 4; yr = 5
          mkConstraints obj =
            ( obj
            , [ LEQ (M.fromList [(1, dx1l), (2, dx2l), (3, (-1))]) (-yl + dx1l * x1l + dx2l * x2l)
              , GEQ (M.fromList [(1, dx1r), (2, dx2r), (3, (-1))]) (-yr + dx1r * x1l + dx2r * x2l)
              , GEQ (M.fromList [(1, 1)]) x1l
              , LEQ (M.fromList [(1, 1)]) x1r
              , GEQ (M.fromList [(2, 1)]) x2l
              , LEQ (M.fromList [(2, 1)]) x2r
              , LEQ (M.fromList [(3, 1)]) 0
              ]
            )

      it "Max x₁: infeasible" $ do
        runTest (mkConstraints (Max (M.fromList [(1, 1)]))) ExpectInfeasible

      it "Min x₁: infeasible" $ do
        runTest (mkConstraints (Min (M.fromList [(1, 1)]))) ExpectInfeasible

      it "Max x₂: infeasible" $ do
        runTest (mkConstraints (Max (M.fromList [(2, 1)]))) ExpectInfeasible

      it "Min x₂: infeasible" $ do
        runTest (mkConstraints (Min (M.fromList [(2, 1)]))) ExpectInfeasible

    describe "PolyPaver-style tests (feasible region [0,3.5]²)" $ do
      let x1l = 0.0; x1r = 3.5; x2l = 0.0; x2r = 3.5
          dx1l = -1; dx1r = -0.9; dx2l = -0.9; dx2r = -0.8
          yl = 4; yr = 5
          mkConstraints obj =
            ( obj
            , [ LEQ (M.fromList [(1, dx1l), (2, dx2l), (3, (-1))]) (-yl + dx1l * x1l + dx2l * x2l)
              , GEQ (M.fromList [(1, dx1r), (2, dx2r), (3, (-1))]) (-yr + dx1r * x1l + dx2r * x2l)
              , GEQ (M.fromList [(1, 1)]) x1l
              , LEQ (M.fromList [(1, 1)]) x1r
              , GEQ (M.fromList [(2, 1)]) x2l
              , LEQ (M.fromList [(2, 1)]) x2r
              , LEQ (M.fromList [(3, 1)]) 0
              ]
            )

      it "Max x₁: x₁=7/2" $ do
        runTest (mkConstraints (Max (M.fromList [(1, 1)]))) 
                (ExpectOptimal (Just (7 % 2)) (M.fromList [(2, 5 % 9), (1, 7 % 2), (3, 0)]))

      it "Min x₁: x₁=17/20" $ do
        runTest (mkConstraints (Min (M.fromList [(1, 1)]))) 
                (ExpectOptimal (Just (17 % 20)) (M.fromList [(1, 17 % 20), (2, 7 % 2), (3, 0)]))

      it "Max x₂: x₂=7/2" $ do
        runTest (mkConstraints (Max (M.fromList [(2, 1)]))) 
                (ExpectOptimal (Just (7 % 2)) (M.fromList [(2, 7 % 2), (1, 22 % 9)]))

      it "Min x₂: x₂=5/9" $ do
        runTest (mkConstraints (Min (M.fromList [(2, 1)]))) 
                (ExpectOptimal (Just (5 % 9)) (M.fromList [(2, 5 % 9), (1, 7 % 2), (3, 0)]))

    describe "PolyPaver two-function tests (infeasible)" $ do
      let x1l = 0.0; x1r = 2.5; x2l = 0.0; x2r = 2.5
          f1dx1l = -1; f1dx1r = -0.9; f1dx2l = -0.9; f1dx2r = -0.8
          f1yl = 4; f1yr = 5
          f2dx1l = -1; f2dx1r = -0.9; f2dx2l = -0.9; f2dx2r = -0.8
          f2yl = 1; f2yr = 2
          mkConstraints obj =
            ( obj
            , [ LEQ (M.fromList [(1, f1dx1l), (2, f1dx2l), (3, (-1))]) (-f1yl + f1dx1l * x1l + f1dx2l * x2l)
              , GEQ (M.fromList [(1, f1dx1r), (2, f1dx2r), (3, (-1))]) (-f1yr + f1dx1r * x1l + f1dx2r * x2l)
              , LEQ (M.fromList [(1, f2dx1l), (2, f2dx2l), (4, (-1))]) (-f2yl + f2dx1l * x1l + f2dx2l * x2l)
              , GEQ (M.fromList [(1, f2dx1r), (2, f2dx2r), (4, (-1))]) (-f2yr + f2dx1r * x1l + f2dx2r * x2l)
              , GEQ (M.fromList [(1, 1)]) x1l
              , LEQ (M.fromList [(1, 1)]) x1r
              , GEQ (M.fromList [(2, 1)]) x2l
              , LEQ (M.fromList [(2, 1)]) x2r
              , LEQ (M.fromList [(3, 1)]) 0
              , LEQ (M.fromList [(4, 1)]) 0
              ]
            )

      it "Max x₁: infeasible" $ do
        runTest (mkConstraints (Max (M.fromList [(1, 1)]))) ExpectInfeasible

      it "Min x₁: infeasible" $ do
        runTest (mkConstraints (Min (M.fromList [(1, 1)]))) ExpectInfeasible

      it "Max x₂: infeasible" $ do
        runTest (mkConstraints (Max (M.fromList [(2, 1)]))) ExpectInfeasible

      it "Min x₂: infeasible" $ do
        runTest (mkConstraints (Min (M.fromList [(2, 1)]))) ExpectInfeasible

    describe "PolyPaver two-function tests (feasible)" $ do
      let x1l = 0.0; x1r = 2.5; x2l = 0.0; x2r = 2.5
          f1dx1l = -1; f1dx1r = -0.9; f1dx2l = -0.9; f1dx2r = -0.8
          f1yl = 4; f1yr = 5
          f2dx1l = -0.66; f2dx1r = -0.66; f2dx2l = -0.66; f2dx2r = -0.66
          f2yl = 3; f2yr = 4
          mkConstraints obj =
            ( obj
            , [ LEQ (M.fromList [(1, f1dx1l), (2, f1dx2l), (3, (-1))]) (-f1yl + f1dx1l * x1l + f1dx2l * x2l)
              , GEQ (M.fromList [(1, f1dx1r), (2, f1dx2r), (3, (-1))]) (-f1yr + f1dx1r * x1l + f1dx2r * x2l)
              , LEQ (M.fromList [(1, f2dx1l), (2, f2dx2l), (4, (-1))]) (-f2yl + f2dx1l * x1l + f2dx2l * x2l)
              , GEQ (M.fromList [(1, f2dx1r), (2, f2dx2r), (4, (-1))]) (-f2yr + f2dx1r * x1l + f2dx2r * x2l)
              , GEQ (M.fromList [(1, 1)]) x1l
              , LEQ (M.fromList [(1, 1)]) x1r
              , GEQ (M.fromList [(2, 1)]) x2l
              , LEQ (M.fromList [(2, 1)]) x2r
              , LEQ (M.fromList [(3, 1)]) 0
              , LEQ (M.fromList [(4, 1)]) 0
              ]
            )

      it "Max x₁: x₁=5/2" $ do
        runTest (mkConstraints (Max (M.fromList [(1, 1)]))) 
                (ExpectOptimal (Just (5 % 2)) (M.fromList [(1, 5 % 2), (2, 45 % 22), (4, 0)]))

      it "Min x₁: x₁=45/22" $ do
        runTest (mkConstraints (Min (M.fromList [(1, 1)]))) 
                (ExpectOptimal (Just (45 % 22)) (M.fromList [(1, 45 % 22), (2, 5 % 2), (4, 0)]))

      it "Max x₂: x₂=5/2" $ do
        runTest (mkConstraints (Max (M.fromList [(2, 1)]))) 
                (ExpectOptimal (Just (5 % 2)) (M.fromList [(2, 5 % 2), (1, 5 % 2), (4, 0)]))

      it "Min x₂: x₂=45/22" $ do
        runTest (mkConstraints (Min (M.fromList [(2, 1)]))) 
                (ExpectOptimal (Just (45 % 22)) (M.fromList [(2, 45 % 22), (1, 5 % 2), (4, 0)]))

    describe "QuickCheck-generated regression tests" $ do
      it "testQuickCheck1: obj=-370, x₁=5/3, x₂=26" $ do
        let testCase =
              ( Max (M.fromList [(1, 12), (2, -15)])
              , [ EQ (M.fromList [(1, 24), (2, -2)]) (-12)
                , GEQ (M.fromList [(1, -20), (2, 11)]) (-7)
                , GEQ (M.fromList [(1, -28), (2, 5)]) (-8)
                , GEQ (M.fromList [(1, 3), (2, 0)]) 5
                , LEQ (M.fromList [(1, -48)]) (-1)
                ]
              )
        runTest testCase (ExpectOptimal (Just (-370)) (M.fromList [(2, 26), (1, 5 % 3)]))

      it "testQuickCheck2: obj=-2/9, x₁=14/9, x₂=8/9" $ do
        let testCase =
              ( Max (M.fromList [(1, -3), (2, 5)])
              , [ LEQ (M.fromList [(1, -6), (2, 6)]) 4
                , LEQ (M.fromList [(1, 1), (2, -4), (3, 3)]) (-2)
                , LEQ (M.fromList [(2, 7), (1, -4)]) 0
                ]
              )
        runTest testCase (ExpectOptimal (Just ((-2) % 9)) (M.fromList [(1, 14 % 9), (2, 8 % 9)]))

      it "testQuickCheck3 (tests objective simplification): obj=-8, x₂=2" $ do
        let testCase =
              ( Min (M.fromList [(2, 0), (2, -4)])
              , [ GEQ (M.fromList [(1, 5), (2, 4)]) (-4)
                , LEQ (M.fromList [(1, -1), (2, -1)]) 2
                , LEQ (M.fromList [(2, 1)]) 2
                , GEQ (M.fromList [(1, -5), (2, -1), (2, 1)]) (-5)
                ]
              )
        runTest testCase (ExpectOptimal (Just (-8)) (M.fromList [(2, 2)]))

  describe "twoPhaseSimplex (with VarDomainMap)" $ do
    it "Shift transformation with negative lower bound" $ do
      let obj = Max (M.fromList [(1, 1)])
          constraints = [ LEQ (M.fromList [(1, 1)]) 10 ]
          domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly (-5))]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap [obj] constraints
      case actualResult of
        SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
        SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just 10
        _ -> expectationFailure "Unexpected result format"

    it "Shift transformation finds minimum at negative bound" $ do
      let obj = Min (M.fromList [(1, 1)])
          constraints = [ LEQ (M.fromList [(1, 1)]) 0 ]
          domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly (-5))]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap [obj] constraints
      case actualResult of
        SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
        SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just (-5)
        _ -> expectationFailure "Unexpected result format"

    it "Split transformation for unbounded variable (max)" $ do
      let obj = Max (M.fromList [(1, 1)])
          constraints = 
            [ LEQ (M.fromList [(1, 1)]) 10
            , GEQ (M.fromList [(1, 1)]) (-10)
            ]
          domainMap = VarDomainMap $ M.fromList [(1, unbounded)]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap [obj] constraints
      case actualResult of
        SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
        SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just 10
        _ -> expectationFailure "Unexpected result format"

    it "Split transformation for unbounded variable (min)" $ do
      let obj = Min (M.fromList [(1, 1)])
          constraints = 
            [ LEQ (M.fromList [(1, 1)]) 10
            , GEQ (M.fromList [(1, 1)]) (-10)
            ]
          domainMap = VarDomainMap $ M.fromList [(1, unbounded)]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap [obj] constraints
      case actualResult of
        SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
        SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just (-10)
        _ -> expectationFailure "Unexpected result format"

    it "AddLowerBound with positive lower bound" $ do
      let obj = Max (M.fromList [(1, 1)])
          constraints = [ LEQ (M.fromList [(1, 1)]) 10 ]
          domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly 5)]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap [obj] constraints
      case actualResult of
        SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
        SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just 10
        _ -> expectationFailure "Unexpected result format"

    it "AddLowerBound finds minimum at positive bound" $ do
      let obj = Min (M.fromList [(1, 1)])
          constraints = [ LEQ (M.fromList [(1, 1)]) 10 ]
          domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly 5)]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap [obj] constraints
      case actualResult of
        SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
        SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just 5
        _ -> expectationFailure "Unexpected result format"

    it "Mixed domain types" $ do
      let obj = Max (M.fromList [(1, 1), (2, 1)])
          constraints = 
            [ LEQ (M.fromList [(1, 1), (2, 1)]) 5
            , GEQ (M.fromList [(2, 1)]) (-3)
            ]
          domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly (-2)), (2, unbounded)]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap [obj] constraints
      case actualResult of
        SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
        SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
          let xVal = M.findWithDefault 0 1 varMap
              yVal = M.findWithDefault 0 2 varMap
              oVal = computeObjValue obj varMap
          (xVal + yVal) `shouldBe` 5
          oVal `shouldBe` 5
        _ -> expectationFailure "Unexpected result format"

    it "lowerBoundOnly 0 is equivalent to NonNegative" $ do
      let obj = Max (M.fromList [(1, 3), (2, 5)])
          constraints = 
            [ LEQ (M.fromList [(1, 3), (2, 1)]) 15
            , LEQ (M.fromList [(1, 1), (2, 1)]) 7
            , LEQ (M.fromList [(2, 1)]) 4
            , LEQ (M.fromList [(1, -1), (2, 2)]) 6
            ]
          domainMap1 = VarDomainMap $ M.fromList [(1, lowerBoundOnly 0), (2, lowerBoundOnly 0)]
          domainMap2 = VarDomainMap $ M.fromList [(1, nonNegative), (2, nonNegative)]
      actualResult1 <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap1 [obj] constraints
      actualResult2 <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap2 [obj] constraints
      -- Both should produce the same optimal solution with x₁=3, x₂=4
      case (actualResult1, actualResult2) of
        (SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap1)], SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap2)]) -> do
          varMap1 `shouldBe` M.fromList [(1, 3), (2, 4)]
          varMap1 `shouldBe` varMap2
        _ -> expectationFailure "Expected optimal results"

    it "Infeasible system with domain constraint" $ do
      let obj = Max (M.fromList [(1, 1)])
          constraints = [ LEQ (M.fromList [(1, 1)]) 5 ]
          domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly 10)]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap [obj] constraints
      case actualResult of
        SimplexResult Nothing _ -> pure ()
        _ -> expectationFailure "Expected infeasible result"

  describe "twoPhaseSimplex with upper bounds (AddUpperBound transformation)" $ do
    describe "Simple single variable systems" $ do
      it "Max x₁ with x₁ ≥ 0, x₁ ≤ 5 (using boundedRange): optimal at x₁=5" $ do
        let obj = Max (M.fromList [(1, 1)])
            constraints = []
            domainMap = VarDomainMap $ M.fromList [(1, boundedRange 0 5)]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just 5
          _ -> expectationFailure "Unexpected result format"

      it "Min x₁ with x₁ ≥ 0, x₁ ≤ 10 (using boundedRange): optimal at x₁=0" $ do
        let obj = Min (M.fromList [(1, 1)])
            constraints = []
            domainMap = VarDomainMap $ M.fromList [(1, boundedRange 0 10)]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> 
            -- Note: non-basic variables with value 0 may not appear in varValMap
            M.findWithDefault 0 1 varMap `shouldBe` 0
          _ -> expectationFailure "Unexpected result format"

      it "Max x₁ with -5 ≤ x₁ ≤ 10 (bounded range with negative lower): optimal at x₁=10" $ do
        let obj = Max (M.fromList [(1, 1)])
            constraints = []
            domainMap = VarDomainMap $ M.fromList [(1, boundedRange (-5) 10)]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just 10
          _ -> expectationFailure "Unexpected result format"

      it "Min x₁ with -5 ≤ x₁ ≤ 10 (bounded range with negative lower): optimal at x₁=-5" $ do
        let obj = Min (M.fromList [(1, 1)])
            constraints = []
            domainMap = VarDomainMap $ M.fromList [(1, boundedRange (-5) 10)]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just (-5)
          _ -> expectationFailure "Unexpected result format"

      it "Infeasible: lower bound > upper bound" $ do
        let obj = Max (M.fromList [(1, 1)])
            constraints = []
            domainMap = VarDomainMap $ M.fromList [(1, boundedRange 10 5)]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> pure ()
          _ -> expectationFailure "Expected infeasible system"

    describe "Two variable systems with upper bounds" $ do
      it "Max x₁ + x₂ with 0 ≤ x₁ ≤ 3, 0 ≤ x₂ ≤ 4: optimal at x₁=3, x₂=4" $ do
        let obj = Max (M.fromList [(1, 1), (2, 1)])
            constraints = []
            domainMap = VarDomainMap $ M.fromList [(1, boundedRange 0 3), (2, boundedRange 0 4)]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
            M.lookup 1 varMap `shouldBe` Just 3
            M.lookup 2 varMap `shouldBe` Just 4
            computeObjValue obj varMap `shouldBe` 7
          _ -> expectationFailure "Unexpected result format"

      it "Max 2x₁ - x₂ with -2 ≤ x₁ ≤ 5, -3 ≤ x₂ ≤ 4" $ do
        -- Maximize 2x₁ - x₂: want x₁ = 5 (max), x₂ = -3 (min)
        -- Optimal: 2*5 - (-3) = 13
        let obj = Max (M.fromList [(1, 2), (2, -1)])
            constraints = []
            domainMap = VarDomainMap $ M.fromList [(1, boundedRange (-2) 5), (2, boundedRange (-3) 4)]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
            M.lookup 1 varMap `shouldBe` Just 5
            M.lookup 2 varMap `shouldBe` Just (-3)
            computeObjValue obj varMap `shouldBe` 13
          _ -> expectationFailure "Unexpected result format"

      it "Mixed bounds: x₁ nonNegative, x₂ with upper bound only (unbounded below)" $ do
        -- x₁ ≥ 0, x₂ ≤ 10 (no lower bound)
        -- Max x₁ + x₂ with x₁ + x₂ ≤ 20
        let obj = Max (M.fromList [(1, 1), (2, 1)])
            constraints = [ LEQ (M.fromList [(1, 1), (2, 1)]) 20 ]
            domainMap = VarDomainMap $ M.fromList [(1, nonNegative), (2, upperBoundOnly 10)]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
            let x1 = M.findWithDefault 0 1 varMap
                x2 = M.findWithDefault 0 2 varMap
            x1 `shouldSatisfy` (>= 0)
            x2 `shouldSatisfy` (<= 10)
            (x1 + x2) `shouldBe` 20
          _ -> expectationFailure "Unexpected result format"

  describe "twoPhaseSimplex with negative lower bounds (Shift transformation)" $ do
    describe "Simple single variable systems" $ do
      it "Max x₁ with x₁ ≤ 5, x₁ ≥ -3: optimal at upper bound x₁=5" $ do
        -- Simple case: maximize x with upper bound 5 and lower bound -3
        -- Optimal should be at x₁ = 5
        let obj = Max (M.fromList [(1, 1)])
            constraints = [ LEQ (M.fromList [(1, 1)]) 5 ]
            domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly (-3))]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just 5
          _ -> expectationFailure "Unexpected result format"

      it "Min x₁ with x₁ ≤ 5, x₁ ≥ -3: optimal at lower bound x₁=-3" $ do
        -- Minimize x with upper bound 5 and lower bound -3
        -- Optimal should be at x₁ = -3
        let obj = Min (M.fromList [(1, 1)])
            constraints = [ LEQ (M.fromList [(1, 1)]) 5 ]
            domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly (-3))]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just (-3)
          _ -> expectationFailure "Unexpected result format"

      it "Max x₁ with x₁ ≥ -10, x₁ ≤ -2: optimal at x₁=-2" $ do
        -- Both bounds are negative, maximize
        let obj = Max (M.fromList [(1, 1)])
            constraints = [ LEQ (M.fromList [(1, 1)]) (-2) ]
            domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly (-10))]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just (-2)
          _ -> expectationFailure "Unexpected result format"

      it "Min x₁ with x₁ ≥ -10, x₁ ≤ -2: optimal at x₁=-10" $ do
        -- Both bounds are negative, minimize
        let obj = Min (M.fromList [(1, 1)])
            constraints = [ LEQ (M.fromList [(1, 1)]) (-2) ]
            domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly (-10))]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just (-10)
          _ -> expectationFailure "Unexpected result format"

    describe "Two variable systems with negative bounds" $ do
      it "Max x₁ + x₂ with x₁ ≥ -2, x₂ ≥ -3, x₁ + x₂ ≤ 10" $ do
        -- Maximize sum, both can go up to contribute to sum ≤ 10
        -- With shifts: x₁' = x₁ + 2, x₂' = x₂ + 3
        -- Constraint becomes: x₁' + x₂' ≤ 15
        -- Optimal in transformed space: x₁' + x₂' = 15
        -- After unapply: x₁ + x₂ = 15 - 5 = 10
        let obj = Max (M.fromList [(1, 1), (2, 1)])
            constraints = [ LEQ (M.fromList [(1, 1), (2, 1)]) 10 ]
            domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly (-2)), (2, lowerBoundOnly (-3))]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
            let x1 = M.findWithDefault 0 1 varMap
                x2 = M.findWithDefault 0 2 varMap
                objVal = computeObjValue obj varMap
            -- Verify the actual objective value
            objVal `shouldBe` 10
            -- Verify lower bounds are respected
            x1 `shouldSatisfy` (>= (-2))
            x2 `shouldSatisfy` (>= (-3))
          _ -> expectationFailure "Unexpected result format"

      it "Min x₁ + x₂ with x₁ ≥ -2, x₂ ≥ -3, x₁ + x₂ ≤ 10" $ do
        -- Minimize sum with lower bounds -2 and -3
        -- Optimal: x₁ = -2, x₂ = -3, sum = -5
        let obj = Min (M.fromList [(1, 1), (2, 1)])
            constraints = [ LEQ (M.fromList [(1, 1), (2, 1)]) 10 ]
            domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly (-2)), (2, lowerBoundOnly (-3))]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
            let objVal = computeObjValue obj varMap
            -- Verify the actual objective value
            objVal `shouldBe` (-5)
            M.lookup 1 varMap `shouldBe` Just (-2)
            M.lookup 2 varMap `shouldBe` Just (-3)
          _ -> expectationFailure "Unexpected result format"

      it "Max 2x₁ - x₂ with x₁ ≥ -5, x₂ ≥ -4, x₁ ≤ 3, x₂ ≤ 6" $ do
        -- Maximize 2x₁ - x₂: want x₁ large (up to 3) and x₂ small (down to -4)
        -- Optimal: x₁ = 3, x₂ = -4, obj = 2*3 - (-4) = 10
        let obj = Max (M.fromList [(1, 2), (2, -1)])
            constraints = 
              [ LEQ (M.fromList [(1, 1)]) 3
              , LEQ (M.fromList [(2, 1)]) 6
              ]
            domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly (-5)), (2, lowerBoundOnly (-4))]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
            let x1 = M.findWithDefault 0 1 varMap
                x2 = M.findWithDefault 0 2 varMap
            M.lookup 1 varMap `shouldBe` Just 3
            M.lookup 2 varMap `shouldBe` Just (-4)
            -- Verify objective value computed from variables
            (2 * x1 - x2) `shouldBe` 10
          _ -> expectationFailure "Unexpected result format"

      it "Min 2x₁ - x₂ with x₁ ≥ -5, x₂ ≥ -4, x₁ ≤ 3, x₂ ≤ 6" $ do
        -- Minimize 2x₁ - x₂: want x₁ small (down to -5) and x₂ large (up to 6)
        -- Optimal: x₁ = -5, x₂ = 6, obj = 2*(-5) - 6 = -16
        let obj = Min (M.fromList [(1, 2), (2, -1)])
            constraints = 
              [ LEQ (M.fromList [(1, 1)]) 3
              , LEQ (M.fromList [(2, 1)]) 6
              ]
            domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly (-5)), (2, lowerBoundOnly (-4))]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
            let x1 = M.findWithDefault 0 1 varMap
                x2 = M.findWithDefault 0 2 varMap
            M.lookup 1 varMap `shouldBe` Just (-5)
            M.lookup 2 varMap `shouldBe` Just 6
            -- Verify objective value computed from variables
            (2 * x1 - x2) `shouldBe` (-16)
          _ -> expectationFailure "Unexpected result format"

    describe "Systems with GEQ constraints and negative bounds" $ do
      it "Max x₁ with x₁ ≥ -5, x₁ ≥ 2 (GEQ tightens bound)" $ do
        -- Lower bound is -5 but GEQ constraint says x₁ ≥ 2
        -- Without upper bound, this is unbounded for Max
        -- Add an upper bound via another constraint
        let obj = Max (M.fromList [(1, 1)])
            constraints = 
              [ GEQ (M.fromList [(1, 1)]) 2
              , LEQ (M.fromList [(1, 1)]) 10
              ]
            domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly (-5))]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just 10
          _ -> expectationFailure "Unexpected result format"

      it "Min x₁ with x₁ ≥ -5, x₁ ≥ 2 (GEQ tightens bound)" $ do
        -- Minimize with GEQ 2, so minimum is at x₁ = 2
        let obj = Min (M.fromList [(1, 1)])
            constraints = 
              [ GEQ (M.fromList [(1, 1)]) 2
              , LEQ (M.fromList [(1, 1)]) 10
              ]
            domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly (-5))]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just 2
          _ -> expectationFailure "Unexpected result format"

    describe "Systems with EQ constraints and negative bounds" $ do
      it "Max x₁ + x₂ with x₁ - x₂ = 0, x₁ ≥ -5, x₂ ≥ -5, x₁ ≤ 10" $ do 
        -- x₁ = x₂, maximize x₁ + x₂ = 2x₁
        -- With x₁ ≤ 10, optimal is x₁ = x₂ = 10, obj = 20
        let obj = Max (M.fromList [(1, 1), (2, 1)])
            constraints = 
              [ EQ (M.fromList [(1, 1), (2, -1)]) 0
              , LEQ (M.fromList [(1, 1)]) 10
              ]
            domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly (-5)), (2, lowerBoundOnly (-5))]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
            let objVal = computeObjValue obj varMap
            M.lookup 1 varMap `shouldBe` Just 10
            M.lookup 2 varMap `shouldBe` Just 10
            -- Verify objective value
            objVal `shouldBe` 20
          _ -> expectationFailure "Unexpected result format"

      it "Min x₁ + x₂ with x₁ - x₂ = 0, x₁ ≥ -5, x₂ ≥ -5, x₁ ≤ 10" $ do
        -- x₁ = x₂, minimize x₁ + x₂ = 2x₁
        -- Lower bound is -5, so optimal is x₁ = x₂ = -5, obj = -10
        let obj = Min (M.fromList [(1, 1), (2, 1)])
            constraints = 
              [ EQ (M.fromList [(1, 1), (2, -1)]) 0
              , LEQ (M.fromList [(1, 1)]) 10
              ]
            domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly (-5)), (2, lowerBoundOnly (-5))]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
            let objVal = computeObjValue obj varMap
            M.lookup 1 varMap `shouldBe` Just (-5)
            M.lookup 2 varMap `shouldBe` Just (-5)
            -- Verify objective value
            objVal `shouldBe` (-10)
          _ -> expectationFailure "Unexpected result format"

    describe "Fractional negative bounds" $ do
      it "Max x₁ with x₁ ≥ -7/2, x₁ ≤ 5/2" $ do
        let obj = Max (M.fromList [(1, 1)])
            constraints = [ LEQ (M.fromList [(1, 1)]) (5 % 2) ]
            domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly ((-7) % 2))]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just (5 % 2)
          _ -> expectationFailure "Unexpected result format"

      it "Min x₁ with x₁ ≥ -7/2, x₁ ≤ 5/2" $ do
        let obj = Min (M.fromList [(1, 1)])
            constraints = [ LEQ (M.fromList [(1, 1)]) (5 % 2) ]
            domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly ((-7) % 2))]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just ((-7) % 2)
          _ -> expectationFailure "Unexpected result format"

  describe "twoPhaseSimplex with unbounded variables (Split transformation)" $ do
    describe "Simple single variable systems" $ do
      it "Max x₁ with -10 ≤ x₁ ≤ 10 (unbounded var with box constraints)" $ do
        -- x₁ is unbounded but constrained by -10 ≤ x₁ ≤ 10
        let obj = Max (M.fromList [(1, 1)])
            constraints = 
              [ LEQ (M.fromList [(1, 1)]) 10
              , GEQ (M.fromList [(1, 1)]) (-10)
              ]
            domainMap = VarDomainMap $ M.fromList [(1, unbounded)]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just 10
          _ -> expectationFailure "Unexpected result format"

      it "Min x₁ with -10 ≤ x₁ ≤ 10 (unbounded var with box constraints)" $ do
        let obj = Min (M.fromList [(1, 1)])
            constraints = 
              [ LEQ (M.fromList [(1, 1)]) 10
              , GEQ (M.fromList [(1, 1)]) (-10)
              ]
            domainMap = VarDomainMap $ M.fromList [(1, unbounded)]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just (-10)
          _ -> expectationFailure "Unexpected result format"

      it "unbounded variable with only upper bound: Min finds negative value" $ do
        -- x₁ unbounded, only x₁ ≤ 5, minimize x₁
        -- This should be unbounded (no finite solution) since x₁ can go to -∞
        let obj = Min (M.fromList [(1, 1)])
            constraints = [ LEQ (M.fromList [(1, 1)]) 5 ]
            domainMap = VarDomainMap $ M.fromList [(1, unbounded)]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        -- This should be unbounded (no finite optimum exists)
        case actualResult of
          SimplexResult (Just _) [ObjectiveResult _ Unbounded] -> pure ()
          _ -> expectationFailure "Expected unbounded system"

    describe "Two variable systems with unbounded variables" $ do
      it "Max x₁ + x₂ with unbounded vars, -5 ≤ x₁ ≤ 5, -3 ≤ x₂ ≤ 7" $ do
        let obj = Max (M.fromList [(1, 1), (2, 1)])
            constraints = 
              [ LEQ (M.fromList [(1, 1)]) 5
              , GEQ (M.fromList [(1, 1)]) (-5)
              , LEQ (M.fromList [(2, 1)]) 7
              , GEQ (M.fromList [(2, 1)]) (-3)
              ]
            domainMap = VarDomainMap $ M.fromList [(1, unbounded), (2, unbounded)]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
            M.lookup 1 varMap `shouldBe` Just 5
            M.lookup 2 varMap `shouldBe` Just 7
            let objVal = computeObjValue obj varMap
            objVal `shouldBe` 12
          _ -> expectationFailure "Unexpected result format"

      it "Min x₁ + x₂ with unbounded vars, -5 ≤ x₁ ≤ 5, -3 ≤ x₂ ≤ 7" $ do
        let obj = Min (M.fromList [(1, 1), (2, 1)])
            constraints = 
              [ LEQ (M.fromList [(1, 1)]) 5
              , GEQ (M.fromList [(1, 1)]) (-5)
              , LEQ (M.fromList [(2, 1)]) 7
              , GEQ (M.fromList [(2, 1)]) (-3)
              ]
            domainMap = VarDomainMap $ M.fromList [(1, unbounded), (2, unbounded)]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
            M.lookup 1 varMap `shouldBe` Just (-5)
            M.lookup 2 varMap `shouldBe` Just (-3)
            let objVal = computeObjValue obj varMap
            objVal `shouldBe` (-8)
          _ -> expectationFailure "Unexpected result format"

      it "Max x₁ - x₂ with unbounded vars: x₁ up, x₂ down" $ do
        -- Maximize x₁ - x₂: want x₁ large (5) and x₂ small (-3)
        let obj = Max (M.fromList [(1, 1), (2, -1)])
            constraints = 
              [ LEQ (M.fromList [(1, 1)]) 5
              , GEQ (M.fromList [(1, 1)]) (-5)
              , LEQ (M.fromList [(2, 1)]) 7
              , GEQ (M.fromList [(2, 1)]) (-3)
              ]
            domainMap = VarDomainMap $ M.fromList [(1, unbounded), (2, unbounded)]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
            M.lookup 1 varMap `shouldBe` Just 5
            M.lookup 2 varMap `shouldBe` Just (-3)
            let objVal = computeObjValue obj varMap
            objVal `shouldBe` 8
          _ -> expectationFailure "Unexpected result format"

    describe "Systems with EQ constraints and unbounded variables" $ do
      it "Max x₁ with x₁ + x₂ = 10, unbounded vars, x₂ ≥ -5" $ do
        -- x₁ + x₂ = 10, x₂ ≥ -5, unbounded x₁
        -- Maximize x₁: make x₂ as small as possible (-5), so x₁ = 15
        let obj = Max (M.fromList [(1, 1)])
            constraints = 
              [ EQ (M.fromList [(1, 1), (2, 1)]) 10
              , GEQ (M.fromList [(2, 1)]) (-5)
              ]
            domainMap = VarDomainMap $ M.fromList [(1, unbounded), (2, unbounded)]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
            M.lookup 1 varMap `shouldBe` Just 15
            M.lookup 2 varMap `shouldBe` Just (-5)
          _ -> expectationFailure "Unexpected result format"

      it "Min x₁ with x₁ + x₂ = 10, unbounded vars, x₂ ≤ 20" $ do
        -- x₁ + x₂ = 10, x₂ ≤ 20, unbounded x₁
        -- Minimize x₁: make x₂ as large as possible (20), so x₁ = -10
        let obj = Min (M.fromList [(1, 1)])
            constraints = 
              [ EQ (M.fromList [(1, 1), (2, 1)]) 10
              , LEQ (M.fromList [(2, 1)]) 20
              ]
            domainMap = VarDomainMap $ M.fromList [(1, unbounded), (2, unbounded)]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
            M.lookup 1 varMap `shouldBe` Just (-10)
            M.lookup 2 varMap `shouldBe` Just 20
          _ -> expectationFailure "Unexpected result format"

  describe "twoPhaseSimplex with mixed domain types" $ do
    describe "NonNegative, negative lower bound, and unbounded in same system" $ do
      it "Max x₁ + x₂ + x₃ with x₁ ≥ 0, x₂ ≥ -5, x₃ unbounded, sum ≤ 20" $ do
        -- x₁ non-negative, x₂ has lower bound -5, x₃ unbounded
        -- All constrained by sum ≤ 20 and individual bounds
        let obj = Max (M.fromList [(1, 1), (2, 1), (3, 1)])
            constraints = 
              [ LEQ (M.fromList [(1, 1), (2, 1), (3, 1)]) 20
              , LEQ (M.fromList [(1, 1)]) 10
              , LEQ (M.fromList [(2, 1)]) 8
              , LEQ (M.fromList [(3, 1)]) 15
              , GEQ (M.fromList [(3, 1)]) (-10)
              ]
            domainMap = VarDomainMap $ M.fromList 
              [(1, nonNegative), (2, lowerBoundOnly (-5)), (3, unbounded)]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
            let objVal = computeObjValue obj varMap
            -- Verify objective value
            objVal `shouldBe` 20
          _ -> expectationFailure "Unexpected result format"

      it "Min x₁ + x₂ + x₃ with x₁ ≥ 0, x₂ ≥ -5, x₃ unbounded, sum ≥ -10" $ do
        -- Minimize sum with lower bound constraint
        let obj = Min (M.fromList [(1, 1), (2, 1), (3, 1)])
            constraints = 
              [ GEQ (M.fromList [(1, 1), (2, 1), (3, 1)]) (-10)
              , LEQ (M.fromList [(1, 1)]) 10
              , LEQ (M.fromList [(2, 1)]) 8
              , LEQ (M.fromList [(3, 1)]) 15
              , GEQ (M.fromList [(3, 1)]) (-20)
              ]
            domainMap = VarDomainMap $ M.fromList 
              [(1, nonNegative), (2, lowerBoundOnly (-5)), (3, unbounded)]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
            let x1 = M.findWithDefault 0 1 varMap
                x2 = M.findWithDefault 0 2 varMap
                x3 = M.findWithDefault 0 3 varMap
                objVal = computeObjValue obj varMap
            -- Verify constraints
            x1 `shouldSatisfy` (>= 0)
            x2 `shouldSatisfy` (>= (-5))
            x3 `shouldSatisfy` (>= (-20))
            -- Verify objective value
            objVal `shouldBe` (-10)
          _ -> expectationFailure "Unexpected result format"

    describe "Positive lower bound with other domain types" $ do
      it "Max 2x₁ + 3x₂ with x₁ ≥ 2 (positive bound), x₂ ≥ -3, 2x₁ + x₂ ≤ 20" $ do
        -- x₁ has positive lower bound (uses AddLowerBound)
        -- x₂ has negative lower bound (uses Shift)
        let obj = Max (M.fromList [(1, 2), (2, 3)])
            constraints = 
              [ LEQ (M.fromList [(1, 2), (2, 1)]) 20
              , LEQ (M.fromList [(2, 1)]) 10
              ]
            domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly 2), (2, lowerBoundOnly (-3))]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
            let x1 = M.findWithDefault 0 1 varMap
                x2 = M.findWithDefault 0 2 varMap
            -- Verify constraints
            x1 `shouldSatisfy` (>= 2)
            x2 `shouldSatisfy` (>= (-3))
            (2 * x1 + x2) `shouldSatisfy` (<= 20)
          _ -> expectationFailure "Unexpected result format"

      it "Min 2x₁ + 3x₂ with x₁ ≥ 2, x₂ ≥ -3, x₁ + x₂ ≥ 0" $ do
        -- Minimize with lower bounds
        -- x₁ = 2 (minimum), x₂ = -2 (to satisfy x₁ + x₂ ≥ 0)
        let obj = Min (M.fromList [(1, 2), (2, 3)])
            constraints = 
              [ GEQ (M.fromList [(1, 1), (2, 1)]) 0
              , LEQ (M.fromList [(1, 1)]) 10
              , LEQ (M.fromList [(2, 1)]) 10
              ]
            domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly 2), (2, lowerBoundOnly (-3))]
        actualResult <-
          runStdoutLoggingT $
            filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
              twoPhaseSimplex domainMap [obj] constraints
        case actualResult of
          SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
          SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
            let x1 = M.findWithDefault 0 1 varMap
                x2 = M.findWithDefault 0 2 varMap
            x1 `shouldSatisfy` (>= 2)
            x2 `shouldSatisfy` (>= (-3))
            (x1 + x2) `shouldSatisfy` (>= 0)
          _ -> expectationFailure "Unexpected result format"

  describe "twoPhaseSimplex edge cases and infeasibility" $ do
    it "Infeasible: negative lower bound conflicts with GEQ constraint" $ do
      -- x₁ ≥ -5 (domain), but x₁ ≥ 10 and x₁ ≤ 5 (constraints conflict)
      let obj = Max (M.fromList [(1, 1)])
          constraints = 
            [ GEQ (M.fromList [(1, 1)]) 10
            , LEQ (M.fromList [(1, 1)]) 5
            ]
          domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly (-5))]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap [obj] constraints
      case actualResult of
        SimplexResult Nothing _ -> pure ()
        _ -> expectationFailure "Expected infeasible system"

    it "Infeasible: unbounded variable with conflicting constraints" $ do
      let obj = Max (M.fromList [(1, 1)])
          constraints = 
            [ GEQ (M.fromList [(1, 1)]) 10
            , LEQ (M.fromList [(1, 1)]) 5
            ]
          domainMap = VarDomainMap $ M.fromList [(1, unbounded)]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap [obj] constraints
      case actualResult of
        SimplexResult Nothing _ -> pure ()
        _ -> expectationFailure "Expected infeasible system"

    it "Variable at exactly zero with negative lower bound" $ do
      -- x₁ ≥ -5, constraint x₁ = 0
      let obj = Max (M.fromList [(1, 1)])
          constraints = [ EQ (M.fromList [(1, 1)]) 0 ]
          domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly (-5))]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap [obj] constraints
      case actualResult of
        SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
        SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just 0
        _ -> expectationFailure "Unexpected result format"

    it "unbounded variable constrained to zero" $ do
      let obj = Max (M.fromList [(1, 1)])
          constraints = [ EQ (M.fromList [(1, 1)]) 0 ]
          domainMap = VarDomainMap $ M.fromList [(1, unbounded)]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap [obj] constraints
      case actualResult of
        SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
        SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> M.lookup 1 varMap `shouldBe` Just 0
        _ -> expectationFailure "Unexpected result format"

    it "Multiple variables, only some with negative bounds" $ do
      -- x₁ ≥ 0 (non-negative), x₂ ≥ -10, x₃ ≥ 0
      -- Max x₁ + x₂ + x₃ with x₁ + x₂ + x₃ ≤ 15
      let obj = Max (M.fromList [(1, 1), (2, 1), (3, 1)])
          constraints = [ LEQ (M.fromList [(1, 1), (2, 1), (3, 1)]) 15 ]
          domainMap = VarDomainMap $ M.fromList 
            [(1, nonNegative), (2, lowerBoundOnly (-10)), (3, nonNegative)]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap [obj] constraints
      case actualResult of
        SimplexResult Nothing _ -> expectationFailure "Expected a solution but got Nothing"
        SimplexResult (Just _) [ObjectiveResult _ (Optimal varMap)] -> do
          let objVal = computeObjValue obj varMap
          -- Verify objective value
          objVal `shouldBe` 15
        _ -> expectationFailure "Unexpected result format"

  -- ===========================================================================
  -- Tests for internal preprocessing functions
  -- ===========================================================================

  describe "collectAllVars" $ do
    describe "Unit tests" $ do
      it "collects variables from Max objective" $ do
        let obj = Max (M.fromList [(1, 3), (2, 5)])
            constraints = []
        collectAllVars [obj] constraints `shouldBe` Set.fromList [1, 2]

      it "collects variables from Min objective" $ do
        let obj = Min (M.fromList [(3, 1), (4, -2)])
            constraints = []
        collectAllVars [obj] constraints `shouldBe` Set.fromList [3, 4]

      it "collects variables from LEQ constraint" $ do
        let obj = Max (M.fromList [(1, 1)])
            constraints = [LEQ (M.fromList [(2, 1), (3, 2)]) 10]
        collectAllVars [obj] constraints `shouldBe` Set.fromList [1, 2, 3]

      it "collects variables from GEQ constraint" $ do
        let obj = Max (M.fromList [(1, 1)])
            constraints = [GEQ (M.fromList [(4, 1)]) 5]
        collectAllVars [obj] constraints `shouldBe` Set.fromList [1, 4]

      it "collects variables from EQ constraint" $ do
        let obj = Max (M.fromList [(1, 1)])
            constraints = [EQ (M.fromList [(5, 2), (6, 3)]) 15]
        collectAllVars [obj] constraints `shouldBe` Set.fromList [1, 5, 6]

      it "collects variables from mixed constraints" $ do
        let obj = Max (M.fromList [(1, 1)])
            constraints = 
              [ LEQ (M.fromList [(2, 1)]) 10
              , GEQ (M.fromList [(3, 1)]) 5
              , EQ (M.fromList [(4, 1)]) 7
              ]
        collectAllVars [obj] constraints `shouldBe` Set.fromList [1, 2, 3, 4]

      it "handles empty objective coefficients" $ do
        let obj = Max M.empty
            constraints = [LEQ (M.fromList [(1, 1)]) 10]
        collectAllVars [obj] constraints `shouldBe` Set.fromList [1]

      it "handles empty constraints" $ do
        let obj = Max (M.fromList [(1, 1), (2, 2)])
            constraints = []
        collectAllVars [obj] constraints `shouldBe` Set.fromList [1, 2]

      it "deduplicates variables appearing in multiple places" $ do
        let obj = Max (M.fromList [(1, 1), (2, 2)])
            constraints = 
              [ LEQ (M.fromList [(1, 3), (3, 4)]) 10
              , GEQ (M.fromList [(2, 5), (3, 6)]) 5
              ]
        collectAllVars [obj] constraints `shouldBe` Set.fromList [1, 2, 3]

  describe "getTransform" $ do
    describe "Unit tests" $ do
      it "returns empty list for NonNegative domain" $ do
        getTransform 10 1 nonNegative `shouldBe` ([], 0)

      it "returns empty list for lowerBoundOnly 0" $ do
        getTransform 10 1 (lowerBoundOnly 0) `shouldBe` ([], 0)

      it "returns AddLowerBound for positive lower bound" $ do
        getTransform 10 1 (lowerBoundOnly 5) `shouldBe` ([AddLowerBound 1 5], 0)

      it "returns AddLowerBound for fractional positive lower bound" $ do
        getTransform 10 1 (lowerBoundOnly (3 % 2)) `shouldBe` ([AddLowerBound 1 (3 % 2)], 0)

      it "returns Shift for negative lower bound" $ do
        getTransform 10 1 (lowerBoundOnly (-5)) `shouldBe` ([Shift 1 10 (-5)], 1)

      it "returns Shift for fractional negative lower bound" $ do
        getTransform 10 1 (lowerBoundOnly ((-7) % 3)) `shouldBe` ([Shift 1 10 ((-7) % 3)], 1)

      it "returns Split for unbounded domain" $ do
        getTransform 10 1 unbounded `shouldBe` ([Split 1 10 11], 2)

      it "returns AddUpperBound for upper bound only" $ do
        getTransform 10 1 (upperBoundOnly 5) `shouldBe` ([Split 1 10 11, AddUpperBound 1 5], 2)

      it "returns AddLowerBound and AddUpperBound for bounded range" $ do
        getTransform 10 1 (boundedRange 2 10) `shouldBe` ([AddLowerBound 1 2, AddUpperBound 1 10], 0)

      it "returns Shift and AddUpperBound for negative lower bound with upper bound" $ do
        getTransform 10 1 (boundedRange (-5) 10) `shouldBe` ([Shift 1 10 (-5), AddUpperBound 1 10], 1)

  describe "generateTransform" $ do
    describe "Unit tests" $ do
      it "generates no transform for NonNegative in domain map" $ do
        let domainMap = M.fromList [(1, nonNegative)]
        generateTransform domainMap 1 ([], 10) `shouldBe` ([], 10)

      it "generates AddLowerBound for positive bound in domain map" $ do
        let domainMap = M.fromList [(1, lowerBoundOnly 5)]
        generateTransform domainMap 1 ([], 10) `shouldBe` ([AddLowerBound 1 5], 10)

      it "generates Shift for negative bound and increments fresh var" $ do
        let domainMap = M.fromList [(1, lowerBoundOnly (-5))]
        generateTransform domainMap 1 ([], 10) `shouldBe` ([Shift 1 10 (-5)], 11)

      it "generates Split for unbounded and increments fresh var by 2" $ do
        let domainMap = M.fromList [(1, unbounded)]
        generateTransform domainMap 1 ([], 10) `shouldBe` ([Split 1 10 11], 12)

      it "treats variable not in domain map as unbounded" $ do
        let domainMap = M.empty
        generateTransform domainMap 1 ([], 10) `shouldBe` ([Split 1 10 11], 12)

      it "accumulates transforms" $ do
        let domainMap = M.fromList [(1, lowerBoundOnly 5)]
            existing = [AddLowerBound 2 3]
        generateTransform domainMap 1 (existing, 10) `shouldBe` ([AddLowerBound 1 5] ++ existing, 10)

      it "generates AddUpperBound for upper bound" $ do
        let domainMap = M.fromList [(1, boundedRange 0 10)]
        generateTransform domainMap 1 ([], 10) `shouldBe` ([AddUpperBound 1 10], 10)

  describe "applyShiftToObjective" $ do
    describe "Unit tests" $ do
      it "substitutes variable in Max objective" $ do
        let obj = Max (M.fromList [(1, 3), (2, 5)])
        applyShiftToObjective 1 10 (-5) obj `shouldBe` Max (M.fromList [(10, 3), (2, 5)])

      it "substitutes variable in Min objective" $ do
        let obj = Min (M.fromList [(1, -2), (2, 4)])
        applyShiftToObjective 1 10 (-3) obj `shouldBe` Min (M.fromList [(10, -2), (2, 4)])

      it "leaves objective unchanged if variable not present" $ do
        let obj = Max (M.fromList [(2, 5), (3, 7)])
        applyShiftToObjective 1 10 (-5) obj `shouldBe` Max (M.fromList [(2, 5), (3, 7)])

      it "preserves coefficient during substitution" $ do
        let obj = Max (M.fromList [(1, 100)])
        applyShiftToObjective 1 10 (-5) obj `shouldBe` Max (M.fromList [(10, 100)])

  describe "applyShiftToConstraint" $ do
    describe "Unit tests" $ do
      it "shifts LEQ constraint correctly" $ do
        -- x1 = x10 + (-5), so x1 has shift -5
        -- constraint: 2*x1 <= 10 becomes 2*x10 <= 10 - 2*(-5) = 20
        let constraint = LEQ (M.fromList [(1, 2)]) 10
        applyShiftToConstraint 1 10 (-5) constraint `shouldBe` LEQ (M.fromList [(10, 2)]) 20

      it "shifts GEQ constraint correctly" $ do
        let constraint = GEQ (M.fromList [(1, 3)]) 6
        applyShiftToConstraint 1 10 (-2) constraint `shouldBe` GEQ (M.fromList [(10, 3)]) 12

      it "shifts EQ constraint correctly" $ do
        let constraint = EQ (M.fromList [(1, 4)]) 8
        applyShiftToConstraint 1 10 (-1) constraint `shouldBe` EQ (M.fromList [(10, 4)]) 12

      it "leaves constraint unchanged if variable not present" $ do
        let constraint = LEQ (M.fromList [(2, 5)]) 10
        applyShiftToConstraint 1 10 (-5) constraint `shouldBe` LEQ (M.fromList [(2, 5)]) 10

      it "handles negative coefficients" $ do
        -- x1 = x10 + (-5), constraint: -3*x1 <= 10
        -- becomes -3*x10 <= 10 - (-3)*(-5) = 10 - 15 = -5
        let constraint = LEQ (M.fromList [(1, -3)]) 10
        applyShiftToConstraint 1 10 (-5) constraint `shouldBe` LEQ (M.fromList [(10, -3)]) (-5)

      it "handles multiple variables in constraint" $ do
        let constraint = LEQ (M.fromList [(1, 2), (2, 3)]) 10
        applyShiftToConstraint 1 10 (-5) constraint `shouldBe` LEQ (M.fromList [(10, 2), (2, 3)]) 20

  describe "applySplitToObjective" $ do
    describe "Unit tests" $ do
      it "splits variable in Max objective" $ do
        let obj = Max (M.fromList [(1, 3)])
        -- x1 = x10 - x11, so coeff 3 -> x10 gets 3, x11 gets -3
        applySplitToObjective 1 10 11 obj `shouldBe` Max (M.fromList [(10, 3), (11, -3)])

      it "splits variable in Min objective" $ do
        let obj = Min (M.fromList [(1, -2)])
        applySplitToObjective 1 10 11 obj `shouldBe` Min (M.fromList [(10, -2), (11, 2)])

      it "leaves objective unchanged if variable not present" $ do
        let obj = Max (M.fromList [(2, 5)])
        applySplitToObjective 1 10 11 obj `shouldBe` Max (M.fromList [(2, 5)])

      it "handles multiple variables" $ do
        let obj = Max (M.fromList [(1, 3), (2, 5)])
        applySplitToObjective 1 10 11 obj `shouldBe` Max (M.fromList [(10, 3), (11, -3), (2, 5)])

  describe "applySplitToConstraint" $ do
    describe "Unit tests" $ do
      it "splits variable in LEQ constraint" $ do
        let constraint = LEQ (M.fromList [(1, 2)]) 10
        applySplitToConstraint 1 10 11 constraint `shouldBe` LEQ (M.fromList [(10, 2), (11, -2)]) 10

      it "splits variable in GEQ constraint" $ do
        let constraint = GEQ (M.fromList [(1, 3)]) 5
        applySplitToConstraint 1 10 11 constraint `shouldBe` GEQ (M.fromList [(10, 3), (11, -3)]) 5

      it "splits variable in EQ constraint" $ do
        let constraint = EQ (M.fromList [(1, 4)]) 8
        applySplitToConstraint 1 10 11 constraint `shouldBe` EQ (M.fromList [(10, 4), (11, -4)]) 8

      it "leaves constraint unchanged if variable not present" $ do
        let constraint = LEQ (M.fromList [(2, 5)]) 10
        applySplitToConstraint 1 10 11 constraint `shouldBe` LEQ (M.fromList [(2, 5)]) 10

      it "handles negative coefficients" $ do
        let constraint = LEQ (M.fromList [(1, -3)]) 10
        applySplitToConstraint 1 10 11 constraint `shouldBe` LEQ (M.fromList [(10, -3), (11, 3)]) 10

      it "handles multiple variables" $ do
        let constraint = LEQ (M.fromList [(1, 2), (2, 3)]) 10
        applySplitToConstraint 1 10 11 constraint `shouldBe` LEQ (M.fromList [(10, 2), (11, -2), (2, 3)]) 10

  describe "applyTransform and applyTransforms" $ do
    describe "Unit tests" $ do
      it "applyTransform AddLowerBound adds GEQ constraint" $ do
        let obj = Max (M.fromList [(1, 1)])
            constraints = [LEQ (M.fromList [(1, 1)]) 10]
            transform = AddLowerBound 1 5
        applyTransform transform (obj, constraints) `shouldBe` 
          (obj, [GEQ (M.singleton 1 1) 5, LEQ (M.fromList [(1, 1)]) 10])

      it "applyTransform Shift transforms objective and constraints" $ do
        let obj = Max (M.fromList [(1, 2)])
            constraints = [LEQ (M.fromList [(1, 1)]) 10]
            transform = Shift 1 10 (-5)
        let (newObj, newConstraints) = applyTransform transform (obj, constraints)
        newObj `shouldBe` Max (M.fromList [(10, 2)])
        newConstraints `shouldBe` [LEQ (M.fromList [(10, 1)]) 15]

      it "applyTransform Split transforms objective and constraints" $ do
        let obj = Max (M.fromList [(1, 3)])
            constraints = [LEQ (M.fromList [(1, 1)]) 10]
            transform = Split 1 10 11
        let (newObj, newConstraints) = applyTransform transform (obj, constraints)
        newObj `shouldBe` Max (M.fromList [(10, 3), (11, -3)])
        newConstraints `shouldBe` [LEQ (M.fromList [(10, 1), (11, -1)]) 10]

      it "applyTransforms applies multiple transforms in order" $ do
        let obj = Max (M.fromList [(1, 1), (2, 1)])
            constraints = [LEQ (M.fromList [(1, 1), (2, 1)]) 10]
            transforms = [AddLowerBound 1 5, AddLowerBound 2 3]
        let (newObj, newConstraints) = applyTransforms transforms obj constraints
        newObj `shouldBe` obj
        -- Two GEQ constraints should be added
        length newConstraints `shouldBe` 3

  describe "unapplyTransformToVarMap and unapplyTransformsToVarMap" $ do
    describe "Unit tests" $ do
      it "unapplyTransformToVarMap AddLowerBound leaves result unchanged" $ do
        let varVals = M.fromList [(1, 7)]
            transform = AddLowerBound 1 5
        unapplyTransformToVarMap transform varVals `shouldBe` varVals

      it "unapplyTransformToVarMap Shift recovers original variable value" $ do
        -- originalVar = shiftedVar + shiftBy
        -- If shiftedVar = 15 and shiftBy = -5, then originalVar = 10
        let varVals = M.fromList [(10, 15)]
            transform = Shift 1 10 (-5)
        let newVarVals = unapplyTransformToVarMap transform varVals
        M.lookup 1 newVarVals `shouldBe` Just 10
        M.lookup 10 newVarVals `shouldBe` Nothing

      it "unapplyTransformToVarMap Split recovers original variable value" $ do
        -- originalVar = posVar - negVar
        -- If posVar = 8 and negVar = 3, then originalVar = 5
        let varVals = M.fromList [(10, 8), (11, 3)]
            transform = Split 1 10 11
        let newVarVals = unapplyTransformToVarMap transform varVals
        M.lookup 1 newVarVals `shouldBe` Just 5
        M.lookup 10 newVarVals `shouldBe` Nothing
        M.lookup 11 newVarVals `shouldBe` Nothing

      it "unapplyTransformToVarMap Split handles negative original value" $ do
        -- originalVar = posVar - negVar
        -- If posVar = 2 and negVar = 7, then originalVar = -5
        let varVals = M.fromList [(10, 2), (11, 7)]
            transform = Split 1 10 11
        let newVarVals = unapplyTransformToVarMap transform varVals
        M.lookup 1 newVarVals `shouldBe` Just (-5)

      it "unapplyTransformsToVarMap applies in correct order (reverse of apply)" $ do
        -- Two shifts: var 1 shifted by -5 to var 10, var 2 shifted by -3 to var 11
        let varVals = M.fromList [(10, 15), (11, 8)]
            transforms = [Shift 1 10 (-5), Shift 2 11 (-3)]
        let newVarVals = unapplyTransformsToVarMap transforms varVals
        M.lookup 1 newVarVals `shouldBe` Just 10
        M.lookup 2 newVarVals `shouldBe` Just 5

  describe "preprocess" $ do
    describe "Unit tests" $ do
      it "returns empty transforms for all NonNegative domains" $ do
        let obj = Max (M.fromList [(1, 1), (2, 1)])
            constraints = [LEQ (M.fromList [(1, 1), (2, 1)]) 10]
            domainMap = VarDomainMap $ M.fromList [(1, nonNegative), (2, nonNegative)]
        let ([newObj], newConstraints, transforms) = preprocess [obj] domainMap constraints
        transforms `shouldBe` []
        newObj `shouldBe` obj
        newConstraints `shouldBe` constraints

      it "generates AddLowerBound for positive lower bounds" $ do
        let obj = Max (M.fromList [(1, 1)])
            constraints = [LEQ (M.fromList [(1, 1)]) 10]
            domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly 5)]
        let (_, newConstraints, transforms) = preprocess [obj] domainMap constraints
        transforms `shouldBe` [AddLowerBound 1 5]
        length newConstraints `shouldBe` 2  -- original + GEQ

      it "generates Shift for negative lower bounds" $ do
        let obj = Max (M.fromList [(1, 1)])
            constraints = [LEQ (M.fromList [(1, 1)]) 10]
            domainMap = VarDomainMap $ M.fromList [(1, lowerBoundOnly (-5))]
        let ([newObj], newConstraints, transforms) = preprocess [obj] domainMap constraints
        length transforms `shouldBe` 1
        case head transforms of
          Shift {..} -> do
            originalVar `shouldBe` 1
            shiftBy `shouldBe` (-5)
          _ -> expectationFailure "Expected Shift transform"

      it "generates Split for unbounded domains" $ do
        let obj = Max (M.fromList [(1, 1)])
            constraints = [LEQ (M.fromList [(1, 1)]) 10]
            domainMap = VarDomainMap $ M.fromList [(1, unbounded)]
        let (_, _, transforms) = preprocess [obj] domainMap constraints
        length transforms `shouldBe` 1
        case head transforms of
          Split {..} -> originalVar `shouldBe` 1
          _ -> expectationFailure "Expected Split transform"

      it "handles mixed domain types" $ do
        let obj = Max (M.fromList [(1, 1), (2, 1), (3, 1)])
            constraints = [LEQ (M.fromList [(1, 1), (2, 1), (3, 1)]) 10]
            domainMap = VarDomainMap $ M.fromList 
              [(1, nonNegative), (2, lowerBoundOnly 5), (3, lowerBoundOnly (-3))]
        let (_, _, transforms) = preprocess [obj] domainMap constraints
        -- Should have AddLowerBound for var 2, Shift for var 3
        length transforms `shouldBe` 2

  -- ===========================================================================
  -- Property-based tests
  -- ===========================================================================

  describe "Property-based tests" $ do
    describe "collectAllVars properties" $ do
      it "result is non-empty when objective is non-empty" $ property $
        \(NonEmpty coeffs :: NonEmptyList (Int, Rational)) ->
          let obj = Max (M.fromList [(abs k `mod` 100 + 1, v) | (k, v) <- coeffs])
          in not (Set.null (collectAllVars [obj] []))

      it "result contains all objective variables" $ property $
        \(vars :: [Int]) ->
          let posVars = filter (> 0) (map abs vars)
              obj = Max (M.fromList [(v, 1) | v <- take 5 posVars])
          in all (`Set.member` collectAllVars [obj] []) (M.keys $ case obj of Max m -> m; Min m -> m)

    describe "getTransform properties" $ do
      it "NonNegative always produces empty list" $ property $
        \(nextVar :: Int) (v :: Int) ->
          getTransform (abs nextVar + 1) (abs v + 1) nonNegative == ([], 0)

      it "lowerBoundOnly 0 produces empty list" $ property $
        \(nextVar :: Int) (v :: Int) ->
          getTransform (abs nextVar + 1) (abs v + 1) (lowerBoundOnly 0) == ([], 0)

      it "positive lowerBoundOnly produces AddLowerBound" $ property $
        \(Positive bound :: Positive Rational) (nextVar :: Int) (v :: Int) ->
          case getTransform (abs nextVar + 1) (abs v + 1) (lowerBoundOnly bound) of
            ([AddLowerBound var b], 0) -> var == abs v + 1 && b == bound
            _ -> False

      it "negative lowerBoundOnly produces Shift" $ property $
        \(Positive bound :: Positive Rational) (nextVar :: Int) (v :: Int) ->
          let negBound = negate bound
          in case getTransform (abs nextVar + 1) (abs v + 1) (lowerBoundOnly negBound) of
            ([Shift origVar _ shiftBy], 1) -> origVar == abs v + 1 && shiftBy == negBound
            _ -> False

      it "unbounded produces Split" $ property $
        \(nextVar :: Int) (v :: Int) ->
          case getTransform (abs nextVar + 1) (abs v + 1) unbounded of
            ([Split origVar _ _], 2) -> origVar == abs v + 1
            _ -> False

      it "boundedRange produces both lower and upper bound transforms" $ property $
        \(lower :: Rational) (Positive delta :: Positive Rational) (nextVar :: Int) (v :: Int) ->
          let upper = lower + delta  -- ensure upper > lower
          in case getTransform (abs nextVar + 1) (abs v + 1) (boundedRange lower upper) of
            (transforms, _) -> 
              any (\case AddUpperBound var u -> var == abs v + 1 && u == upper; _ -> False) transforms

    describe "applyShiftToConstraint properties" $ do
      it "RHS adjustment follows formula: newRHS = oldRHS - coeff * shiftBy" $ property $
        \(coeff :: Rational) (oldRHS :: Rational) (shiftBy :: Rational) ->
          coeff /= 0 ==>
            let constraint = LEQ (M.fromList [(1, coeff)]) oldRHS
                LEQ _ newRHS = applyShiftToConstraint 1 10 shiftBy constraint
            in newRHS == oldRHS - coeff * shiftBy

      it "preserves constraint type (LEQ stays LEQ)" $ property $
        \(coeff :: Rational) (rhs :: Rational) (shiftBy :: Rational) ->
          coeff /= 0 ==>
            let constraint = LEQ (M.fromList [(1, coeff)]) rhs
            in case applyShiftToConstraint 1 10 shiftBy constraint of
                 LEQ {} -> True
                 _ -> False

      it "preserves constraint type (GEQ stays GEQ)" $ property $
        \(coeff :: Rational) (rhs :: Rational) (shiftBy :: Rational) ->
          coeff /= 0 ==>
            let constraint = GEQ (M.fromList [(1, coeff)]) rhs
            in case applyShiftToConstraint 1 10 shiftBy constraint of
                 GEQ {} -> True
                 _ -> False

    describe "applySplitToConstraint properties" $ do
      it "preserves RHS value" $ property $
        \(coeff :: Rational) (rhs :: Rational) ->
          coeff /= 0 ==>
            let constraint = LEQ (M.fromList [(1, coeff)]) rhs
                LEQ _ newRHS = applySplitToConstraint 1 10 11 constraint
            in newRHS == rhs

      it "negVar coefficient is negation of posVar coefficient" $ property $
        \(coeff :: Rational) (rhs :: Rational) ->
          coeff /= 0 ==>
            let constraint = LEQ (M.fromList [(1, coeff)]) rhs
                LEQ m _ = applySplitToConstraint 1 10 11 constraint
                posCoeff = M.findWithDefault 0 10 m
                negCoeff = M.findWithDefault 0 11 m
            in negCoeff == negate posCoeff

    describe "unapplyTransformToVarMap Shift properties" $ do
      it "recovers originalVar = shiftedVar + shiftBy" $ property $
        \(shiftedVal :: Rational) (shiftBy :: Rational) ->
          let varMap = M.fromList [(5, 100), (10, shiftedVal)]
              transform = Shift 1 10 shiftBy
              newVarMap = unapplyTransformToVarMap transform varMap
          in M.lookup 1 newVarMap == Just (shiftedVal + shiftBy)

      it "removes shifted variable from result" $ property $
        \(shiftedVal :: Rational) (shiftBy :: Rational) ->
          let varMap = M.fromList [(5, 100), (10, shiftedVal)]
              transform = Shift 1 10 shiftBy
              newVarMap = unapplyTransformToVarMap transform varMap
          in M.lookup 10 newVarMap == Nothing

    describe "unapplyTransformToVarMap Split properties" $ do
      it "recovers originalVar = posVar - negVar" $ property $
        \(posVal :: Rational) (negVal :: Rational) ->
          let varMap = M.fromList [(5, 100), (10, posVal), (11, negVal)]
              transform = Split 1 10 11
              newVarMap = unapplyTransformToVarMap transform varMap
          in M.lookup 1 newVarMap == Just (posVal - negVal)


      it "removes pos and neg variables from result" $ property $
        \(posVal :: Rational) (negVal :: Rational) ->
          let varMap = M.fromList [(5, 100), (10, posVal), (11, negVal)]
              transform = Split 1 10 11
              newVarMap = unapplyTransformToVarMap transform varMap
          in M.lookup 10 newVarMap == Nothing && 
             M.lookup 11 newVarMap == Nothing

    describe "Round-trip properties" $ do
      it "Shift transform and unapply is identity for variable value" $ property $
        \(origVal :: Rational) (shiftBy :: Rational) ->
          shiftBy < 0 ==>  -- Only negative shifts are valid
            let shiftedVal = origVal - shiftBy  -- shiftedVar = originalVar - shiftBy
                varMap = M.fromList [(5, 100), (10, shiftedVal)]
                transform = Shift 1 10 shiftBy
                newVarMap = unapplyTransformToVarMap transform varMap
            in M.lookup 1 newVarMap == Just origVal

      it "Split with posVal=origVal and negVal=0 gives correct value for positive origVal" $ property $
        \(Positive origVal :: Positive Rational) ->
          let varMap = M.fromList [(5, 100), (10, origVal), (11, 0)]
              transform = Split 1 10 11
              newVarMap = unapplyTransformToVarMap transform varMap
          in M.lookup 1 newVarMap == Just origVal

      it "Split with posVal=0 and negVal=-origVal gives correct value for negative origVal" $ property $
        \(Positive origVal :: Positive Rational) ->
          let negOrigVal = negate origVal
              varMap = M.fromList [(5, 100), (10, 0), (11, origVal)]
              transform = Split 1 10 11
              newVarMap = unapplyTransformToVarMap transform varMap
          in M.lookup 1 newVarMap == Just negOrigVal

  describe "twoPhaseSimplex with multiple objectives" $ do
    it "optimizes two objectives over the same feasible region" $ do
      -- Feasible region: x₁ + x₂ ≤ 10, x₁ ≤ 6, x₂ ≤ 8, x₁,x₂ ≥ 0
      -- Max x₁: optimal at x₁=6, x₂=0 (or x₁=6, x₂=4) with obj=6
      -- Max x₂: optimal at x₁=0, x₂=8 (or x₁=2, x₂=8) with obj=8
      let obj1 = Max (M.fromList [(1, 1)])  -- Max x₁
          obj2 = Max (M.fromList [(2, 1)])  -- Max x₂
          constraints = 
            [ LEQ (M.fromList [(1, 1), (2, 1)]) 10
            , LEQ (M.fromList [(1, 1)]) 6
            , LEQ (M.fromList [(2, 1)]) 8
            ]
          allVars = collectAllVars [obj1, obj2] constraints
          domainMap = VarDomainMap $ M.fromSet (const nonNegative) allVars
      SimplexResult mFeasibleSystem objResults <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap [obj1, obj2] constraints
      -- Should have a feasible system
      mFeasibleSystem `shouldSatisfy` isJust
      -- Should have two results
      length objResults `shouldBe` 2
      -- First result (Max x₁) should have x₁=6
      case objResults !! 0 of
        ObjectiveResult _ (Optimal varVals) -> 
          M.lookup 1 varVals `shouldBe` Just 6
        _ -> expectationFailure "Expected optimal result for obj1"
      -- Second result (Max x₂) should have x₂=8
      case objResults !! 1 of
        ObjectiveResult _ (Optimal varVals) -> 
          M.lookup 2 varVals `shouldBe` Just 8
        _ -> expectationFailure "Expected optimal result for obj2"

    it "handles empty objective list returning feasible system only" $ do
      let constraints = [ LEQ (M.fromList [(1, 1)]) 10 ]
          domainMap = VarDomainMap $ M.fromSet (const nonNegative) (Set.singleton 1)
      SimplexResult mFeasibleSystem objResults <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap [] constraints
      mFeasibleSystem `shouldSatisfy` isJust
      length objResults `shouldBe` 0

    it "handles infeasible system with multiple objectives" $ do
      -- x₁ ≤ 5 and x₁ ≥ 10 is infeasible
      let obj1 = Max (M.fromList [(1, 1)])
          obj2 = Min (M.fromList [(1, 1)])
          constraints = 
            [ LEQ (M.fromList [(1, 1)]) 5
            , GEQ (M.fromList [(1, 1)]) 10
            ]
          allVars = collectAllVars [obj1, obj2] constraints
          domainMap = VarDomainMap $ M.fromSet (const nonNegative) allVars
      SimplexResult mFeasibleSystem objResults <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap [obj1, obj2] constraints
      -- Should be infeasible
      mFeasibleSystem `shouldBe` Nothing
      -- No objective results when infeasible
      length objResults `shouldBe` 0

    it "optimizes Max and Min of same function over feasible region" $ do
      -- Feasible region: 0 ≤ x₁ ≤ 10
      -- Max x₁: optimal at x₁=10
      -- Min x₁: optimal at x₁=0
      let obj1 = Max (M.fromList [(1, 1)])
          obj2 = Min (M.fromList [(1, 1)])
          constraints = [ LEQ (M.fromList [(1, 1)]) 10 ]
          domainMap = VarDomainMap $ M.fromList [(1, nonNegative)]
      SimplexResult mFeasibleSystem objResults <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap [obj1, obj2] constraints
      mFeasibleSystem `shouldSatisfy` isJust
      length objResults `shouldBe` 2
      -- Max x₁ should be 10
      case objResults !! 0 of
        ObjectiveResult _ (Optimal varVals) -> 
          M.lookup 1 varVals `shouldBe` Just 10
        _ -> expectationFailure "Expected optimal result for Max x₁"
      -- Min x₁ should be 0 (or not present in map if zero)
      case objResults !! 1 of
        ObjectiveResult _ (Optimal varVals) -> 
          M.findWithDefault 0 1 varVals `shouldBe` 0
        _ -> expectationFailure "Expected optimal result for Min x₁"

    it "handles one unbounded objective among multiple objectives" $ do
      -- x₁ with only a lower bound (non-negative)
      -- Max x₁: unbounded (no upper constraint)
      -- Min x₁ with x₁ ≥ 0: optimal at x₁=0
      let obj1 = Max (M.fromList [(1, 1)])  -- This will be unbounded
          obj2 = Min (M.fromList [(1, 1)])  -- This will have optimal at 0
          -- Add a dummy constraint to ensure the system is processable
          -- x₁ ≥ 0 (enforced by nonNegative domain) but no upper bound
          constraints = [ GEQ (M.fromList [(1, 1)]) 0 ]  -- x₁ ≥ 0
          domainMap = VarDomainMap $ M.fromList [(1, nonNegative)]
      SimplexResult mFeasibleSystem objResults <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex domainMap [obj1, obj2] constraints
      mFeasibleSystem `shouldSatisfy` isJust
      length objResults `shouldBe` 2
      -- Max x₁ should be unbounded
      case objResults !! 0 of
        ObjectiveResult _ Unbounded -> pure ()
        _ -> expectationFailure "Expected unbounded result for Max x₁"
      -- Min x₁ should be 0
      case objResults !! 1 of
        ObjectiveResult _ (Optimal varVals) -> 
          M.findWithDefault 0 1 varVals `shouldBe` 0
        _ -> expectationFailure "Expected optimal result for Min x₁"
