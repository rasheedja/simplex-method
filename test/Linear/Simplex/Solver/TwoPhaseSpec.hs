module Linear.Simplex.Solver.TwoPhaseSpec where

import Prelude hiding (EQ)

import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Map as M
import Data.Ratio
import Text.InterpolatedString.Perl6

import Test.Hspec
import Test.Hspec.Expectations.Contrib (annotate)

import Linear.Simplex.Prettify
import Linear.Simplex.Solver.TwoPhase
import Linear.Simplex.Types
import Linear.Simplex.Util

-- | Helper to run a test case and check result
runTest :: (ObjectiveFunction, [PolyConstraint]) -> Maybe Result -> IO ()
runTest (obj, constraints) expectedResult = do
  actualResult <-
    runStdoutLoggingT $
      filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
        twoPhaseSimplex obj constraints
  let prettyObj = prettyShowObjectiveFunction obj
      prettyConstraints = map prettyShowPolyConstraint constraints
      expectedObjVal = extractObjectiveValue expectedResult
      actualObjVal = extractObjectiveValue actualResult
  annotate
    [qc|

Objective Function (Non-prettified): {obj}
Constraints        (Non-prettified): {constraints}
====================================
Objective Function     (Prettified): {prettyObj}
Constraints            (Prettified): {prettyConstraints}
====================================
Expected Solution            (Full): {expectedResult}
Actual Solution              (Full): {actualResult}
Expected Solution       (Objective): {expectedObjVal}
Actual Solution         (Objective): {actualObjVal}

    |]
    $ do
      actualResult `shouldBe` expectedResult

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
        runTest testCase (Just (Result 7 (M.fromList [(7, 29), (1, 3), (2, 4)])))

      it "Min 3x₁ + 5x₂ with LEQ constraints: obj=0" $ do
        let testCase =
              ( Min (M.fromList [(1, 3), (2, 5)])
              , [ LEQ (M.fromList [(1, 3), (2, 1)]) 15
                , LEQ (M.fromList [(1, 1), (2, 1)]) 7
                , LEQ (M.fromList [(2, 1)]) 4
                , LEQ (M.fromList [(1, -1), (2, 2)]) 6
                ]
              )
        runTest testCase (Just (Result 7 (M.fromList [(7, 0)])))

      it "Max 3x₁ + 5x₂ with GEQ constraints: infeasible" $ do
        let testCase =
              ( Max (M.fromList [(1, 3), (2, 5)])
              , [ GEQ (M.fromList [(1, 3), (2, 1)]) 15
                , GEQ (M.fromList [(1, 1), (2, 1)]) 7
                , GEQ (M.fromList [(2, 1)]) 4
                , GEQ (M.fromList [(1, -1), (2, 2)]) 6
                ]
              )
        runTest testCase Nothing

      it "Min 3x₁ + 5x₂ with GEQ constraints: obj=237/7, x₁=24/7, x₂=33/7" $ do
        let testCase =
              ( Min (M.fromList [(1, 3), (2, 5)])
              , [ GEQ (M.fromList [(1, 3), (2, 1)]) 15
                , GEQ (M.fromList [(1, 1), (2, 1)]) 7
                , GEQ (M.fromList [(2, 1)]) 4
                , GEQ (M.fromList [(1, -1), (2, 2)]) 6
                ]
              )
        runTest testCase (Just (Result 11 (M.fromList [(11, 237 % 7), (1, 24 % 7), (2, 33 % 7)])))

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
        runTest testCase (Just (Result 9 (M.fromList [(9, 3 % 5), (2, 14 % 5), (3, 17 % 5)])))

      it "Min x₁ - x₂ + x₃ with LEQ constraints: infeasible" $ do
        let testCase =
              ( Min (M.fromList [(1, 1), (2, -1), (3, 1)])
              , [ LEQ (M.fromList [(1, 2), (2, -1), (3, 2)]) 4
                , LEQ (M.fromList [(1, 2), (2, -3), (3, 1)]) (-5)
                , LEQ (M.fromList [(1, -1), (2, 1), (3, -2)]) (-1)
                ]
              )
        runTest testCase Nothing

      it "Max x₁ - x₂ + x₃ with GEQ constraints: obj=1, x₁=3, x₂=2" $ do
        let testCase =
              ( Max (M.fromList [(1, 1), (2, -1), (3, 1)])
              , [ GEQ (M.fromList [(1, 2), (2, -1), (3, 2)]) 4
                , GEQ (M.fromList [(1, 2), (2, -3), (3, 1)]) (-5)
                , GEQ (M.fromList [(1, -1), (2, 1), (3, -2)]) (-1)
                ]
              )
        runTest testCase (Just (Result 8 (M.fromList [(8, 1), (2, 2), (1, 3)])))

      it "Min x₁ - x₂ + x₃ with GEQ constraints: obj=-1/4, x₁=17/4, x₂=9/2" $ do
        let testCase =
              ( Min (M.fromList [(1, 1), (2, -1), (3, 1)])
              , [ GEQ (M.fromList [(1, 2), (2, -1), (3, 2)]) 4
                , GEQ (M.fromList [(1, 2), (2, -3), (3, 1)]) (-5)
                , GEQ (M.fromList [(1, -1), (2, 1), (3, -2)]) (-1)
                ]
              )
        runTest testCase (Just (Result 8 (M.fromList [(8, (-1) % 4), (2, 9 % 2), (1, 17 % 4)])))

    -- From page 49 of 'Linear and Integer Programming Made Easy' (requires two phases)
    describe "From 'Linear and Integer Programming Made Easy' (page 49, requires two phases)" $ do
      it "Min x₁ + x₂ + 2x₃ + x₄ with EQ constraints: obj=5, x₃=2, x₄=1" $ do
        let testCase =
              ( Min (M.fromList [(1, 1), (2, 1), (3, 2), (4, 1)])
              , [ EQ (M.fromList [(1, 1), (3, 2), (4, -2)]) 2
                , EQ (M.fromList [(2, 1), (3, 1), (4, 4)]) 6
                ]
              )
        runTest testCase (Just (Result 7 (M.fromList [(7, 5), (3, 2), (4, 1)])))

      it "Max x₁ + x₂ + 2x₃ + x₄ with EQ constraints: obj=8, x₁=2, x₂=6" $ do
        let testCase =
              ( Max (M.fromList [(1, 1), (2, 1), (3, 2), (4, 1)])
              , [ EQ (M.fromList [(1, 1), (3, 2), (4, -2)]) 2
                , EQ (M.fromList [(2, 1), (3, 1), (4, 4)]) 6
                ]
              )
        runTest testCase (Just (Result 7 (M.fromList [(7, 8), (1, 2), (2, 6)])))

    -- From page 52 of 'Linear and Integer Programming Made Easy'
    describe "From 'Linear and Integer Programming Made Easy' (page 52)" $ do
      it "Max -2x₃ + 2x₄ + x₅ with EQ constraints: obj=20, x₃=6, x₄=16" $ do
        let testCase =
              ( Max (M.fromList [(3, -2), (4, 2), (5, 1)])
              , [ EQ (M.fromList [(3, -2), (4, 1), (5, 1)]) 4
                , EQ (M.fromList [(3, 3), (4, -1), (5, 2)]) 2
                ]
              )
        runTest testCase (Just (Result 8 (M.fromList [(8, 20), (4, 16), (3, 6)])))

      it "Min -2x₃ + 2x₄ + x₅ with EQ constraints: obj=6, x₄=2, x₅=2" $ do
        let testCase =
              ( Min (M.fromList [(3, -2), (4, 2), (5, 1)])
              , [ EQ (M.fromList [(3, -2), (4, 1), (5, 1)]) 4
                , EQ (M.fromList [(3, 3), (4, -1), (5, 2)]) 2
                ]
              )
        runTest testCase (Just (Result 8 (M.fromList [(8, 6), (4, 2), (5, 2)])))

    -- From page 59 of 'Linear and Integer Programming Made Easy' (requires two phases)
    describe "From 'Linear and Integer Programming Made Easy' (page 59, requires two phases)" $ do
      it "Max 2x₁ + x₂: obj=150, x₂=150" $ do
        let testCase =
              ( Max (M.fromList [(1, 2), (2, 1)])
              , [ LEQ (M.fromList [(1, 4), (2, 1)]) 150
                , LEQ (M.fromList [(1, 2), (2, -3)]) (-40)
                ]
              )
        runTest testCase (Just (Result 6 (M.fromList [(6, 150), (2, 150)])))

      it "Min 2x₁ + x₂: obj=40/3, x₂=40/3" $ do
        let testCase =
              ( Min (M.fromList [(1, 2), (2, 1)])
              , [ LEQ (M.fromList [(1, 4), (2, 1)]) 150
                , LEQ (M.fromList [(1, 2), (2, -3)]) (-40)
                ]
              )
        runTest testCase (Just (Result 6 (M.fromList [(6, 40 % 3), (2, 40 % 3)])))

      it "Max 2x₁ + x₂ with GEQ constraints: infeasible" $ do
        let testCase =
              ( Max (M.fromList [(1, 2), (2, 1)])
              , [ GEQ (M.fromList [(1, 4), (2, 1)]) 150
                , GEQ (M.fromList [(1, 2), (2, -3)]) (-40)
                ]
              )
        runTest testCase Nothing

      it "Min 2x₁ + x₂ with GEQ constraints: obj=75, x₁=75/2" $ do
        let testCase =
              ( Min (M.fromList [(1, 2), (2, 1)])
              , [ GEQ (M.fromList [(1, 4), (2, 1)]) 150
                , GEQ (M.fromList [(1, 2), (2, -3)]) (-40)
                ]
              )
        runTest testCase (Just (Result 6 (M.fromList [(6, 75), (1, 75 % 2)])))

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
        runTest testCase (Just (Result 7 (M.fromList [(7, (-120)), (1, 20)])))

      it "Max -6x₁ - 4x₂ + 2x₃: obj=10, x₃=5" $ do
        let testCase =
              ( Max (M.fromList [(1, -6), (2, -4), (3, 2)])
              , [ LEQ (M.fromList [(1, 1), (2, 1), (3, 4)]) 20
                , LEQ (M.fromList [(2, -5), (3, 5)]) 100
                , LEQ (M.fromList [(1, 1), (3, 1), (1, 1)]) 400
                ]
              )
        runTest testCase (Just (Result 7 (M.fromList [(7, 10), (3, 5)])))

      it "Min -6x₁ - 4x₂ + 2x₃ with GEQ constraints: infeasible" $ do
        let testCase =
              ( Min (M.fromList [(1, -6), (2, -4), (3, 2)])
              , [ GEQ (M.fromList [(1, 1), (2, 1), (3, 4)]) 20
                , GEQ (M.fromList [(2, -5), (3, 5)]) 100
                , GEQ (M.fromList [(1, 1), (3, 1), (1, 1)]) 400
                ]
              )
        runTest testCase Nothing

      it "Max -6x₁ - 4x₂ + 2x₃ with GEQ constraints: infeasible" $ do
        let testCase =
              ( Max (M.fromList [(1, -6), (2, -4), (3, 2)])
              , [ GEQ (M.fromList [(1, 1), (2, 1), (3, 4)]) 20
                , GEQ (M.fromList [(2, -5), (3, 5)]) 100
                , GEQ (M.fromList [(1, 1), (3, 1), (1, 1)]) 400
                ]
              )
        runTest testCase Nothing

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
        runTest testCase (Just (Result 7 (M.fromList [(7, 250), (2, 50)])))

      it "Min 3x₁ + 5x₂ + 2x₃: obj=0" $ do
        let testCase =
              ( Min (M.fromList [(1, 3), (2, 5), (3, 2)])
              , [ LEQ (M.fromList [(1, 5), (2, 1), (3, 4)]) 50
                , LEQ (M.fromList [(1, 1), (2, -1), (3, 1)]) 150
                , LEQ (M.fromList [(1, 2), (2, 1), (3, 2)]) 100
                ]
              )
        runTest testCase (Just (Result 7 (M.fromList [(7, 0)])))

      it "Max 3x₁ + 5x₂ + 2x₃ with GEQ constraints: infeasible" $ do
        let testCase =
              ( Max (M.fromList [(1, 3), (2, 5), (3, 2)])
              , [ GEQ (M.fromList [(1, 5), (2, 1), (3, 4)]) 50
                , GEQ (M.fromList [(1, 1), (2, -1), (3, 1)]) 150
                , GEQ (M.fromList [(1, 2), (2, 1), (3, 2)]) 100
                ]
              )
        runTest testCase Nothing

      it "Min 3x₁ + 5x₂ + 2x₃ with GEQ constraints: obj=300, x₃=150" $ do
        let testCase =
              ( Min (M.fromList [(1, 3), (2, 5), (3, 2)])
              , [ GEQ (M.fromList [(1, 5), (2, 1), (3, 4)]) 50
                , GEQ (M.fromList [(1, 1), (2, -1), (3, 1)]) 150
                , GEQ (M.fromList [(1, 2), (2, 1), (3, 2)]) 100
                ]
              )
        runTest testCase (Just (Result 10 (M.fromList [(10, 300), (3, 150)])))

    describe "Simple single/two variable tests" $ do
      it "Max x₁ with x₁ <= 15: obj=15, x₁=15" $ do
        let testCase =
              ( Max (M.fromList [(1, 1)])
              , [ LEQ (M.fromList [(1, 1)]) 15
                ]
              )
        runTest testCase (Just (Result 3 (M.fromList [(3, 15), (1, 15)])))

      it "Max 2x₁ with mixed constraints: obj=20, x₁=10, x₂=10" $ do
        let testCase =
              ( Max (M.fromList [(1, 2)])
              , [ LEQ (M.fromList [(1, 2)]) 20
                , GEQ (M.fromList [(2, 1)]) 10
                ]
              )
        runTest testCase (Just (Result 6 (M.fromList [(6, 20), (1, 10), (2, 10)])))

      it "Min x₁ with x₁ <= 15: obj=0" $ do
        let testCase =
              ( Min (M.fromList [(1, 1)])
              , [ LEQ (M.fromList [(1, 1)]) 15
                ]
              )
        runTest testCase (Just (Result 3 (M.fromList [(3, 0)])))

      it "Min 2x₁ with mixed constraints: obj=0, x₂=10" $ do
        let testCase =
              ( Min (M.fromList [(1, 2)])
              , [ LEQ (M.fromList [(1, 2)]) 20
                , GEQ (M.fromList [(2, 1)]) 10
                ]
              )
        runTest testCase (Just (Result 6 (M.fromList [(6, 0), (2, 10)])))

    describe "Infeasibility tests" $ do
      it "Conflicting bounds x₁ <= 15 and x₁ >= 15.01: infeasible" $ do
        let testCase =
              ( Max (M.fromList [(1, 1)])
              , [ LEQ (M.fromList [(1, 1)]) 15
                , GEQ (M.fromList [(1, 1)]) 15.01
                ]
              )
        runTest testCase Nothing

      it "Conflicting bounds with additional constraint: infeasible" $ do
        let testCase =
              ( Max (M.fromList [(1, 1)])
              , [ LEQ (M.fromList [(1, 1)]) 15
                , GEQ (M.fromList [(1, 1)]) 15.01
                , GEQ (M.fromList [(2, 1)]) 10
                ]
              )
        runTest testCase Nothing

      it "Min x₁ with duplicate GEQ constraints: obj=0, x₂=1" $ do
        let testCase =
              ( Min (M.fromList [(1, 1)])
              , [ GEQ (M.fromList [(1, 1), (2, 1)]) 1
                , GEQ (M.fromList [(1, 1), (2, 1)]) 1
                ]
              )
        runTest testCase (Just (Result 5 (M.fromList [(2, 1 % 1), (5, 0 % 1)])))

      it "Conflicting x₁+x₂ >= 2 and x₁+x₂ <= 1: infeasible" $ do
        let testCase =
              ( Min (M.fromList [(1, 1)])
              , [ GEQ (M.fromList [(1, 1), (2, 1)]) 2
                , LEQ (M.fromList [(1, 1), (2, 1)]) 1
                ]
              )
        runTest testCase Nothing

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
        runTest testCase (Just (Result 5 (M.fromList [(5, 3), (1, 3), (2, 3)])))

      it "testLeqGeqBugMax1: obj=3, x₁=3, x₂=3" $ do
        let testCase =
              ( Min (M.fromList [(1, 1)])
              , [ GEQ (M.fromList [(1, 1)]) 3
                , LEQ (M.fromList [(1, 1)]) 3
                , GEQ (M.fromList [(2, 1)]) 3
                , LEQ (M.fromList [(2, 1)]) 3
                ]
              )
        runTest testCase (Just (Result 5 (M.fromList [(5, 3), (1, 3), (2, 3)])))

      it "testLeqGeqBugMin2: obj=3, x₁=3, x₂=3" $ do
        let testCase =
              ( Min (M.fromList [(1, 1)])
              , [ GEQ (M.fromList [(1, 1)]) 3
                , LEQ (M.fromList [(1, 1)]) 3
                , GEQ (M.fromList [(2, 1)]) 3
                , LEQ (M.fromList [(2, 1)]) 3
                ]
              )
        runTest testCase (Just (Result 5 (M.fromList [(5, 3), (1, 3), (2, 3)])))

      it "testLeqGeqBugMax2: obj=3, x₁=3, x₂=3" $ do
        let testCase =
              ( Min (M.fromList [(1, 1)])
              , [ GEQ (M.fromList [(1, 1)]) 3
                , LEQ (M.fromList [(1, 1)]) 3
                , GEQ (M.fromList [(2, 1)]) 3
                , LEQ (M.fromList [(2, 1)]) 3
                ]
              )
        runTest testCase (Just (Result 5 (M.fromList [(5, 3), (1, 3), (2, 3)])))

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
                (Just (Result 12 (M.fromList [(12, 7 % 4), (2, 5 % 2), (1, 7 % 4), (3, 0)])))

      it "Max x₁: x₁=5/2, x₂=5/3" $ do
        runTest (mkConstraints (Max (M.fromList [(1, 1)]))) 
                (Just (Result 12 (M.fromList [(12, 5 % 2), (2, 5 % 3), (1, 5 % 2), (3, 0)])))

      it "Min x₂: x₂=5/3" $ do
        runTest (mkConstraints (Min (M.fromList [(2, 1)]))) 
                (Just (Result 12 (M.fromList [(12, 5 % 3), (2, 5 % 3), (1, 5 % 2), (3, 0)])))

      it "Max x₂: x₂=5/2" $ do
        runTest (mkConstraints (Max (M.fromList [(2, 1)]))) 
                (Just (Result 12 (M.fromList [(12, 5 % 2), (2, 5 % 2), (1, 5 % 2), (3, 0)])))

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
        runTest (mkConstraints (Max (M.fromList [(1, 1)]))) Nothing

      it "Min x₁: infeasible" $ do
        runTest (mkConstraints (Min (M.fromList [(1, 1)]))) Nothing

      it "Max x₂: infeasible" $ do
        runTest (mkConstraints (Max (M.fromList [(2, 1)]))) Nothing

      it "Min x₂: infeasible" $ do
        runTest (mkConstraints (Min (M.fromList [(2, 1)]))) Nothing

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
                (Just (Result 12 (M.fromList [(12, 7 % 2), (2, 5 % 9), (1, 7 % 2), (3, 0)])))

      it "Min x₁: x₁=17/20" $ do
        runTest (mkConstraints (Min (M.fromList [(1, 1)]))) 
                (Just (Result 12 (M.fromList [(12, 17 % 20), (2, 7 % 2), (1, 17 % 20), (3, 0)])))

      it "Max x₂: x₂=7/2" $ do
        runTest (mkConstraints (Max (M.fromList [(2, 1)]))) 
                (Just (Result 12 (M.fromList [(12, 7 % 2), (2, 7 % 2), (1, 22 % 9)])))

      it "Min x₂: x₂=5/9" $ do
        runTest (mkConstraints (Min (M.fromList [(2, 1)]))) 
                (Just (Result 12 (M.fromList [(12, 5 % 9), (2, 5 % 9), (1, 7 % 2), (3, 0)])))

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
        runTest (mkConstraints (Max (M.fromList [(1, 1)]))) Nothing

      it "Min x₁: infeasible" $ do
        runTest (mkConstraints (Min (M.fromList [(1, 1)]))) Nothing

      it "Max x₂: infeasible" $ do
        runTest (mkConstraints (Max (M.fromList [(2, 1)]))) Nothing

      it "Min x₂: infeasible" $ do
        runTest (mkConstraints (Min (M.fromList [(2, 1)]))) Nothing

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
                (Just (Result 17 (M.fromList [(17, 5 % 2), (2, 45 % 22), (1, 5 % 2), (4, 0)])))

      it "Min x₁: x₁=45/22" $ do
        runTest (mkConstraints (Min (M.fromList [(1, 1)]))) 
                (Just (Result 17 (M.fromList [(17, 45 % 22), (2, 5 % 2), (1, 45 % 22), (4, 0)])))

      it "Max x₂: x₂=5/2" $ do
        runTest (mkConstraints (Max (M.fromList [(2, 1)]))) 
                (Just (Result 17 (M.fromList [(17, 5 % 2), (2, 5 % 2), (1, 5 % 2), (4, 0)])))

      it "Min x₂: x₂=45/22" $ do
        runTest (mkConstraints (Min (M.fromList [(2, 1)]))) 
                (Just (Result 17 (M.fromList [(17, 45 % 22), (2, 45 % 22), (1, 5 % 2), (4, 0)])))

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
        runTest testCase (Just (Result 10 (M.fromList [(10, (-370)), (2, 26), (1, 5 % 3)])))

      it "testQuickCheck2: obj=-2/9, x₁=14/9, x₂=8/9" $ do
        let testCase =
              ( Max (M.fromList [(1, -3), (2, 5)])
              , [ LEQ (M.fromList [(1, -6), (2, 6)]) 4
                , LEQ (M.fromList [(1, 1), (2, -4), (3, 3)]) (-2)
                , LEQ (M.fromList [(2, 7), (1, -4)]) 0
                ]
              )
        runTest testCase (Just (Result 8 (M.fromList [(8, (-2) % 9), (1, 14 % 9), (2, 8 % 9)])))

      it "testQuickCheck3 (tests objective simplification): obj=-8, x₂=2" $ do
        let testCase =
              ( Min (M.fromList [(2, 0), (2, -4)])
              , [ GEQ (M.fromList [(1, 5), (2, 4)]) (-4)
                , LEQ (M.fromList [(1, -1), (2, -1)]) 2
                , LEQ (M.fromList [(2, 1)]) 2
                , GEQ (M.fromList [(1, -5), (2, -1), (2, 1)]) (-5)
                ]
              )
        runTest testCase (Just (Result 7 (M.fromList [(7, (-8)), (2, 2)])))

  describe "twoPhaseSimplex' (with VarDomainMap)" $ do
    it "NonNegative domain gives same result as twoPhaseSimplex" $ do
      let obj = Max (M.fromList [(1, 3), (2, 5)])
          constraints = 
            [ LEQ (M.fromList [(1, 3), (2, 1)]) 15
            , LEQ (M.fromList [(1, 1), (2, 1)]) 7
            , LEQ (M.fromList [(2, 1)]) 4
            , LEQ (M.fromList [(1, -1), (2, 2)]) 6
            ]
          domainMap = VarDomainMap $ M.fromList [(1, NonNegative), (2, NonNegative)]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex' domainMap obj constraints
      actualResult `shouldBe` Just (Result 7 (M.fromList [(7, 29), (1, 3), (2, 4)]))

    it "Shift transformation with negative lower bound" $ do
      let obj = Max (M.fromList [(1, 1)])
          constraints = [ LEQ (M.fromList [(1, 1)]) 10 ]
          domainMap = VarDomainMap $ M.fromList [(1, LowerBound (-5))]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex' domainMap obj constraints
      case actualResult of
        Nothing -> expectationFailure "Expected a solution but got Nothing"
        Just result -> M.lookup 1 (varValMap result) `shouldBe` Just 10

    it "Shift transformation finds minimum at negative bound" $ do
      let obj = Min (M.fromList [(1, 1)])
          constraints = [ LEQ (M.fromList [(1, 1)]) 0 ]
          domainMap = VarDomainMap $ M.fromList [(1, LowerBound (-5))]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex' domainMap obj constraints
      case actualResult of
        Nothing -> expectationFailure "Expected a solution but got Nothing"
        Just result -> M.lookup 1 (varValMap result) `shouldBe` Just (-5)

    it "Split transformation for unbounded variable (max)" $ do
      let obj = Max (M.fromList [(1, 1)])
          constraints = 
            [ LEQ (M.fromList [(1, 1)]) 10
            , GEQ (M.fromList [(1, 1)]) (-10)
            ]
          domainMap = VarDomainMap $ M.fromList [(1, Unbounded)]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex' domainMap obj constraints
      case actualResult of
        Nothing -> expectationFailure "Expected a solution but got Nothing"
        Just result -> M.lookup 1 (varValMap result) `shouldBe` Just 10

    it "Split transformation for unbounded variable (min)" $ do
      let obj = Min (M.fromList [(1, 1)])
          constraints = 
            [ LEQ (M.fromList [(1, 1)]) 10
            , GEQ (M.fromList [(1, 1)]) (-10)
            ]
          domainMap = VarDomainMap $ M.fromList [(1, Unbounded)]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex' domainMap obj constraints
      case actualResult of
        Nothing -> expectationFailure "Expected a solution but got Nothing"
        Just result -> M.lookup 1 (varValMap result) `shouldBe` Just (-10)

    it "AddLowerBound with positive lower bound" $ do
      let obj = Max (M.fromList [(1, 1)])
          constraints = [ LEQ (M.fromList [(1, 1)]) 10 ]
          domainMap = VarDomainMap $ M.fromList [(1, LowerBound 5)]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex' domainMap obj constraints
      case actualResult of
        Nothing -> expectationFailure "Expected a solution but got Nothing"
        Just result -> M.lookup 1 (varValMap result) `shouldBe` Just 10

    it "AddLowerBound finds minimum at positive bound" $ do
      let obj = Min (M.fromList [(1, 1)])
          constraints = [ LEQ (M.fromList [(1, 1)]) 10 ]
          domainMap = VarDomainMap $ M.fromList [(1, LowerBound 5)]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex' domainMap obj constraints
      case actualResult of
        Nothing -> expectationFailure "Expected a solution but got Nothing"
        Just result -> M.lookup 1 (varValMap result) `shouldBe` Just 5

    it "Mixed domain types" $ do
      let obj = Max (M.fromList [(1, 1), (2, 1)])
          constraints = 
            [ LEQ (M.fromList [(1, 1), (2, 1)]) 5
            , GEQ (M.fromList [(2, 1)]) (-3)
            ]
          domainMap = VarDomainMap $ M.fromList [(1, LowerBound (-2)), (2, Unbounded)]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex' domainMap obj constraints
      case actualResult of
        Nothing -> expectationFailure "Expected a solution but got Nothing"
        Just result -> do
          let xVal = M.findWithDefault 0 1 (varValMap result)
              yVal = M.findWithDefault 0 2 (varValMap result)
          (xVal + yVal) `shouldBe` 5

    it "LowerBound 0 is equivalent to NonNegative" $ do
      let obj = Max (M.fromList [(1, 3), (2, 5)])
          constraints = 
            [ LEQ (M.fromList [(1, 3), (2, 1)]) 15
            , LEQ (M.fromList [(1, 1), (2, 1)]) 7
            , LEQ (M.fromList [(2, 1)]) 4
            , LEQ (M.fromList [(1, -1), (2, 2)]) 6
            ]
          domainMap = VarDomainMap $ M.fromList [(1, LowerBound 0), (2, LowerBound 0)]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex' domainMap obj constraints
      actualResult `shouldBe` Just (Result 7 (M.fromList [(7, 29), (1, 3), (2, 4)]))

    it "Infeasible system with domain constraint" $ do
      let obj = Max (M.fromList [(1, 1)])
          constraints = [ LEQ (M.fromList [(1, 1)]) 5 ]
          domainMap = VarDomainMap $ M.fromList [(1, LowerBound 10)]
      actualResult <-
        runStdoutLoggingT $
          filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $
            twoPhaseSimplex' domainMap obj constraints
      actualResult `shouldBe` Nothing
