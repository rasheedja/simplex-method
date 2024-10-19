-- |
-- Module: Linear.System.Simple.TypesSpec
-- Description: Tests for Linear.System.Simple.Types
-- Copyright: (c) Junaid Rasheed, 2020-2024
-- License: BSD-3
-- Maintainer: Junaid Rasheed <jrasheed178@gmail.com
-- Stability: experimental
module Linear.System.Simple.UtilSpec where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Linear.Constraint.Generic.Types
  ( GenericConstraint ((:<=), (:>=))
  )
import Linear.Expr.Types (ExprVarsOnly (..))
import Linear.System.Simple.Types
  ( SimpleSystem (SimpleSystem)
  , findHighestVar
  , simpleSystemVars
  , simplifySimpleSystem
  )
import Linear.System.Simple.Util
  ( deriveBounds
  , removeUselessSystemBounds
  )
import Linear.Term.Types (TermVarsOnly (..))
import Linear.Var.Types (Bounds (..))
import Linear.Var.Util (validateBounds)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (counterexample)
import TestUtil (evalSimpleSystem, genVarMap)
import Linear.Constraint.Simple.Types (SimpleConstraint(..))

spec :: Spec
spec = do
  describe "SimpleSystem" $ do
    prop "simplifySimpleSystem leads to the same evaluation" $ \simpleSystem -> do
      let vars = Set.toList $ simpleSystemVars simpleSystem
      varMap <- genVarMap vars
      let simplifiedSimpleSystem = simplifySimpleSystem simpleSystem
          simpleSystemEval = evalSimpleSystem varMap simpleSystem
          simplifiedSimpleSystemEval = evalSimpleSystem varMap simplifiedSimpleSystem
      pure
        $ counterexample
          ( "simpleSystem: "
              <> show simpleSystem
              <> "\nsimplifiedSimpleSystem: "
              <> show simplifiedSimpleSystem
              <> "\ninitialVarMap: "
              <> show varMap
              <> "\nsimpleSystemEval: "
              <> show simpleSystemEval
              <> "\nsimplifiedSimpleSystemEval: "
              <> show simplifiedSimpleSystemEval
          )
        $ simpleSystemEval == simplifiedSimpleSystemEval
    it "findHighestVar finds the highest variable in a simple system" $ do
      let simpleSystem1 =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :>= 0
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 1
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 1] :>= 0
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 1] :<= 1
              ]
          simpleSystem100 =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 1
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 50] :<= 1
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 100] :<= 1
              ]
          simpleSystem10 =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO (-10)] :<= 1
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 1
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 10] :<= 1
              ]
          simpleSystemMinus10 =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO (-10)] :<= 1
              , SimpleConstraint $ ExprVarsOnly [VarTermVO (-20)] :<= 1
              ]

      findHighestVar (SimpleSystem []) `shouldBe` Nothing
      findHighestVar simpleSystem1 `shouldBe` Just 1
      findHighestVar simpleSystem100 `shouldBe` Just 100
      findHighestVar simpleSystem10 `shouldBe` Just 10
      findHighestVar simpleSystemMinus10 `shouldBe` Just (-10)
  describe "Bounds" $ it
    "validateBounds finds that deriving bounds for a system where -1 <= x <= 1 has valid bounds"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :>= (-1)
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 1
              ]
          derivedBounds = deriveBounds simpleSystem
          expectedBounds = Map.fromList [(0, Bounds (Just (-1)) (Just 1))]
      derivedBounds `shouldBe` expectedBounds
      validateBounds derivedBounds `shouldBe` True
  it
    "validateBounds finds that deriving bounds for a system where 0 <= x <= 1 has valid bounds"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :>= 0
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 1
              ]
          derivedBounds = deriveBounds simpleSystem
          expectedBounds = Map.fromList [(0, Bounds (Just 0) (Just 1))]
      derivedBounds `shouldBe` expectedBounds
      validateBounds derivedBounds `shouldBe` True
  it
    "validateBounds finds that deriving bounds for a system where 1 <= x <= 1 has valid bounds"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :>= 1
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 1
              ]
          derivedBounds = deriveBounds simpleSystem
          expectedBounds = Map.fromList [(0, Bounds (Just 1) (Just 1))]
      derivedBounds `shouldBe` expectedBounds
      validateBounds derivedBounds `shouldBe` True
  it
    "validateBounds finds that deriving bounds for a system where 1 <= x <= 0 has invalid bounds"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :>= 1
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 0
              ]
          derivedBounds = deriveBounds simpleSystem
          expectedBounds = Map.fromList [(0, Bounds (Just 1) (Just 0))]
      derivedBounds `shouldBe` expectedBounds
      validateBounds derivedBounds `shouldBe` False
  it
    "validateBounds finds that deriving bounds for a system where 0 <= x <= 1 and 1 <= y <= 3 has valid bounds"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :>= 0
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 1
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 1] :>= 1
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 1] :<= 3
              ]
          derivedBounds = deriveBounds simpleSystem
          expectedBounds = Map.fromList [(0, Bounds (Just 0) (Just 1)), (1, Bounds (Just 1) (Just 3))]
      derivedBounds `shouldBe` expectedBounds
      validateBounds derivedBounds `shouldBe` True
  it
    "validateBounds finds that deriving bounds for a system where 1 <= x <= 0 and 3 <= y <= 1 has invalid bounds"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :>= 1
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 0
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 1] :>= 3
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 1] :<= 1
              ]
          derivedBounds = deriveBounds simpleSystem
          expectedBounds = Map.fromList [(0, Bounds (Just 1) (Just 0)), (1, Bounds (Just 3) (Just 1))]
      derivedBounds `shouldBe` expectedBounds
      validateBounds derivedBounds `shouldBe` False
  it
    "validateBounds finds that deriving bounds for a system where 1 <= x <= 0 and 1 <= y <= 3 has invalid bounds"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :>= 1
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 0
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 1] :>= 1
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 1] :<= 3
              ]
          derivedBounds = deriveBounds simpleSystem
          expectedBounds = Map.fromList [(0, Bounds (Just 1) (Just 0)), (1, Bounds (Just 1) (Just 3))]
      derivedBounds `shouldBe` expectedBounds
      validateBounds derivedBounds `shouldBe` False
  it
    "validateBounds finds that deriving bounds for a system where 0 <= x <= 1 and 3 <= y <= 1 has invalid bounds"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :>= 0
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 1
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 1] :>= 3
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 1] :<= 1
              ]
          derivedBounds = deriveBounds simpleSystem
          expectedBounds = Map.fromList [(0, Bounds (Just 0) (Just 1)), (1, Bounds (Just 3) (Just 1))]
      derivedBounds `shouldBe` expectedBounds
      validateBounds derivedBounds `shouldBe` False
  it "removeUselessSystemBounds removes x <= 3 when bounds say x <= 2" $ do
    let simpleSystem =
          SimpleSystem
            [ SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 2
            , SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 3
            ]
        bounds = Map.fromList [(0, Bounds (Just 0) (Just 2))]
        simplifiedSimpleSystem = removeUselessSystemBounds simpleSystem bounds
        expectedSimpleSystem = SimpleSystem [SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 2]
    simplifiedSimpleSystem `shouldBe` expectedSimpleSystem
  it "removeUselessSystemBounds does not remove x <= 2 when bounds say x <= 2" $ do
    let simpleSystem =
          SimpleSystem
            [ SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 2
            ]
        bounds = Map.fromList [(0, Bounds (Just 0) (Just 2))]
        simplifiedSimpleSystem = removeUselessSystemBounds simpleSystem bounds
        expectedSimpleSystem = SimpleSystem [SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 2]
    simplifiedSimpleSystem `shouldBe` expectedSimpleSystem
  it "removeUselessSystemBounds removes x >= 3 when bounds say x >= 4" $ do
    let simpleSystem =
          SimpleSystem
            [ SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :>= 4
            , SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :>= 3
            ]
        bounds = Map.fromList [(0, Bounds (Just 4) (Just 5))]
        simplifiedSimpleSystem = removeUselessSystemBounds simpleSystem bounds
        expectedSimpleSystem = SimpleSystem [SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :>= 4]
    simplifiedSimpleSystem `shouldBe` expectedSimpleSystem
  it "removeUselessSystemBounds does not remove x >= 4 when bounds say x >= 4" $ do
    let simpleSystem =
          SimpleSystem
            [ SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :>= 4
            ]
        bounds = Map.fromList [(0, Bounds (Just 4) (Just 5))]
        simplifiedSimpleSystem = removeUselessSystemBounds simpleSystem bounds
        expectedSimpleSystem = SimpleSystem [SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :>= 4]
    simplifiedSimpleSystem `shouldBe` expectedSimpleSystem
  it
    "removeUselessSystemBounds does not remove 0 <= x <= 2 when bounds say 0 <= x <= 2"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :>= 0
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 2
              ]
          bounds = Map.fromList [(0, Bounds (Just 0) (Just 2))]
          simplifiedSimpleSystem = removeUselessSystemBounds simpleSystem bounds
          expectedSimpleSystem =
            SimpleSystem
              [SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :>= 0, SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 2]
      simplifiedSimpleSystem `shouldBe` expectedSimpleSystem
  it
    "removeUselessSystemBounds removes upper bound of 0 <= x <= 2 when bounds say 0 <= x <= 1"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :>= 0
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 2
              ]
          bounds = Map.fromList [(0, Bounds (Just 0) (Just 1))]
          simplifiedSimpleSystem = removeUselessSystemBounds simpleSystem bounds
          expectedSimpleSystem = SimpleSystem [SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :>= 0]
      simplifiedSimpleSystem `shouldBe` expectedSimpleSystem
  it
    "removeUselessSystemBounds removes lower bound of 0 <= x <= 2 when bounds say 1 <= x <= 2"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :>= 0
              , SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 2
              ]
          bounds = Map.fromList [(0, Bounds (Just 1) (Just 2))]
          simplifiedSimpleSystem = removeUselessSystemBounds simpleSystem bounds
          expectedSimpleSystem = SimpleSystem [SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 2]
      simplifiedSimpleSystem `shouldBe` expectedSimpleSystem
  it "removeUselssSystemBounds only removes constraints of the form x <= c" $ do
    let simpleSystem =
          SimpleSystem
            [ SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 2
            , SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 3
            , SimpleConstraint $ ExprVarsOnly [CoeffTermVO 2 0] :<= 6
            ]
        bounds = Map.fromList [(0, Bounds (Just 0) (Just 2))]
        simplifiedSimpleSystem = removeUselessSystemBounds simpleSystem bounds
        expectedSimpleSystem =
          SimpleSystem
            [ SimpleConstraint $ ExprVarsOnly [VarTermVO 0] :<= 2
            , SimpleConstraint $ ExprVarsOnly [CoeffTermVO 2 0] :<= 6
            ]
    simplifiedSimpleSystem `shouldBe` expectedSimpleSystem
