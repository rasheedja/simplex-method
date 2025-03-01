module Linear.CanonicalForm.UtilSpec where

import Comparison.Types
  ( MixedComparison ((:<=), (:==), (:>=))
  , getLHS
  )
import Control.Monad (forM)
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Debug.Trace as T
import Linear.Constraint.Linear.Types (LinearEquation (..))
import Linear.Constraint.Simple.Types (SimpleConstraint (..))
import Linear.Expr.Types (Expr (..), ExprVarsOnly (..))
import Linear.CanonicalForm.Util
  ( addSlackVars
  , eliminateNonZeroLowerBounds
  , eliminateUnrestrictedLowerBounds
  )
import Linear.System.Linear.Types (LinearSystem (..))
import Linear.System.Simple.Types (SimpleSystem (..))
import Linear.System.Simple.Util (deriveBounds)
import Linear.Term.Types
  ( Term (..)
  , TermVarsOnly (..)
  )
import Linear.Var.Types
  ( Var (..)
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Testable (property), withMaxSuccess)

-- data Term = ConstTerm SimplexNum | CoeffTerm SimplexNum Var | VarTerm Var -- Consider VarTerm Var - note, we must consider normalizing this: Considered. It makes going to standard form easier due to type safety
--   deriving (Show, Read, Eq, Ord, Generic)

-- TODO: consider type NumberConstraint = MixedComparison SimplexNum SimplexNum
spec :: Spec
spec = describe "Slack Form Transformations" $ do
  it
    "eliminateNonZeroLowerBounds does not do anything when all lower bounds are zero"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 0)] :>= 0
              , SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 1)] :>= 0
              ]
          (updatedBounds, updatedSystem) = eliminateNonZeroLowerBounds simpleSystem Map.empty
          expectedSimpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 0)] :>= 0
              , SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 1)] :>= 0
              ]
          expectedEliminatedVarExprMap = Map.empty
      updatedSystem `shouldBe` expectedSimpleSystem
      updatedBounds `shouldBe` expectedEliminatedVarExprMap
  it "eliminateNonZeroLowerBounds correctly eliminates positive lower bounds" $ do
    let simpleSystem =
          SimpleSystem
            [ SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 0)] :>= 1
            , SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 1)] :>= 0
            ]
        (updatedBounds, updatedSystem) = eliminateNonZeroLowerBounds simpleSystem Map.empty
        expectedSimpleSystem =
          SimpleSystem
            [ SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 2)] :>= 0
            , SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 1)] :>= 0
            ]
        expectedEliminatedVarExprMap = Map.fromList [(Var 0, Expr (VarTerm (Var 2) : [ConstTerm 1]))]
    updatedSystem `shouldBe` expectedSimpleSystem
    updatedBounds `shouldBe` expectedEliminatedVarExprMap
  it "eliminateNonZeroLowerBounds correctly eliminates negative lower bounds" $ do
    let simpleSystem =
          SimpleSystem
            [ SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 0)] :>= (-1)
            , SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 1)] :>= 0
            ]
        (updatedBounds, updatedSystem) = eliminateNonZeroLowerBounds simpleSystem Map.empty
        expectedSimpleSystem =
          SimpleSystem
            [ SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 2)] :>= 0
            , SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 1)] :>= 0
            ]
        expectedEliminatedVarExprMap = Map.fromList [(Var 0, Expr (VarTerm (Var 2) : [ConstTerm (-1)]))]
    updatedSystem `shouldBe` expectedSimpleSystem
    updatedBounds `shouldBe` expectedEliminatedVarExprMap
  it
    "eliminateNonZeroLowerBounds correctly eliminates positive and negative lower bounds"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 0)] :>= 1
              , SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 1)] :>= (-1)
              ]
          (updatedBounds, updatedSystem) = eliminateNonZeroLowerBounds simpleSystem Map.empty
          expectedSimpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 2)] :>= 0
              , SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 3)] :>= 0
              ]
          expectedEliminatedVarExprMap =
            Map.fromList
              [ (Var 0, Expr (VarTerm (Var 2) : [ConstTerm 1]))
              , (Var 1, Expr (VarTerm (Var 3) : [ConstTerm (-1)]))
              ]
      updatedSystem `shouldBe` expectedSimpleSystem
      updatedBounds `shouldBe` expectedEliminatedVarExprMap
  it
    "eliminateNonZeroLowerBounds correctly substitutes vars with non-zero lower bounds"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 0)] :>= 1
              , SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 1)] :>= 0
              , SimpleConstraint $ ExprVarsOnly (VarTermVO (Var 0) : [VarTermVO (Var 1)]) :>= 1
              ]
          (updatedBounds, updatedSystem) = eliminateNonZeroLowerBounds simpleSystem Map.empty
          expectedSimpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 2)] :>= 0
              , SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 1)] :>= 0
              , SimpleConstraint $ ExprVarsOnly (VarTermVO (Var 1) : [VarTermVO (Var 2)]) :>= 0
              ]
          expectedEliminatedVarExprMap = Map.fromList [(Var 0, Expr (VarTerm (Var 2) : [ConstTerm 1]))]
      updatedSystem `shouldBe` expectedSimpleSystem
      updatedBounds `shouldBe` expectedEliminatedVarExprMap
  it "eliminateNonZeroLowerBounds property based test lower bounds" $ withMaxSuccess 5 $ property $ \simpleSystem -> do
    let (updatedBounds, updatedSystem) = eliminateNonZeroLowerBounds simpleSystem Map.empty
    all
      ( \case
          SimpleConstraint (ExprVarsOnly [VarTermVO (Var _)] :>= num) -> num == 0
          _ -> True
      )
      updatedSystem.unSimpleSystem
  it "eliminateNonZeroLowerBounds property based test map" $ withMaxSuccess 5 $ property $ \simpleSystem -> do
    let (updatedBounds, updatedSystem) = eliminateNonZeroLowerBounds simpleSystem Map.empty
    all
      ( \(var, _) ->
          any
            ( \(SimpleConstraint constraint) ->
                let getVars _a = []
                    lhs = getLHS constraint
                    allVars = getVars lhs
                in  var `notElem` allVars
            )
            updatedSystem.unSimpleSystem
      )
      (Map.toList updatedBounds)
  it
    "addSlackVars correctly transforms inequalities to equalities (wikipedia case)"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly (VarTermVO (Var 2) : [CoeffTermVO 2 (Var 3)]) :<= 3 -- x_2 + 2x_3 <= 3
              , SimpleConstraint $ ExprVarsOnly (CoeffTermVO (-1) (Var 4) : [CoeffTermVO 3 (Var 5)]) :>= 2 -- -x_4 + 3x_5 >= 2
              ]
          expectedSystem =
            LinearSystem
              [ LinearEquation (ExprVarsOnly (VarTermVO (Var 2) : [CoeffTermVO 2 (Var 3), VarTermVO (Var 6)])) 3 -- x_2 + 2x_3 + x_6 = 3
              , LinearEquation
                  (ExprVarsOnly (CoeffTermVO (-1) (Var 4) : [CoeffTermVO 3 (Var 5), CoeffTermVO (-1) (Var 7)]))
                  2 -- -x_4 + 3x_5 + x_7 = 2
              ]
          expectedSlackVars = [Var 6, Var 7]
          (slackVars, updatedSystem) = addSlackVars simpleSystem
      updatedSystem `shouldBe` expectedSystem
      slackVars `shouldBe` expectedSlackVars
  it
    "addSlackVars correctly transforms inequalities to equalities (test case 1)"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly (VarTermVO (Var 1) : [CoeffTermVO 2 (Var 2)]) :<= 4 -- x_1 + 2x_2 <= 4
              , SimpleConstraint $ ExprVarsOnly (CoeffTermVO (-1) (Var 3) : [CoeffTermVO 2 (Var 4)]) :>= 3 -- -x_3 + 2x_4 >= 3
              ]
          expectedSystem =
            LinearSystem
              [ LinearEquation (ExprVarsOnly (VarTermVO (Var 1) : [CoeffTermVO 2 (Var 2), VarTermVO (Var 5)])) 4 -- x_1 + 2x_2 + x_5 = 4
              , LinearEquation
                  (ExprVarsOnly (CoeffTermVO (-1) (Var 3) : [CoeffTermVO 2 (Var 4), CoeffTermVO (-1) (Var 6)]))
                  3 -- -x_3 + 2x_4 - x_6 = 3
              ]
          expectedSlackVars = [Var 5, Var 6]
          (slackVars, updatedSystem) = addSlackVars simpleSystem
      updatedSystem `shouldBe` expectedSystem
      slackVars `shouldBe` expectedSlackVars
  it
    "addSlackVars correctly transforms inequalities to equalities (test case 2)"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly (VarTermVO (Var 1) : [CoeffTermVO 2 (Var 2)]) :<= 5 -- x_1 + 2x_2 <= 5
              , SimpleConstraint $ ExprVarsOnly (CoeffTermVO (-1) (Var 3) : [CoeffTermVO 2 (Var 4)]) :>= 4 -- -x_3 + 2x_4 >= 4
              ]
          expectedSystem =
            LinearSystem
              [ LinearEquation (ExprVarsOnly (VarTermVO (Var 1) : [CoeffTermVO 2 (Var 2), VarTermVO (Var 5)])) 5 -- x_1 + 2x_2 + x_5 = 5
              , LinearEquation
                  (ExprVarsOnly (CoeffTermVO (-1) (Var 3) : [CoeffTermVO 2 (Var 4), CoeffTermVO (-1) (Var 6)]))
                  4 -- -x_3 + 2x_4 - x_6 = 4
              ]
          expectedSlackVars = [Var 5, Var 6]
          (slackVars, updatedSystem) = addSlackVars simpleSystem
      updatedSystem `shouldBe` expectedSystem
      slackVars `shouldBe` expectedSlackVars
  it
    "addSlackVars correctly transforms inequalities to equalities (test case 3)"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly (VarTermVO (Var 1) : [CoeffTermVO 2 (Var 2)]) :<= 5 -- x_1 + 2x_2 <= 5
              , SimpleConstraint $ ExprVarsOnly (CoeffTermVO (-1) (Var 3) : [CoeffTermVO 2 (Var 4)]) :== 4 -- -x_3 + 2x_4 = 4
              ]
          expectedSystem =
            LinearSystem
              [ LinearEquation (ExprVarsOnly (VarTermVO (Var 1) : [CoeffTermVO 2 (Var 2), VarTermVO (Var 5)])) 5 -- x_1 + 2x_2 + x_5 = 5
              , LinearEquation (ExprVarsOnly (CoeffTermVO (-1) (Var 3) : [CoeffTermVO 2 (Var 4)])) 4 -- -x_3 + 2x_4 = 4
              ]
          expectedSlackVars = [Var 5]
          (slackVars, updatedSystem) = addSlackVars simpleSystem
      updatedSystem `shouldBe` expectedSystem
      slackVars `shouldBe` expectedSlackVars
  it
    "addSlackVars correctly transforms inequalities to equalities (test case 4)"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly (VarTermVO (Var 1) : [CoeffTermVO 2 (Var 2)]) :== 5 -- x_1 + 2x_2 = 5
              , SimpleConstraint $ ExprVarsOnly (CoeffTermVO (-1) (Var 3) : [CoeffTermVO 2 (Var 4)]) :== 4 -- -x_3 + 2x_4 = 4
              ]
          expectedSystem =
            LinearSystem
              [ LinearEquation (ExprVarsOnly (VarTermVO (Var 1) : [CoeffTermVO 2 (Var 2)])) 5 -- x_1 + 2x_2 = 5
              , LinearEquation (ExprVarsOnly (CoeffTermVO (-1) (Var 3) : [CoeffTermVO 2 (Var 4)])) 4 -- -x_3 + 2x_4 = 4
              ]
          expectedSlackVars = []
          (slackVars, updatedSystem) = addSlackVars simpleSystem
      updatedSystem `shouldBe` expectedSystem
      slackVars `shouldBe` expectedSlackVars
  it
    "eliminateUnrestrictedLowerBounds correctly eliminates unrestricted lower bounds (wikipedia case)"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 1)] :>= 0 -- x_1 >= 0
              , SimpleConstraint $ ExprVarsOnly (VarTermVO (Var 1) : [VarTermVO (Var 2)]) :>= 0 -- x_1 + x_2 >= 0
              ]
          systemBounds = deriveBounds simpleSystem
          (eliminatedNonZeroLowerBounds, systemWithoutNonZeroLowerBounds) = eliminateNonZeroLowerBounds simpleSystem Map.empty
          (slackVars, systemWithSlackVars) = addSlackVars systemWithoutNonZeroLowerBounds
          (updatedEliminatedVarsMap, updatedSystem) =
            eliminateUnrestrictedLowerBounds
              systemWithSlackVars
              systemBounds
              eliminatedNonZeroLowerBounds
          expectedSlackVars = [Var 3, Var 4]
          expectedSystem =
            LinearSystem
              [ LinearEquation (ExprVarsOnly (CoeffTermVO (-1) (Var 3) : [VarTermVO (Var 1)])) 0 -- -x_3 + x_1 = 0
              , LinearEquation
                  ( ExprVarsOnly
                      (CoeffTermVO (-1) (Var 4) : [CoeffTermVO (-1) (Var 6), VarTermVO (Var 1), VarTermVO (Var 5)])
                  )
                  0 -- -x_4 - x_6 + x_1 + x_5 = 0
              ]
          expectedEliminatedVarExprMap = Map.fromList [(Var 2, Expr (VarTerm (Var 5) : [CoeffTerm (-1) (Var 6)]))]

      slackVars `shouldBe` expectedSlackVars
      updatedSystem `shouldBe` expectedSystem
      updatedEliminatedVarsMap `shouldBe` expectedEliminatedVarExprMap
  it
    "eliminateUnrestrictedLowerBounds correctly eliminates unrestricted lower bounds (test case 2)"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 1)] :>= 0 -- x_1 >= 0
              , SimpleConstraint $ ExprVarsOnly (VarTermVO (Var 1) : [VarTermVO (Var 2), VarTermVO (Var 3)]) :>= 0 -- x_1 + x_2 + x_3 >= 0
              ]
          systemBounds = deriveBounds simpleSystem
          (eliminatedNonZeroLowerBounds, systemWithoutNonZeroLowerBounds) = eliminateNonZeroLowerBounds simpleSystem Map.empty
          (slackVars, systemWithSlackVars) = addSlackVars systemWithoutNonZeroLowerBounds
          (updatedEliminatedVarsMap, updatedSystem) =
            eliminateUnrestrictedLowerBounds
              systemWithSlackVars
              systemBounds
              eliminatedNonZeroLowerBounds
          expectedSlackVars = [Var 4, Var 5]
          expectedSystem =
            LinearSystem
              [ LinearEquation (ExprVarsOnly (CoeffTermVO (-1) (Var 4) : [VarTermVO (Var 1)])) 0 -- -x_4 + x_1 = 0
              , LinearEquation
                  ( ExprVarsOnly
                      ( CoeffTermVO (-1) (Var 5)
                          : [CoeffTermVO (-1) (Var 7), CoeffTermVO (-1) (Var 9), VarTermVO (Var 1), VarTermVO (Var 6), VarTermVO (Var 8)]
                      )
                  )
                  0 -- -x_5 - x_7 - x_9 + x_1 + x_6 + x_8 = 0
              ]
          expectedEliminatedVarExprMap =
            Map.fromList
              [ (Var 2, Expr (VarTerm (Var 6) : [CoeffTerm (-1) (Var 7)]))
              , (Var 3, Expr (VarTerm (Var 8) : [CoeffTerm (-1) (Var 9)]))
              ]
      slackVars `shouldBe` expectedSlackVars
      updatedSystem `shouldBe` expectedSystem
      updatedEliminatedVarsMap `shouldBe` expectedEliminatedVarExprMap

  it
    "eliminateUnrestrictedLowerBounds correctly eliminates unrestricted lower bounds (test case 3)"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 1)] :>= 0 -- x_1 >= 0
              , SimpleConstraint $ ExprVarsOnly (VarTermVO (Var 1) : [VarTermVO (Var 2)]) :>= 0 -- x_1 + x_2 >= 0
              , SimpleConstraint $ ExprVarsOnly (VarTermVO (Var 2) : [VarTermVO (Var 3)]) :>= 0 -- x_2 + x_3 >= 0
              ]
          systemBounds = deriveBounds simpleSystem
          (eliminatedNonZeroLowerBounds, systemWithoutNonZeroLowerBounds) = eliminateNonZeroLowerBounds simpleSystem Map.empty
          (slackVars, systemWithSlackVars) = addSlackVars systemWithoutNonZeroLowerBounds
          expectedSlackVars = [Var 4, Var 5, Var 6]
          (updatedEliminatedVarsMap, updatedSystem) =
            eliminateUnrestrictedLowerBounds
              systemWithSlackVars
              systemBounds
              eliminatedNonZeroLowerBounds
          expectedSystem =
            LinearSystem
              [ LinearEquation (ExprVarsOnly (CoeffTermVO (-1) (Var 4) : [VarTermVO (Var 1)])) 0 -- -x_4 + x_1 = 0
              , LinearEquation
                  ( ExprVarsOnly
                      (CoeffTermVO (-1) (Var 5) : [CoeffTermVO (-1) (Var 8), VarTermVO (Var 1), VarTermVO (Var 7)])
                  )
                  0 -- -x_5 - x_8 + x_1 + x_7 = 0
              , LinearEquation
                  ( ExprVarsOnly
                      ( CoeffTermVO (-1) (Var 6)
                          : [CoeffTermVO (-1) (Var 8), CoeffTermVO (-1) (Var 10), VarTermVO (Var 7), VarTermVO (Var 9)]
                      )
                  )
                  0 -- -x_6 - x_8 - x_10 + x_7 + x_9 = 0
              ]
          expectedEliminatedVarExprMap =
            Map.fromList
              [ (Var 2, Expr (VarTerm (Var 7) : [CoeffTerm (-1) (Var 8)]))
              , (Var 3, Expr (VarTerm (Var 9) : [CoeffTerm (-1) (Var 10)]))
              ]
      slackVars `shouldBe` expectedSlackVars
      updatedSystem `shouldBe` expectedSystem
      updatedEliminatedVarsMap `shouldBe` expectedEliminatedVarExprMap
  it
    "eliminateUnrestrictedLowerBounds correctly eliminates non-zero lower bounds for all variables"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly (VarTermVO (Var 1) : [CoeffTermVO 2 (Var 1)]) :>= 0 -- x_1 + 2x_1 >= 0
              , SimpleConstraint $ ExprVarsOnly (VarTermVO (Var 2) : [CoeffTermVO 3 (Var 1)]) :>= 0 -- x_2 + 3x_1 >= 0
              ]
          systemBounds = deriveBounds simpleSystem
          (eliminatedNonZeroLowerBounds, systemWithoutNonZeroLowerBounds) = eliminateNonZeroLowerBounds simpleSystem Map.empty
          (slackVars, systemWithSlackVars) = addSlackVars systemWithoutNonZeroLowerBounds
          expectedSlackVars = [Var 3, Var 4]
          (updatedEliminatedVarsMap, updatedSystem) =
            eliminateUnrestrictedLowerBounds
              systemWithSlackVars
              systemBounds
              eliminatedNonZeroLowerBounds
          expectedSystem =
            LinearSystem
              [ LinearEquation
                  (ExprVarsOnly (CoeffTermVO (-3) (Var 6) : [CoeffTermVO (-1) (Var 3), CoeffTermVO 3 (Var 5)]))
                  0 -- -3x_6 - x_3 + 3x_5 = 0
              , LinearEquation
                  ( ExprVarsOnly
                      ( CoeffTermVO (-3) (Var 6)
                          : [CoeffTermVO (-1) (Var 4), CoeffTermVO (-1) (Var 8), CoeffTermVO 3 (Var 5), VarTermVO (Var 7)]
                      )
                  )
                  0 -- -3x_6 - x_4 - x_8 + 3x_5 + x_7 = 0
              ]
          expectedEliminatedVarExprMap =
            Map.fromList
              [ (Var 1, Expr (VarTerm (Var 5) : [CoeffTerm (-1) (Var 6)]))
              , (Var 2, Expr (VarTerm (Var 7) : [CoeffTerm (-1) (Var 8)]))
              ]
      slackVars `shouldBe` expectedSlackVars
      updatedSystem `shouldBe` expectedSystem
      updatedEliminatedVarsMap `shouldBe` expectedEliminatedVarExprMap

  it
    "eliminateUnrestrictedLowerBounds correctly handles all variables with zero lower bounds"
    $ do
      let simpleSystem =
            SimpleSystem
              [ SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 1)] :>= 0 -- x_1 >= 0
              , SimpleConstraint $ ExprVarsOnly [VarTermVO (Var 2)] :>= 0 -- x_2 >= 0
              ]
          systemBounds = deriveBounds simpleSystem
          (eliminatedNonZeroLowerBounds, systemWithoutNonZeroLowerBounds) = eliminateNonZeroLowerBounds simpleSystem Map.empty
          (slackVars, systemWithSlackVars) = addSlackVars systemWithoutNonZeroLowerBounds
          expectedSlackVars = [Var 3, Var 4]
          (updatedEliminatedVarsMap, updatedSystem) =
            eliminateUnrestrictedLowerBounds
              systemWithSlackVars
              systemBounds
              eliminatedNonZeroLowerBounds
          expectedSystem =
            LinearSystem
              [ LinearEquation (ExprVarsOnly (VarTermVO (Var 1) : [CoeffTermVO (-1) (Var 3)])) 0 -- x_1 - x_3 = 0
              , LinearEquation (ExprVarsOnly (VarTermVO (Var 2) : [CoeffTermVO (-1) (Var 4)])) 0 -- x_2 - x_4 = 0
              ]
          expectedEliminatedVarExprMap = Map.empty
      slackVars `shouldBe` expectedSlackVars
      updatedSystem `shouldBe` expectedSystem
      updatedEliminatedVarsMap `shouldBe` expectedEliminatedVarExprMap
