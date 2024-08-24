module Linear.SlackForm.UtilSpec where

import Control.Monad (forM)
import Data.Functor ((<&>))
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Debug.Trace as T
import Linear.Constraint.Generic.Types
  ( GenericConstraint ((:<=), (:==), (:>=))
  , getGenericConstraintLHS
  )
import Linear.Expr.Types (Expr (..))
import Linear.SlackForm.Util
  ( addSlackVariables
  , eliminateNonZeroLowerBounds
  , eliminateUnrestrictedLowerBounds
  )
import Linear.System.Simple.Util (deriveBounds)
import Linear.Term.Types
  ( Term (..)
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Testable (property), withMaxSuccess)

-- data Term = ConstTerm SimplexNum | CoeffTerm SimplexNum Var | VarTerm Var -- Consider VarTerm Var - note, we must consider normalizing this: Considered. It makes going to standard form easier due to type safety
--   deriving (Show, Read, Eq, Ord, Generic)

-- TODO: consider type NumberConstraint = GenericConstraint SimplexNum SimplexNum
spec :: Spec
spec = do
  describe "Slack Form Transformations" $ do
    it
      "eliminateNonZeroLowerBounds does not do anything when all lower bounds are zero"
      $ do
        let simpleSystem =
              [ Expr (VarTerm 0 :| []) :>= 0
              , Expr (VarTerm 1 :| []) :>= 0
              ]
            (updatedBounds, updatedSystem) = eliminateNonZeroLowerBounds simpleSystem Map.empty
            expectedSimpleSystem =
              [ Expr (VarTerm 0 :| []) :>= 0
              , Expr (VarTerm 1 :| []) :>= 0
              ]
            expectedEliminatedVarExprMap = Map.empty
        updatedSystem `shouldBe` expectedSimpleSystem
        updatedBounds `shouldBe` expectedEliminatedVarExprMap
    it "eliminateNonZeroLowerBounds correctly eliminates positive lower bounds" $ do
      let simpleSystem =
            [ Expr (VarTerm 0 :| []) :>= 1
            , Expr (VarTerm 1 :| []) :>= 0
            ]
          (updatedBounds, updatedSystem) = eliminateNonZeroLowerBounds simpleSystem Map.empty
          expectedSimpleSystem =
            [ Expr (VarTerm 2 :| []) :>= 0
            , Expr (VarTerm 1 :| []) :>= 0
            ]
          expectedEliminatedVarExprMap = Map.fromList [(0, Expr (VarTerm 2 :| [ConstTerm 1]))]
      updatedSystem `shouldBe` expectedSimpleSystem
      updatedBounds `shouldBe` expectedEliminatedVarExprMap
    it "eliminateNonZeroLowerBounds correctly eliminates negative lower bounds" $ do
      let simpleSystem =
            [ Expr (VarTerm 0 :| []) :>= (-1)
            , Expr (VarTerm 1 :| []) :>= 0
            ]
          (updatedBounds, updatedSystem) = eliminateNonZeroLowerBounds simpleSystem Map.empty
          expectedSimpleSystem =
            [ Expr (VarTerm 2 :| []) :>= 0
            , Expr (VarTerm 1 :| []) :>= 0
            ]
          expectedEliminatedVarExprMap = Map.fromList [(0, Expr (VarTerm 2 :| [ConstTerm (-1)]))]
      updatedSystem `shouldBe` expectedSimpleSystem
      updatedBounds `shouldBe` expectedEliminatedVarExprMap
    it
      "eliminateNonZeroLowerBounds correctly eliminates positive and negative lower bounds"
      $ do
        let simpleSystem =
              [ Expr (VarTerm 0 :| []) :>= 1
              , Expr (VarTerm 1 :| []) :>= (-1)
              ]
            (updatedBounds, updatedSystem) = eliminateNonZeroLowerBounds simpleSystem Map.empty
            expectedSimpleSystem =
              [ Expr (VarTerm 2 :| []) :>= 0
              , Expr (VarTerm 3 :| []) :>= 0
              ]
            expectedEliminatedVarExprMap =
              Map.fromList
                [ (0, Expr (VarTerm 2 :| [ConstTerm 1]))
                , (1, Expr (VarTerm 3 :| [ConstTerm (-1)]))
                ]
        updatedSystem `shouldBe` expectedSimpleSystem
        updatedBounds `shouldBe` expectedEliminatedVarExprMap
    it
      "eliminateNonZeroLowerBounds correctly substitutes vars with non-zero lower bounds"
      $ do
        let simpleSystem =
              [ Expr (VarTerm 0 :| []) :>= 1
              , Expr (VarTerm 1 :| []) :>= 0
              , Expr (VarTerm 0 :| [VarTerm 1]) :>= 1
              ]
            (updatedBounds, updatedSystem) = eliminateNonZeroLowerBounds simpleSystem Map.empty
            expectedSimpleSystem =
              [ Expr (VarTerm 2 :| []) :>= 0
              , Expr (VarTerm 1 :| []) :>= 0
              , Expr (VarTerm 1 :| [VarTerm 2]) :>= 0
              ]
            expectedEliminatedVarExprMap = Map.fromList [(0, Expr (VarTerm 2 :| [ConstTerm 1]))]
        updatedSystem `shouldBe` expectedSimpleSystem
        updatedBounds `shouldBe` expectedEliminatedVarExprMap
    it "eliminateNonZeroLowerBounds property based test lower bounds" $ do
      withMaxSuccess 5 $ property $ \simpleSystem -> do
        let (updatedBounds, updatedSystem) = eliminateNonZeroLowerBounds simpleSystem Map.empty
        all
          ( \case
              Expr (VarTerm _ :| []) :>= num -> num == 0
              _ -> True
          )
          updatedSystem
    it "eliminateNonZeroLowerBounds property based test map" $ do
      withMaxSuccess 5 $ property $ \simpleSystem -> do
        let (updatedBounds, updatedSystem) = eliminateNonZeroLowerBounds simpleSystem Map.empty
        all
          ( \(var, _) ->
              any
                ( \constraint ->
                    let getVars _a = []
                        lhs = getGenericConstraintLHS constraint
                        allVars = getVars lhs
                    in  var `notElem` allVars
                )
                updatedSystem
          )
          (Map.toList updatedBounds)
    it
      "addSlackVariables correctly transforms inequalities to equalities (wikipedia case)"
      $ do
        let simpleSystem =
              [ Expr (VarTerm 2 :| [CoeffTerm 2 3]) :<= 3 -- x_2 + 2x_3 <= 3
              , Expr (CoeffTerm (-1) 4 :| [CoeffTerm 3 5]) :>= 2 -- -x_4 + 3x_5 >= 2
              ]
            expectedSystem =
              [ Expr (VarTerm 2 :| [CoeffTerm 2 3, VarTerm 6]) :== 3 -- x_2 + 2x_3 + x_6 = 3
              , Expr (VarTerm 6 :| []) :>= 0 -- x_6 >= 0
              , Expr (CoeffTerm (-1) 4 :| [CoeffTerm 3 5, CoeffTerm (-1) 7]) :== 2 -- -x_4 + 3x_5 + x_7 = 2
              , Expr (VarTerm 7 :| []) :>= 0 -- x_7 >= 0
              ]
            expectedSlackVars = [6, 7]
            (slackVars, updatedSystem) = addSlackVariables simpleSystem
        updatedSystem `shouldBe` expectedSystem
        slackVars `shouldBe` expectedSlackVars
    it
      "addSlackVariables correctly transforms inequalities to equalities (test case 1)"
      $ do
        let simpleSystem =
              [ Expr (VarTerm 1 :| [CoeffTerm 2 2]) :<= 4 -- x_1 + 2x_2 <= 4
              , Expr (CoeffTerm (-1) 3 :| [CoeffTerm 2 4]) :>= 3 -- -x_3 + 2x_4 >= 3
              ]
            expectedSystem =
              [ Expr (VarTerm 1 :| [CoeffTerm 2 2, VarTerm 5]) :== 4 -- x_1 + 2x_2 + x_5 = 4
              , Expr (VarTerm 5 :| []) :>= 0 -- x_5 >= 0
              , Expr (CoeffTerm (-1) 3 :| [CoeffTerm 2 4, CoeffTerm (-1) 6]) :== 3 -- -x_3 + 2x_4 - x_6 = 3
              , Expr (VarTerm 6 :| []) :>= 0 -- x_6 >= 0
              ]
            expectedSlackVars = [5, 6]
            (slackVars, updatedSystem) = addSlackVariables simpleSystem
        updatedSystem `shouldBe` expectedSystem
        slackVars `shouldBe` expectedSlackVars
    it
      "addSlackVariables correctly transforms inequalities to equalities (test case 2)"
      $ do
        let simpleSystem =
              [ Expr (VarTerm 1 :| [CoeffTerm 2 2]) :<= 5 -- x_1 + 2x_2 <= 5
              , Expr (CoeffTerm (-1) 3 :| [CoeffTerm 2 4]) :>= 4 -- -x_3 + 2x_4 >= 4
              ]
            expectedSystem =
              [ Expr (VarTerm 1 :| [CoeffTerm 2 2, VarTerm 5]) :== 5 -- x_1 + 2x_2 + x_5 = 5
              , Expr (VarTerm 5 :| []) :>= 0 -- x_5 >= 0
              , Expr (CoeffTerm (-1) 3 :| [CoeffTerm 2 4, CoeffTerm (-1) 6]) :== 4 -- -x_3 + 2x_4 - x_6 = 4
              , Expr (VarTerm 6 :| []) :>= 0 -- x_6 >= 0
              ]
            expectedSlackVars = [5, 6]
            (slackVars, updatedSystem) = addSlackVariables simpleSystem
        updatedSystem `shouldBe` expectedSystem
        slackVars `shouldBe` expectedSlackVars
    it
      "addSlackVariables correctly transforms inequalities to equalities (test case 3)"
      $ do
        let simpleSystem =
              [ Expr (VarTerm 1 :| [CoeffTerm 2 2]) :<= 5 -- x_1 + 2x_2 <= 5
              , Expr (CoeffTerm (-1) 3 :| [CoeffTerm 2 4]) :== 4 -- -x_3 + 2x_4 = 4
              ]
            expectedSystem =
              [ Expr (VarTerm 1 :| [CoeffTerm 2 2, VarTerm 5]) :== 5 -- x_1 + 2x_2 + x_5 = 5
              , Expr (VarTerm 5 :| []) :>= 0 -- x_5 >= 0
              , Expr (CoeffTerm (-1) 3 :| [CoeffTerm 2 4]) :== 4 -- -x_3 + 2x_4 = 4
              ]
            expectedSlackVars = [5]
            (slackVars, updatedSystem) = addSlackVariables simpleSystem
        updatedSystem `shouldBe` expectedSystem
        slackVars `shouldBe` expectedSlackVars
    it
      "addSlackVariables correctly transforms inequalities to equalities (test case 4)"
      $ do
        let simpleSystem =
              [ Expr (VarTerm 1 :| [CoeffTerm 2 2]) :== 5 -- x_1 + 2x_2 = 5
              , Expr (CoeffTerm (-1) 3 :| [CoeffTerm 2 4]) :== 4 -- -x_3 + 2x_4 = 4
              ]
            expectedSystem =
              [ Expr (VarTerm 1 :| [CoeffTerm 2 2]) :== 5 -- x_1 + 2x_2 = 5
              , Expr (CoeffTerm (-1) 3 :| [CoeffTerm 2 4]) :== 4 -- -x_3 + 2x_4 = 4
              ]
            expectedSlackVars = []
            (slackVars, updatedSystem) = addSlackVariables simpleSystem
        updatedSystem `shouldBe` expectedSystem
        slackVars `shouldBe` expectedSlackVars
    it
      "eliminateUnrestrictedLowerBounds correctly eliminates unrestricted lower bounds (wikipedia case)"
      $ do
        let simpleSystem =
              [ Expr (VarTerm 1 :| []) :>= 0 -- x_1 >= 0
              , Expr (VarTerm 1 :| [VarTerm 2]) :>= 0 -- x_1 + x_2 >= 0
              ]
            systemBounds = deriveBounds simpleSystem
            (eliminatedNonZeroLowerBounds, systemWithoutNonZeroLowerBounds) = eliminateNonZeroLowerBounds simpleSystem Map.empty
            (slackVars, systemWithSlackVars) = addSlackVariables systemWithoutNonZeroLowerBounds
            (updatedEliminatedVarsMap, updatedSystem) =
              eliminateUnrestrictedLowerBounds
                systemWithSlackVars
                systemBounds
                eliminatedNonZeroLowerBounds
            expectedSystem =
              [ Expr (VarTerm 5 :| []) :>= 0 -- x_5 >= 0
              , Expr (VarTerm 6 :| []) :>= 0 -- x_6 >= 0
              , Expr (CoeffTerm (-1) 3 :| [VarTerm 1]) :== 0 -- -x_3 + x_1 = 0
              , Expr (VarTerm 3 :| []) :>= 0 -- x_3 >= 0
              , Expr (CoeffTerm (-1) 4 :| [CoeffTerm (-1) 6, VarTerm 1, VarTerm 5]) :== 0 -- -x_4 - x_6 + x_1 + x_5 = 0
              , Expr (VarTerm 4 :| []) :>= 0 -- x_4 >= 0
              ]
            expectedEliminatedVarExprMap = Map.fromList [(2, Expr (VarTerm 5 :| [CoeffTerm (-1) 6]))]

        updatedSystem `shouldBe` expectedSystem
        updatedEliminatedVarsMap `shouldBe` expectedEliminatedVarExprMap
    it
      "eliminateUnrestrictedLowerBounds correctly eliminates unrestricted lower bounds (test case 2)"
      $ do
        let simpleSystem =
              [ Expr (VarTerm 1 :| []) :>= 0 -- x_1 >= 0
              , Expr (VarTerm 1 :| [VarTerm 2, VarTerm 3]) :>= 0 -- x_1 + x_2 + x_3 >= 0
              ]
            systemBounds = deriveBounds simpleSystem
            (eliminatedNonZeroLowerBounds, systemWithoutNonZeroLowerBounds) = eliminateNonZeroLowerBounds simpleSystem Map.empty
            (slackVars, systemWithSlackVars) = addSlackVariables systemWithoutNonZeroLowerBounds
            (updatedEliminatedVarsMap, updatedSystem) =
              eliminateUnrestrictedLowerBounds
                systemWithSlackVars
                systemBounds
                eliminatedNonZeroLowerBounds
            expectedSystem =
              [ Expr (VarTerm 8 :| []) :>= 0 -- x_8 >= 0
              , Expr (VarTerm 9 :| []) :>= 0 -- x_9 >= 0
              , Expr (VarTerm 6 :| []) :>= 0 -- x_6 >= 0
              , Expr (VarTerm 7 :| []) :>= 0 -- x_7 >= 0
              , Expr (CoeffTerm (-1) 4 :| [VarTerm 1]) :== 0 -- -x_4 + x_1 = 0
              , Expr (VarTerm 4 :| []) :>= 0 -- x_4 >= 0
              , Expr
                  ( CoeffTerm (-1) 5
                      :| [CoeffTerm (-1) 7, CoeffTerm (-1) 9, VarTerm 1, VarTerm 6, VarTerm 8]
                  )
                  :== 0 -- -x_5 - x_7 - x_9 + x_1 + x_6 + x_8 = 0
              , Expr (VarTerm 5 :| []) :>= 0 -- x_5 >= 0
              ]
            expectedEliminatedVarExprMap =
              Map.fromList
                [ (2, Expr (VarTerm 6 :| [CoeffTerm (-1) 7]))
                , (3, Expr (VarTerm 8 :| [CoeffTerm (-1) 9]))
                ]
        updatedSystem `shouldBe` expectedSystem
        updatedEliminatedVarsMap `shouldBe` expectedEliminatedVarExprMap

    it
      "eliminateUnrestrictedLowerBounds correctly eliminates unrestricted lower bounds (test case 3)"
      $ do
        let simpleSystem =
              [ Expr (VarTerm 1 :| []) :>= 0 -- x_1 >= 0
              , Expr (VarTerm 1 :| [VarTerm 2]) :>= 0 -- x_1 + x_2 >= 0
              , Expr (VarTerm 2 :| [VarTerm 3]) :>= 0 -- x_2 + x_3 >= 0
              ]
            systemBounds = deriveBounds simpleSystem
            (eliminatedNonZeroLowerBounds, systemWithoutNonZeroLowerBounds) = eliminateNonZeroLowerBounds simpleSystem Map.empty
            (slackVars, systemWithSlackVars) = addSlackVariables systemWithoutNonZeroLowerBounds
            (updatedEliminatedVarsMap, updatedSystem) =
              eliminateUnrestrictedLowerBounds
                systemWithSlackVars
                systemBounds
                eliminatedNonZeroLowerBounds
            expectedSystem =
              [ Expr (VarTerm 9 :| []) :>= 0 -- x_9 >= 0
              , Expr (VarTerm 10 :| []) :>= 0 -- x_10 >= 0
              , Expr (VarTerm 7 :| []) :>= 0 -- x_7 >= 0
              , Expr (VarTerm 8 :| []) :>= 0 -- x_8 >= 0
              , Expr (CoeffTerm (-1) 4 :| [VarTerm 1]) :== 0 -- -x_4 + x_1 = 0
              , Expr (VarTerm 4 :| []) :>= 0 -- x_4 >= 0
              , Expr (CoeffTerm (-1) 5 :| [CoeffTerm (-1) 8, VarTerm 1, VarTerm 7]) :== 0 -- -x_5 - x_8 + x_1 + x_7 = 0
              , Expr (VarTerm 5 :| []) :>= 0 -- x_5 >= 0
              , Expr
                  ( CoeffTerm (-1) 6 :| [CoeffTerm (-1) 8, CoeffTerm (-1) 10, VarTerm 7, VarTerm 9]
                  )
                  :== 0 -- -x_6 - x_8 - x_10 + x_7 + x_9 = 0
              , Expr (VarTerm 6 :| []) :>= 0 -- x_6 >= 0
              ]
            expectedEliminatedVarExprMap =
              Map.fromList
                [ (2, Expr (VarTerm 7 :| [CoeffTerm (-1) 8]))
                , (3, Expr (VarTerm 9 :| [CoeffTerm (-1) 10]))
                ]
        updatedSystem `shouldBe` expectedSystem
        updatedEliminatedVarsMap `shouldBe` expectedEliminatedVarExprMap
    it
      "eliminateUnrestrictedLowerBounds correctly eliminates non-zero lower bounds for all variables"
      $ do
        let simpleSystem =
              [ Expr (VarTerm 1 :| [CoeffTerm 2 1]) :>= 0 -- x_1 + 2 >= 0
              , Expr (VarTerm 2 :| [CoeffTerm 3 1]) :>= 0 -- x_2 + 3 >= 0
              ]
            systemBounds = deriveBounds simpleSystem
            (eliminatedNonZeroLowerBounds, systemWithoutNonZeroLowerBounds) = eliminateNonZeroLowerBounds simpleSystem Map.empty
            (slackVars, systemWithSlackVars) = addSlackVariables systemWithoutNonZeroLowerBounds
            (updatedEliminatedVarsMap, updatedSystem) =
              eliminateUnrestrictedLowerBounds
                systemWithSlackVars
                systemBounds
                eliminatedNonZeroLowerBounds
            expectedSystem =
              [ Expr (VarTerm 7 :| []) :>= 0 -- x_7 >= 0
              , Expr (VarTerm 8 :| []) :>= 0 -- x_8 >= 0
              , Expr (VarTerm 5 :| []) :>= 0 -- x_5 >= 0
              , Expr (VarTerm 6 :| []) :>= 0 -- x_6 >= 0
              , Expr (CoeffTerm (-3) 6 :| [CoeffTerm (-1) 3, CoeffTerm 3 5]) :== 0 -- -3x_6 - x_3 + 3x_5 = 0
              , Expr (VarTerm 3 :| []) :>= 0 -- x_3 >= 0
              , Expr
                  ( CoeffTerm (-3) 6
                      :| [CoeffTerm (-1) 4, CoeffTerm (-1) 8, CoeffTerm 3 5, VarTerm 7]
                  )
                  :== 0 -- -3x_6 - x_4 - x_8 + 3x_5 + x_7 = 0
              , Expr (VarTerm 4 :| []) :>= 0 -- x_4 >= 0
              ]
            expectedEliminatedVarExprMap =
              Map.fromList
                [ (1, Expr (VarTerm 5 :| [CoeffTerm (-1) 6]))
                , (2, Expr (VarTerm 7 :| [CoeffTerm (-1) 8]))
                ]
        updatedSystem `shouldBe` expectedSystem
        updatedEliminatedVarsMap `shouldBe` expectedEliminatedVarExprMap

    it
      "eliminateUnrestrictedLowerBounds correctly handles all variables with zero lower bounds"
      $ do
        let simpleSystem =
              [ Expr (VarTerm 1 :| []) :>= 0 -- x_1 >= 0
              , Expr (VarTerm 2 :| []) :>= 0 -- x_2 >= 0
              ]
            systemBounds = deriveBounds simpleSystem
            (eliminatedNonZeroLowerBounds, systemWithoutNonZeroLowerBounds) = eliminateNonZeroLowerBounds simpleSystem Map.empty
            (slackVars, systemWithSlackVars) = addSlackVariables systemWithoutNonZeroLowerBounds
            (updatedEliminatedVarsMap, updatedSystem) =
              eliminateUnrestrictedLowerBounds
                systemWithSlackVars
                systemBounds
                eliminatedNonZeroLowerBounds
            expectedSystem =
              [ (Expr (VarTerm 1 :| [CoeffTerm (-1) 3])) :== 0 -- x_1 - x_3 = 0
              , Expr (VarTerm 3 :| []) :>= 0 -- x_3 >= 0
              , (Expr (VarTerm 2 :| [CoeffTerm (-1) 4])) :== 0 -- x_2 - x_4 = 0
              , Expr (VarTerm 4 :| []) :>= 0 -- x_4 >= 0
              ]
            expectedEliminatedVarExprMap = Map.empty
        updatedSystem `shouldBe` expectedSystem
        updatedEliminatedVarsMap `shouldBe` expectedEliminatedVarExprMap
