module Linear.Tableau.TypesSpec where

import Data.List (nub)
import qualified Data.Set as Set
import Linear.Tableau.Types

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec =
  describe "Tableau Pivoting" $ do
    let -- Sample initial tableau
        initialTableau = Tableau
          { rows =
              [ TableauRow { coeffs = [1, 2], rhs = 3 }   -- x₁ + 2x₂ = 3
              , TableauRow { coeffs = [4, 5], rhs = 6 }   -- 4x₁ +5x₂ = 6
              ]
          , basicVars = Set.fromList [2, 3]        -- x₃ basic in row 0, x₄ in row 1
          }

    it "handles basic pivot operation" $ do
      let result = pivot 0 0 initialTableau
      result.rows `shouldBe`
        [ TableauRow { coeffs = [1, 2], rhs = 3  }    -- x₁ remains same
        , TableauRow { coeffs = [0, -3], rhs = -6 } -- 0x₁ -3x₂ = -6
        ]
      result.basicVars `shouldBe` Set.fromList [0, 3]  -- x₁ now basic in row 0

    it "handles pivot with negative coefficients" $ do
      let tab = Tableau
            { rows = [ TableauRow { coeffs = [-2, 4], rhs = 6 }
            , TableauRow { coeffs = [3, -1], rhs = 2 }
            ]
            , basicVars = Set.fromList [1, 2]
            }
          result = pivot 0 0 tab

      result.rows `shouldBe`
        [ TableauRow { coeffs = [1, -2], rhs = -3 } -- Normalized row
        , TableauRow { coeffs = [0, 5], rhs = 11 }     -- Eliminated row: 3 - (3 * -3) = 0, -1 - (3 * -2) = 5
        ]
      result.basicVars `shouldBe` Set.fromList [0, 2]

    it "maintains row count" $ -- TODO: Why am I testing this?
      property $ \(Positive n) ->
        let tab = Tableau { rows = (replicate n (TableauRow { coeffs = [], rhs = 0 })), basicVars = Set.fromList (replicate n 0) }
        in length (rows (pivot 0 0 tab)) `shouldBe` n

    it "handles fractional results" $ do
      let tab = Tableau
            { rows = [ TableauRow { coeffs = [2, 4], rhs = 6 } ]
            , basicVars = Set.fromList [0]
            }
          result = pivot 0 0 tab

      coeffs (head (rows result)) `shouldBe` [1, 2]
      rhs (head (rows result)) `shouldBe` 3

    it "pivots the wikipeda worked example correctly" $ do
      let tab = Tableau
            { rows = [ TableauRow { coeffs = [1, 2, 3, 4, 0, 0], rhs = 0 }
            , TableauRow { coeffs = [0, 3, 2, 1, 1, 0], rhs = 10 }
            , TableauRow { coeffs = [0, 2, 5, 3, 0, 1], rhs = 15 }
            ]
            , basicVars = Set.fromList [0, 4, 5]
            }
      let step1 = pivot 3 2 tab
      let step1Expected = Tableau
            { rows = [ TableauRow { coeffs = [1, -(2 / 3), -(11 / 3), 0, 0, -(4 / 3)], rhs = -20 }
            , TableauRow { coeffs = [0, 7/3, 1/3, 0, 1,- (1 / 3)], rhs = 5 }
            , TableauRow { coeffs = [0, 2/3, 5/3, 1, 0, 1/3], rhs = 5 }
            ]
            , basicVars = Set.fromList [0, 4, 3]
            }
      step1 `shouldBe` step1Expected

