module Linear.Var.UtilSpec where

import qualified Data.Map as Map
import Linear.Var.Types ( Bounds(..), Var(..) )
import Linear.Var.Util (validateBounds)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Bounds" $ do
  it "validateBounds returns true for valid bounds" $ do
    validateBounds (Map.fromList [(Var 1, Bounds (Just 1) (Just 2))]) `shouldBe` True
    validateBounds (Map.fromList [(Var 1, Bounds (Just 1.1) (Just 1.2))])
      `shouldBe` True
    validateBounds (Map.fromList [(Var 1, Bounds (Just 1) Nothing)]) `shouldBe` True
    validateBounds (Map.fromList [(Var 1, Bounds Nothing (Just 2))]) `shouldBe` True
    validateBounds (Map.fromList [(Var 1, Bounds Nothing Nothing)]) `shouldBe` True
  it "validateBounds returns false for invalid bounds" $ do
    validateBounds (Map.fromList [(Var 1, Bounds (Just 2) (Just 1))]) `shouldBe` False
    validateBounds (Map.fromList [(Var 1, Bounds (Just 1.2) (Just 1.1))])
      `shouldBe` False
