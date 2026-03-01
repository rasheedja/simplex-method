{-# LANGUAGE ScopedTypeVariables #-}

module Linear.Simplex.UtilSpec where

import Prelude hiding (EQ)

import Control.Exception (evaluate)
import qualified Data.Map as M
import Test.Hspec (Spec, anyErrorCall, describe, expectationFailure, it, shouldBe, shouldThrow)
import Test.QuickCheck (Positive (..), property)

import Linear.Simplex.Types
  ( DictValue (..)
  , ObjectiveFunction (..)
  , PivotObjective (..)
  , PolyConstraint (..)
  , TableauRow (..)
  , VarLitMapSum
  )
import Linear.Simplex.Util
  ( combineVarLitMapSums
  , dictionaryFormToTableau
  , foldVarLitMap
  , insertPivotObjectiveToDict
  , isMax
  , simplifySystem
  , tableauInDictionaryForm
  )

spec :: Spec
spec = do
  describe "isMax" $ do
    it "returns True for Max" $ do
      isMax (Max (M.fromList [(1, 1)])) `shouldBe` True

    it "returns False for Min" $ do
      isMax (Min (M.fromList [(1, 1)])) `shouldBe` False

    it "returns True for Max with empty coefficients" $ do
      isMax (Max M.empty) `shouldBe` True

    it "returns False for Min with empty coefficients" $ do
      isMax (Min M.empty) `shouldBe` False

  describe "simplifySystem" $ do
    describe "Unit tests" $ do
      it "returns empty list for empty input" $ do
        simplifySystem [] `shouldBe` []

      it "preserves a single LEQ constraint" $ do
        let c = LEQ (M.fromList [(1, 2)]) 10
        simplifySystem [c] `shouldBe` [c]

      it "preserves a single GEQ constraint" $ do
        let c = GEQ (M.fromList [(1, 1)]) 5
        simplifySystem [c] `shouldBe` [c]

      it "preserves a single EQ constraint" $ do
        let c = EQ (M.fromList [(1, 3)]) 15
        simplifySystem [c] `shouldBe` [c]

      it "reduces matching LEQ and GEQ into EQ" $ do
        let lhs = M.fromList [(1, 1)]
            rhs = 5
        simplifySystem [LEQ lhs rhs, GEQ lhs rhs] `shouldBe` [EQ lhs rhs]

      it "reduces matching GEQ and LEQ into EQ" $ do
        let lhs = M.fromList [(1, 1)]
            rhs = 5
        simplifySystem [GEQ lhs rhs, LEQ lhs rhs] `shouldBe` [EQ lhs rhs]

      it "keeps non-matching LEQ and GEQ separate" $ do
        let c1 = LEQ (M.fromList [(1, 1)]) 10
            c2 = GEQ (M.fromList [(1, 1)]) 5
        simplifySystem [c1, c2] `shouldBe` [c1, c2]

      it "removes duplicate constraints" $ do
        let c = LEQ (M.fromList [(1, 2)]) 10
        simplifySystem [c, c] `shouldBe` [c]

      it "reduces LEQ matching an existing EQ" $ do
        let lhs = M.fromList [(1, 1)]
            rhs = 5
        simplifySystem [LEQ lhs rhs, EQ lhs rhs] `shouldBe` [EQ lhs rhs]

      it "reduces GEQ matching an existing EQ" $ do
        let lhs = M.fromList [(1, 1)]
            rhs = 5
        simplifySystem [GEQ lhs rhs, EQ lhs rhs] `shouldBe` [EQ lhs rhs]

      it "keeps EQ and removes matching LEQ from rest" $ do
        let lhs = M.fromList [(1, 1)]
            rhs = 5
        simplifySystem [EQ lhs rhs, LEQ lhs rhs] `shouldBe` [EQ lhs rhs]

      it "preserves unrelated constraints alongside a reduction" $ do
        let lhs = M.fromList [(1, 1)]
            rhs = 5
            unrelated = GEQ (M.fromList [(2, 3)]) 7
        simplifySystem [LEQ lhs rhs, GEQ lhs rhs, unrelated]
          `shouldBe` [EQ lhs rhs, unrelated]

    describe "Properties" $ do
      it "idempotent: simplifying twice equals simplifying once" $
        property $
          \(constraints :: [(Int, Rational, Rational)]) ->
            let mkConstraint (tag, coeff, rhs) =
                  case tag `mod` 3 of
                    0 -> LEQ (M.fromList [(1, coeff)]) rhs
                    1 -> GEQ (M.fromList [(1, coeff)]) rhs
                    _ -> EQ (M.fromList [(1, coeff)]) rhs
                system = map mkConstraint constraints
            in  simplifySystem (simplifySystem system) == simplifySystem system

      it "never increases the number of constraints" $
        property $
          \(constraints :: [(Int, Rational, Rational)]) ->
            let mkConstraint (tag, coeff, rhs) =
                  case tag `mod` 3 of
                    0 -> LEQ (M.fromList [(1, coeff)]) rhs
                    1 -> GEQ (M.fromList [(1, coeff)]) rhs
                    _ -> EQ (M.fromList [(1, coeff)]) rhs
                system = map mkConstraint constraints
            in  length (simplifySystem system) <= length system

      it "matching LEQ and GEQ always produce EQ" $
        property $
          \(coeff :: Rational, rhs :: Rational) ->
            let lhs = M.fromList [(1, coeff)]
            in  simplifySystem [LEQ lhs rhs, GEQ lhs rhs] == [EQ lhs rhs]

  describe "dictionaryFormToTableau and tableauInDictionaryForm" $ do
    describe "Unit tests" $ do
      it "converts a simple dictionary entry to tableau form" $ do
        -- Dict: x₁ = 3 + 2*x₂ means DictValue {varMapSum = {2: 2}, constant = 3}
        -- Tableau: x₁ - 2*x₂ = 3 means TableauRow {lhs = {1: 1, 2: -2}, rhs = 3}
        let dict = M.fromList [(1, DictValue {varMapSum = M.fromList [(2, 2)], constant = 3})]
            tableau = dictionaryFormToTableau dict
        case M.lookup 1 tableau of
          Just row -> do
            row.lhs `shouldBe` M.fromList [(1, 1), (2, -2)]
            row.rhs `shouldBe` 3
          Nothing -> expectationFailure "Expected row for var 1"

      it "converts an empty dictionary to empty tableau" $ do
        dictionaryFormToTableau M.empty `shouldBe` M.empty

      it "converts a simple tableau entry to dictionary form" $ do
        -- Tableau: 1*x₁ + (-2)*x₂ = 3 means x₁ = 3 + 2*x₂
        let tableau = M.fromList [(1, TableauRow {lhs = M.fromList [(1, 1), (2, -2)], rhs = 3})]
            dict = tableauInDictionaryForm tableau
        case M.lookup 1 dict of
          Just entry -> do
            entry.varMapSum `shouldBe` M.fromList [(2, 2)]
            entry.constant `shouldBe` 3
          Nothing -> expectationFailure "Expected entry for var 1"

      it "converts an empty tableau to empty dictionary" $ do
        tableauInDictionaryForm M.empty `shouldBe` M.empty

    describe "Round-trip properties" $ do
      it "tableauInDictionaryForm . dictionaryFormToTableau is identity" $
        property $
          \(pairs :: [(Int, [(Int, Rational)], Rational)]) ->
            let mkEntry (var, coeffs, c) =
                  let basicVar = abs var + 1
                      -- Ensure no coefficient var clashes with the basic var
                      safeCoeffs = filter (\(v, _) -> abs v + 1000 /= basicVar) coeffs
                  in  (basicVar, DictValue {varMapSum = M.fromList (map (\(v, coeff) -> (abs v + 1000, coeff)) safeCoeffs), constant = c})
                dict = M.fromList (map mkEntry pairs)
                roundTripped = tableauInDictionaryForm (dictionaryFormToTableau dict)
            in  roundTripped == dict

      it "dictionaryFormToTableau . tableauInDictionaryForm is identity for well-formed tableaux" $
        property $
          \(var :: Positive Int, coeffPairs :: [(Positive Int, Rational)], c :: Rational) ->
            let basicVar = getPositive var
                -- Ensure basic var has coefficient 1 (well-formed tableau)
                otherCoeffs = M.fromList [(getPositive v + basicVar, coeff) | (v, coeff) <- coeffPairs]
                lhs = M.insert basicVar 1 otherCoeffs
                tableau = M.singleton basicVar (TableauRow {lhs = lhs, rhs = c})
                roundTripped = dictionaryFormToTableau (tableauInDictionaryForm tableau)
            in  roundTripped == tableau

  describe "combineVarLitMapSums" $ do
    describe "Unit tests" $ do
      it "combines two disjoint maps" $ do
        let m1 = M.fromList [(1, 3)]
            m2 = M.fromList [(2, 5)]
        combineVarLitMapSums m1 m2 `shouldBe` M.fromList [(1, 3), (2, 5)]

      it "sums values for overlapping keys" $ do
        let m1 = M.fromList [(1, 3), (2, 4)]
            m2 = M.fromList [(1, 7), (3, 1)]
        combineVarLitMapSums m1 m2 `shouldBe` M.fromList [(1, 10), (2, 4), (3, 1)]

      it "returns other map when one is empty" $ do
        let m = M.fromList [(1, 5)]
        combineVarLitMapSums M.empty m `shouldBe` m
        combineVarLitMapSums m M.empty `shouldBe` m

      it "returns empty for two empty maps" $ do
        combineVarLitMapSums M.empty M.empty `shouldBe` (M.empty :: VarLitMapSum)

    describe "Properties" $ do
      it "is commutative" $
        property $
          \(pairs1 :: [(Int, Rational)], pairs2 :: [(Int, Rational)]) ->
            let m1 = M.fromList pairs1
                m2 = M.fromList pairs2
            in  combineVarLitMapSums m1 m2 == combineVarLitMapSums m2 m1

      it "has empty map as identity" $
        property $
          \(pairs :: [(Int, Rational)]) ->
            let m = M.fromList pairs
            in  combineVarLitMapSums m M.empty == m
                  && combineVarLitMapSums M.empty m == m

      it "is associative" $
        property $
          \(p1 :: [(Int, Rational)], p2 :: [(Int, Rational)], p3 :: [(Int, Rational)]) ->
            let m1 = M.fromList p1
                m2 = M.fromList p2
                m3 = M.fromList p3
            in  combineVarLitMapSums (combineVarLitMapSums m1 m2) m3
                  == combineVarLitMapSums m1 (combineVarLitMapSums m2 m3)

  describe "foldVarLitMap" $ do
    describe "Unit tests" $ do
      it "returns the single map for a singleton list" $ do
        let m = M.fromList [(1, 5), (2, 3)]
        foldVarLitMap [m] `shouldBe` m

      it "combines two maps" $ do
        let m1 = M.fromList [(1, 3)]
            m2 = M.fromList [(1, 7), (2, 4)]
        foldVarLitMap [m1, m2] `shouldBe` M.fromList [(1, 10), (2, 4)]

      it "combines three maps" $ do
        let m1 = M.fromList [(1, 1)]
            m2 = M.fromList [(1, 2), (2, 3)]
            m3 = M.fromList [(2, 7), (3, 5)]
        foldVarLitMap [m1, m2, m3] `shouldBe` M.fromList [(1, 3), (2, 10), (3, 5)]

      it "throws error on empty list" $ do
        evaluate (foldVarLitMap []) `shouldThrow` anyErrorCall

    describe "Properties" $ do
      it "folding a singleton list is identity" $
        property $
          \(pairs :: [(Int, Rational)]) ->
            let m = M.fromList pairs
            in  foldVarLitMap [m] == m

      it "folding two maps equals combineVarLitMapSums" $
        property $
          \(p1 :: [(Int, Rational)], p2 :: [(Int, Rational)]) ->
            let m1 = M.fromList p1
                m2 = M.fromList p2
            in  foldVarLitMap [m1, m2] == combineVarLitMapSums m1 m2

  describe "insertPivotObjectiveToDict" $ do
    it "inserts objective into empty dictionary" $ do
      let obj = PivotObjective {variable = 1, function = M.fromList [(2, 3)], constant = 5}
          result = insertPivotObjectiveToDict obj M.empty
      case M.lookup 1 result of
        Just entry -> do
          entry.varMapSum `shouldBe` M.fromList [(2, 3)]
          entry.constant `shouldBe` 5
        Nothing -> expectationFailure "Expected entry for objective variable"

    it "inserts objective into non-empty dictionary without overwriting others" $ do
      let existing = M.fromList [(2, DictValue {varMapSum = M.fromList [(3, 1)], constant = 10})]
          obj = PivotObjective {variable = 1, function = M.fromList [(3, 4)], constant = 7}
          result = insertPivotObjectiveToDict obj existing
      M.size result `shouldBe` 2
      case M.lookup 2 result of
        Just entry -> entry.constant `shouldBe` 10
        Nothing -> expectationFailure "Existing entry should be preserved"

    it "overwrites existing entry with same variable" $ do
      let existing = M.fromList [(1, DictValue {varMapSum = M.fromList [(2, 1)], constant = 3})]
          obj = PivotObjective {variable = 1, function = M.fromList [(2, 99)], constant = 42}
          result = insertPivotObjectiveToDict obj existing
      M.size result `shouldBe` 1
      case M.lookup 1 result of
        Just entry -> do
          entry.varMapSum `shouldBe` M.fromList [(2, 99)]
          entry.constant `shouldBe` 42
        Nothing -> expectationFailure "Expected updated entry"
