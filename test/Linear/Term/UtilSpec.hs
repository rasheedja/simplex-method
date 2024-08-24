-- |
-- Module: Linear.Term.TypesSpec
-- Description: Tests for Linear.Term.Types
-- Copyright: (c) Junaid Rasheed, 2020-2024
-- License: BSD-3
-- Maintainer: Junaid Rasheed <jrasheed178@gmail.com>
-- Stability: experimental
module Linear.Term.UtilSpec where

import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import Linear.Term.Types
  ( Term (..)
  , TermVarsOnly (..)
  )
import Linear.Term.Util
  ( isConstTerm
  , negateTerm
  , normalizeTerms
  , simplifyTerm
  , termToTermVarsOnly
  , termVar
  , unsafeTermToTermVarsOnly
  , zeroConstTerm
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (counterexample)
import TestUtil (evalTerm, genVarMap)
import Prelude

spec :: Spec
spec = do
  describe "Term" $ do
    prop "simplifying leads to same evaluation" $ \term -> do
      varMap <- maybe (pure Map.empty) (genVarMap . List.singleton) $ termVar term
      let simplifiedTerm = simplifyTerm term
          termEval = evalTerm varMap term
          simplifiedTermEval = evalTerm varMap simplifiedTerm
      pure
        $ counterexample
          ( "term: "
              <> show term
              <> "simplifiedTerm: "
              <> show simplifiedTerm
              <> "\nvarMap: "
              <> show varMap
              <> "\ntermEval: "
              <> show termEval
              <> "\nsimplifiedTermEval: "
              <> show simplifiedTermEval
          )
        $ evalTerm varMap (simplifyTerm term) == evalTerm varMap term
    prop "simplifyTerm is idempotent" $ \term -> do
      let simplifiedTerm = simplifyTerm term
          simplifiedTwiceTerm = simplifyTerm simplifiedTerm
      counterexample
        ( "term: "
            <> show term
            <> "\nsimplifiedTerm: "
            <> show simplifiedTerm
            <> "\nsimplifiedTwiceTerm: "
            <> show simplifiedTwiceTerm
        )
        $ simplifiedTwiceTerm == simplifiedTerm
    prop "negating and evaluating is the same as negating the evaluation" $ \term -> do
      varMap <- maybe (pure $ Map.empty) (genVarMap . List.singleton) $ termVar term
      let negatedTerm = negateTerm term
          termEval = evalTerm varMap term
          negatedTermEval = evalTerm varMap negatedTerm
      pure
        $ counterexample
          ( "term: "
              <> show term
              <> "\nnegatedTerm: "
              <> show negatedTerm
              <> "\nvarMap: "
              <> show varMap
              <> "\ntermEval: "
              <> show termEval
              <> "\nnegatedTermEval: "
              <> show negatedTermEval
          )
        $ negate termEval == negatedTermEval
    prop "negating  twice is the same as not negating" $ \term -> do
      let simplifiedTerm = simplifyTerm term
          negatedTwiceSimpleTerm = negateTerm (negateTerm simplifiedTerm)
      counterexample
        ( "term: "
            <> show term
            <> "\nsimplifiedTerm: "
            <> show simplifiedTerm
            <> "\nnegatedTwiceSimpleTerm: "
            <> show negatedTwiceSimpleTerm
        )
        $ negatedTwiceSimpleTerm == simplifiedTerm
    prop "zeroConstTerm correctly zeroes constant terms" $ \term -> do
      let termZeroedConsts = zeroConstTerm term
      counterexample
        ( "term: "
            <> show term
            <> "\ntermZeroedConsts: "
            <> show termZeroedConsts
        )
        $ case term of
          ConstTerm _ -> termZeroedConsts == ConstTerm 0
          _ -> termZeroedConsts == term
    it "isConstTerm correctly identifies constant terms" $ do
      isConstTerm (ConstTerm 0) `shouldBe` True
      isConstTerm (ConstTerm 1) `shouldBe` True
      isConstTerm (CoeffTerm 1 1) `shouldBe` False
      isConstTerm (VarTerm 1) `shouldBe` False
    it "termToTermVarsOnly correctly converts terms to vars only" $ do
      termToTermVarsOnly (ConstTerm 0) `shouldSatisfy` Either.isLeft
      termToTermVarsOnly (ConstTerm 1) `shouldSatisfy` Either.isLeft
      termToTermVarsOnly (CoeffTerm 1 1) `shouldBe` Right (CoeffTermVO 1 1)
      termToTermVarsOnly (VarTerm 1) `shouldBe` Right (VarTermVO 1)
    it "unsafeTermToTermVarsOnly correctly converts terms without vars" $ do
      unsafeTermToTermVarsOnly (CoeffTerm 1 1) `shouldBe` (CoeffTermVO 1 1)
      unsafeTermToTermVarsOnly (VarTerm 1) `shouldBe` (VarTermVO 1)
    prop "normalizeTerms is idempotent" $ \terms -> do
      let normalizedTerms = normalizeTerms terms
          normalizedTwiceTerms = normalizeTerms normalizedTerms
      counterexample
        ( "terms: "
            <> show terms
            <> "\nnormalizedTerms: "
            <> show normalizedTerms
            <> "\nnormalizedTwiceTerms: "
            <> show normalizedTwiceTerms
        )
        $ normalizedTwiceTerms == normalizedTerms
