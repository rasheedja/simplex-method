module Tester where

import Simplex
import SoplexTranslator
import System.IO.Unsafe

import Test.QuickCheck
import Test.QuickCheck.Gen
-- import Debug.Trace
-- instance Arbitrary ObjectiveFunction where
--   arbitrary = undefined
--     where
--       variables = listOf1 (Gen Positive)

data System = System (ObjectiveFunction, [PolyConstraint])

prop_matchSoplex system = 
  extractObjectiveValue (uncurry twoPhaseSimplex system) == unsafePerformIO (findObjectiveValueUsingSoplex system)
  where types = system :: (ObjectiveFunction, [PolyConstraint])

prop_RevRev xs = reverse (reverse xs) == xs
  where types = xs :: [(Integer, Rational)]

-- prop_ObjectiveFunction varCoeff = 
--   trace (show varCoeff2) $ reverse (reverse varCoeff2) == varCoeff2
--   -- forAll obj $ \o -> 
--   --   forAll rows $ \rs ->
--   --     let
--   --       rowCoeffs = vectorOf (length rs)
--   --     in
--   --       forAll coeffs $ \c ->
--   --         trace (show (zip r c)) $ reverse (reverse r) == r
--   where 
--     types = (varCoeff :: [(Positive Integer, Rational)])
--     varCoeff2 = map (\(v,c) -> (getPositive v, c)) varCoeff
--     -- obj   = listOf (fmap (\(v,c) -> (getPositive v, c) varCoeff)
--     -- rows  = listOf1 (sublistOf (map getPositive vars))
    
--     -- createCoefficients :: [Positive Integer] -> [(Integer, Rational)]
--     -- createCoefficients ps = map (\p -> (getPositive p, arbitrary)) ps

prop_Simplex vars =
  forAllBlind objs $ \obj ->
    not (null obj) ==>
    let
      
      coeffs :: Gen [Integer]
      coeffs = vectorOf (length obj) arbitrary
    in
      forAllBlind coeffs $ \coeff ->
        let varConstMap :: VarConstMap
            varConstMap = (zip obj (map toRational coeff))
            objective = oneof [return (Max varConstMap), return (Min varConstMap)]
        in
          forAllBlind objective $ \obje ->
            let
              rowsG = listOf1 (sublistOf iVars)
            in
              forAllBlind rowsG $ \rows ->
                not (any null rows) ==>
                let
                  rowsCoeffsG :: Gen [[Integer]]
                  rowsCoeffsG = vectorOf (length rows) infiniteList

                  rowsRhsG :: Gen [Integer]
                  rowsRhsG = vectorOf (length rows) arbitrary
                in
                  forAllBlind rowsCoeffsG $ \rowsCoeffs ->
                    let
                      iRows = zip [0..] rows
                      rowsWithCoeffs = map (\(i, row) -> zip row (map toRational (rowsCoeffs !! i))) iRows
                      -- map () rows
                      -- rowsWithCoeffs = imap (\(i, row) -> zip row (rowsCoeffs ! i)) rows 
                      -- pcsG = map (\rowWithCoeffs -> oneof [return (LEQ rowWithCoeffs 1.0), return (GEQ rowWithCoeffs 1.0), return (Simplex.EQ rowWithCoeffs 1.0)]) rowsWithCoeffs
                      -- pcs = oneof pcsG
                      constructorG = vectorOf (length rowsWithCoeffs) $ oneof [return LEQ, return GEQ, return Simplex.EQ]
                      -- pcs = vectorOf (length rowsWithCoeffs) $ map (\rowWithCoeffs -> oneof [return (LEQ rowWithCoeffs 1.0), return (GEQ rowWithCoeffs 1.0), return (Simplex.EQ rowWithCoeffs 1.0)]) rowsWithCoeffs
                    in
                      forAllBlind constructorG $ \constructor ->
                          forAllBlind rowsRhsG $ \rowsRhs ->
                            let
                              pcs = map (\(i, c) -> (c (rowsWithCoeffs !! i) (toRational (rowsRhs !! i)))) (zip [0..] constructor)
                            in
                              counterexample 
                              (show (obje, pcs))
                              -- Write each generated test case to a file
                              $ extractObjectiveValue (twoPhaseSimplex obje pcs) == unsafePerformIO (findObjectiveValueUsingSoplex (obje, pcs))
                              -- 2/3 == (-2/3) not matching
                              -- Add condition so vars only appear once on each row
                              -- Or support this in our version
                -- length (rows) == length (rows)
  where
    types = (vars :: [Positive Integer])
    iVars = (map getPositive vars)
    objs = sublistOf iVars
-- instance Arbitrary (ObjectiveFunction, [PolyConstraint]) where

testQuick :: (ObjectiveFunction, [PolyConstraint])
testQuick =
  (
    Min [(1, 2)],
    [
      GEQ [(2, 1), (1, 0)] (-1),
      LEQ [(2, -1), (1, -2)] (-2)
    ]
  )
