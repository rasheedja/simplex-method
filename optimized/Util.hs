module Util where

import Simplex;
import Prelude;
import Data.Ratio

import Debug.Trace (trace)

data Opt = Max | Min

type VarConstMap = [(Integer, Rational)]

data PolyConstraint =
  LT VarConstMap Rational       | 
  GT VarConstMap Rational       | 
  LEQ VarConstMap Rational      | 
  GEQ VarConstMap Rational      | 
  EQ VarConstMap Rational       | 
  LTPP VarConstMap VarConstMap  | 
  GTPP VarConstMap VarConstMap  | 
  LEQPP VarConstMap VarConstMap | 
  GEQPP VarConstMap VarConstMap | 
  EQPP VarConstMap VarConstMap; 

varConstMapToLinearPoly :: VarConstMap -> Linear_poly
varConstMapToLinearPoly vcm = LinearPoly (Fmap_of_list (map (\(v,r) -> (nat_of_integer v, r)) vcm))

polyConstraintToConstraint :: PolyConstraint -> Constraint
polyConstraintToConstraint pcs =
  case pcs of
    Util.LT vcm r -> Simplex.LT (varConstMapToLinearPoly vcm) r
    Util.GT vcm r -> Simplex.GT (varConstMapToLinearPoly vcm) r
    Util.LEQ vcm r -> Simplex.LEQ (varConstMapToLinearPoly vcm) r
    Util.GEQ vcm r -> Simplex.GEQ (varConstMapToLinearPoly vcm) r
    Util.EQ vcm r -> Simplex.EQ (varConstMapToLinearPoly vcm) r
    Util.LTPP vcm vcm' -> Simplex.LTPP (varConstMapToLinearPoly vcm) (varConstMapToLinearPoly vcm')
    Util.GTPP vcm vcm' -> Simplex.GTPP (varConstMapToLinearPoly vcm) (varConstMapToLinearPoly vcm')
    Util.LEQPP vcm vcm' -> Simplex.LEQPP (varConstMapToLinearPoly vcm) (varConstMapToLinearPoly vcm')
    Util.GEQPP vcm vcm' -> Simplex.GEQPP (varConstMapToLinearPoly vcm) (varConstMapToLinearPoly vcm')
    Util.EQPP vcm vcm' -> Simplex.EQPP (varConstMapToLinearPoly vcm) (varConstMapToLinearPoly vcm')

showSimplexResult :: Sum [Nat] (Mapping Nat Rational) -> (Bool, [Maybe (Integer, Integer)])
showSimplexResult result =
  case result of
    Inl unsatl  -> 
      (False, map (\n -> Just ((integer_of_nat n), 1)) unsatl)
    Inr mapping ->
      (True,
      map
      (\i ->
        case lookupa mapping (nat_of_integer i) of
          Nothing -> Nothing
          Just r -> Just (n, d)
            where
              (n, d) = (integer_of_int n', integer_of_int d')
              (n', d') = quotient_of r
      )
      [0..3]
      )


optSimplex :: [PolyConstraint] -> Integer -> Opt -> (Bool, [Maybe (Integer, Integer)])
optSimplex pcs varToMaximize minOrMax = simplexHelper (map polyConstraintToConstraint pcs) (nat_of_integer varToMaximize) Nothing
  where
    simplexHelper :: [Constraint] -> Nat -> Maybe (Sum [Nat] (Mapping Nat Rational)) -> (Bool, [Maybe (Integer, Integer)])
    simplexHelper cs' varToMaximize mPreviousSimplexResult =
      case simplexResult of
        Inl unsatl ->
          case mPreviousSimplexResult of
            Just previousSimplexResult -> showSimplexResult previousSimplexResult
            Nothing -> showSimplexResult simplexResult
        Inr mapping ->
          case lookupa mapping varToMaximize of
            Nothing -> trace "Var to maximize not found" $ undefined
            Just r  -> 
              trace (show (both integer_of_int (quotient_of r))) $
              case minOrMax of
                Max -> simplexHelper ((Simplex.GT (LinearPoly (Fmap_of_list [(varToMaximize, 1)])) r) : cs') varToMaximize (Just simplexResult)
                Min -> simplexHelper ((Simplex.LT (LinearPoly (Fmap_of_list [(varToMaximize, 1)])) r) : cs') varToMaximize (Just simplexResult)
      where
        simplexResult = simplex cs'
        both f (a, b) = (f a, f b)
