{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  Linear_Poly_Maps(linear_poly_map, equal_linear_poly, get_var_coeff,
                    ordered_keys, set_var_coeff, add, zero, scale)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified Option;
import qualified FSet;
import qualified Set;
import qualified Orderings;
import qualified List;
import qualified Finite_Map;
import qualified Rat;
import qualified Arith;
import qualified Abstract_Linear_Poly;

linear_poly_map ::
  Abstract_Linear_Poly.Linear_poly -> Finite_Map.Fmap Arith.Nat Rat.Rat;
linear_poly_map (Abstract_Linear_Poly.LinearPoly x) = x;

equal_linear_poly ::
  Abstract_Linear_Poly.Linear_poly -> Abstract_Linear_Poly.Linear_poly -> Bool;
equal_linear_poly x y =
  Finite_Map.equal_fmap (linear_poly_map x) (linear_poly_map y);

instance Eq Abstract_Linear_Poly.Linear_poly where {
  a == b = equal_linear_poly a b;
};

get_var_coeff :: Finite_Map.Fmap Arith.Nat Rat.Rat -> Arith.Nat -> Rat.Rat;
get_var_coeff lp v = (case Finite_Map.fmlookup lp v of {
                       Nothing -> Rat.zero_rat;
                       Just c -> c;
                     });

ordered_keys ::
  forall a b. (Eq a, Orderings.Linorder a) => Finite_Map.Fmap a b -> [a];
ordered_keys m = List.sorted_list_of_set (FSet.fset (Finite_Map.fmdom m));

set_var_coeff ::
  Arith.Nat ->
    Rat.Rat ->
      Finite_Map.Fmap Arith.Nat Rat.Rat -> Finite_Map.Fmap Arith.Nat Rat.Rat;
set_var_coeff v c lp =
  (if Rat.equal_rat c Rat.zero_rat then Finite_Map.fmdrop v lp
    else Finite_Map.fmupd v c lp);

add_monom ::
  Rat.Rat ->
    Arith.Nat ->
      Finite_Map.Fmap Arith.Nat Rat.Rat -> Finite_Map.Fmap Arith.Nat Rat.Rat;
add_monom c v lp = set_var_coeff v (Rat.plus_rat c (get_var_coeff lp v)) lp;

add ::
  Finite_Map.Fmap Arith.Nat Rat.Rat ->
    Finite_Map.Fmap Arith.Nat Rat.Rat -> Finite_Map.Fmap Arith.Nat Rat.Rat;
add lp1 lp2 =
  List.foldl (\ lp v -> add_monom (get_var_coeff lp1 v) v lp) lp2
    (ordered_keys lp1);

zero :: Finite_Map.Fmap Arith.Nat Rat.Rat;
zero = Finite_Map.fmempty;

scale ::
  Rat.Rat ->
    Finite_Map.Fmap Arith.Nat Rat.Rat -> Finite_Map.Fmap Arith.Nat Rat.Rat;
scale r lp =
  (if Rat.equal_rat r Rat.zero_rat then Finite_Map.fmempty
    else Finite_Map.fmmap (Rat.times_rat r) lp);

}
