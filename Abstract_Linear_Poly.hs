{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  Abstract_Linear_Poly(Linear_poly(..), var, coeff, vars_list, max_var, valuate,
                        is_monom, monom_var, monom_coeff, plus_linear_poly,
                        zero_linear_poly, scaleRat_linear_poly,
                        minus_linear_poly)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified Groups_List;
import qualified Option;
import qualified Simplex_Algebra;
import qualified Orderings;
import qualified List;
import qualified HOL;
import qualified Linear_Poly_Maps;
import qualified Finite_Map;
import qualified Rat;
import qualified Arith;

newtype Linear_poly = LinearPoly (Finite_Map.Fmap Arith.Nat Rat.Rat);

var :: Arith.Nat -> Linear_poly;
var x =
  LinearPoly (Linear_Poly_Maps.set_var_coeff x Rat.one_rat Finite_Map.fmempty);

coeff :: Linear_poly -> Arith.Nat -> Rat.Rat;
coeff lp = Linear_Poly_Maps.get_var_coeff (Linear_Poly_Maps.linear_poly_map lp);

vars_list :: Linear_poly -> [Arith.Nat];
vars_list lp =
  Linear_Poly_Maps.ordered_keys (Linear_Poly_Maps.linear_poly_map lp);

max_var :: Linear_poly -> Arith.Nat;
max_var lp =
  let {
    vl = vars_list lp;
  } in (if null vl then Arith.zero_nat
         else List.foldl Orderings.max (List.hd vl) (List.tl vl));

valuate ::
  forall a.
    (Simplex_Algebra.Rational_vector a) => Linear_poly -> (Arith.Nat -> a) -> a;
valuate lp val =
  let {
    lpm = Linear_Poly_Maps.linear_poly_map lp;
  } in Groups_List.sum_list
         (map (\ x ->
                Simplex_Algebra.scaleRat
                  (Option.the (Finite_Map.fmlookup lpm x)) (val x))
           (vars_list lp));

is_monom :: Linear_poly -> Bool;
is_monom l = Arith.equal_nat (List.size_list (vars_list l)) Arith.one_nat;

monom_var :: Linear_poly -> Arith.Nat;
monom_var l = max_var l;

monom_coeff :: Linear_poly -> Rat.Rat;
monom_coeff l = coeff l (monom_var l);

plus_linear_poly :: Linear_poly -> Linear_poly -> Linear_poly;
plus_linear_poly p1 p2 =
  LinearPoly
    (Linear_Poly_Maps.add (Linear_Poly_Maps.linear_poly_map p1)
      (Linear_Poly_Maps.linear_poly_map p2));

zero_linear_poly :: Linear_poly;
zero_linear_poly = LinearPoly Linear_Poly_Maps.zero;

scaleRat_linear_poly :: Rat.Rat -> Linear_poly -> Linear_poly;
scaleRat_linear_poly r p =
  LinearPoly (Linear_Poly_Maps.scale r (Linear_Poly_Maps.linear_poly_map p));

uminus_linear_poly :: Linear_poly -> Linear_poly;
uminus_linear_poly lp = scaleRat_linear_poly (Rat.uminus_rat Rat.one_rat) lp;

minus_linear_poly :: Linear_poly -> Linear_poly -> Linear_poly;
minus_linear_poly lp1 lp2 = plus_linear_poly lp1 (uminus_linear_poly lp2);

}
