{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module Simplex(Constraint, simplex) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified Simplex_Auxiliary;
import qualified Fun;
import qualified Linear_Poly_Maps;
import qualified Product_Type;
import qualified Simplex_Algebra;
import qualified Orderings;
import qualified Sum_Type;
import qualified List;
import qualified HOL;
import qualified Set;
import qualified Option;
import qualified Rat;
import qualified QDelta;
import qualified Abstract_Linear_Poly;
import qualified Mapping;
import qualified Arith;

data Atom a = Leq Arith.Nat a | Geq Arith.Nat a;

data State a b =
  State [(Arith.Nat, Abstract_Linear_Poly.Linear_poly)]
    (Mapping.Mapping Arith.Nat (a, b)) (Mapping.Mapping Arith.Nat (a, b))
    (Mapping.Mapping Arith.Nat b) Bool (Maybe [a]);

data Istate a =
  IState Arith.Nat [(Arith.Nat, Abstract_Linear_Poly.Linear_poly)]
    [(a, Atom QDelta.QDelta)]
    (Abstract_Linear_Poly.Linear_poly -> Maybe Arith.Nat) [a];

data Direction a b =
  Direction (b -> b -> Bool) (State a b -> Mapping.Mapping Arith.Nat (a, b))
    (State a b -> Mapping.Mapping Arith.Nat (a, b))
    (State a b -> Arith.Nat -> Maybe b) (State a b -> Arith.Nat -> Maybe b)
    (State a b -> Arith.Nat -> a) (State a b -> Arith.Nat -> a)
    ((Mapping.Mapping Arith.Nat (a, b) -> Mapping.Mapping Arith.Nat (a, b)) ->
      State a b -> State a b)
    (Arith.Nat -> b -> Atom b) (Arith.Nat -> b -> Atom b)
    (Rat.Rat -> Rat.Rat -> Bool);

data Constraint = LT Abstract_Linear_Poly.Linear_poly Rat.Rat
  | GT Abstract_Linear_Poly.Linear_poly Rat.Rat
  | LEQ Abstract_Linear_Poly.Linear_poly Rat.Rat
  | GEQ Abstract_Linear_Poly.Linear_poly Rat.Rat
  | EQ Abstract_Linear_Poly.Linear_poly Rat.Rat
  | LTPP Abstract_Linear_Poly.Linear_poly Abstract_Linear_Poly.Linear_poly
  | GTPP Abstract_Linear_Poly.Linear_poly Abstract_Linear_Poly.Linear_poly
  | LEQPP Abstract_Linear_Poly.Linear_poly Abstract_Linear_Poly.Linear_poly
  | GEQPP Abstract_Linear_Poly.Linear_poly Abstract_Linear_Poly.Linear_poly
  | EQPP Abstract_Linear_Poly.Linear_poly Abstract_Linear_Poly.Linear_poly;

data Ns_constraint a = LEQ_ns Abstract_Linear_Poly.Linear_poly a
  | GEQ_ns Abstract_Linear_Poly.Linear_poly a;

lhs :: (Arith.Nat, Abstract_Linear_Poly.Linear_poly) -> Arith.Nat;
lhs (l, r) = l;

rhs ::
  (Arith.Nat, Abstract_Linear_Poly.Linear_poly) ->
    Abstract_Linear_Poly.Linear_poly;
rhs (l, r) = r;

gelb :: forall a. (Eq a) => (a -> a -> Bool) -> a -> Maybe a -> Bool;
gelb lt c b = (case b of {
                Nothing -> True;
                Just ba -> lt ba c || ba == c;
              });

geub :: forall a. (Eq a) => (a -> a -> Bool) -> a -> Maybe a -> Bool;
geub lt c b = (case b of {
                Nothing -> False;
                Just ba -> lt ba c || ba == c;
              });

gtlb :: forall a b. (a -> b -> Bool) -> b -> Maybe a -> Bool;
gtlb lt c b = (case b of {
                Nothing -> True;
                Just ba -> lt ba c;
              });

leub :: forall a. (Eq a) => (a -> a -> Bool) -> a -> Maybe a -> Bool;
leub lt c b = (case b of {
                Nothing -> True;
                Just ba -> lt c ba || c == ba;
              });

ltlb :: forall a b. (a -> b -> Bool) -> a -> Maybe b -> Bool;
ltlb lt c b = (case b of {
                Nothing -> False;
                Just a -> lt c a;
              });

ltub :: forall a b. (a -> b -> Bool) -> a -> Maybe b -> Bool;
ltub lt c b = (case b of {
                Nothing -> True;
                Just a -> lt c a;
              });

poly :: forall a. Ns_constraint a -> Abstract_Linear_Poly.Linear_poly;
poly (LEQ_ns p a) = p;
poly (GEQ_ns p a) = p;

lvars :: [(Arith.Nat, Abstract_Linear_Poly.Linear_poly)] -> Set.Set Arith.Nat;
lvars t = Set.Set (map lhs t);

constraint_to_qdelta_constraint :: Constraint -> [Ns_constraint QDelta.QDelta];
constraint_to_qdelta_constraint (LT l r) =
  [LEQ_ns l (QDelta.QDelta r (Rat.uminus_rat Rat.one_rat))];
constraint_to_qdelta_constraint (GT l r) =
  [GEQ_ns l (QDelta.QDelta r Rat.one_rat)];
constraint_to_qdelta_constraint (LEQ l r) =
  [LEQ_ns l (QDelta.QDelta r Rat.zero_rat)];
constraint_to_qdelta_constraint (GEQ l r) =
  [GEQ_ns l (QDelta.QDelta r Rat.zero_rat)];
constraint_to_qdelta_constraint (EQ l r) =
  [LEQ_ns l (QDelta.QDelta r Rat.zero_rat),
    GEQ_ns l (QDelta.QDelta r Rat.zero_rat)];
constraint_to_qdelta_constraint (LTPP l1 l2) =
  [LEQ_ns (Abstract_Linear_Poly.minus_linear_poly l1 l2)
     (QDelta.QDelta Rat.zero_rat (Rat.uminus_rat Rat.one_rat))];
constraint_to_qdelta_constraint (GTPP l1 l2) =
  [GEQ_ns (Abstract_Linear_Poly.minus_linear_poly l1 l2)
     (QDelta.QDelta Rat.zero_rat Rat.one_rat)];
constraint_to_qdelta_constraint (LEQPP l1 l2) =
  [LEQ_ns (Abstract_Linear_Poly.minus_linear_poly l1 l2) QDelta.zero_QDelta];
constraint_to_qdelta_constraint (GEQPP l1 l2) =
  [GEQ_ns (Abstract_Linear_Poly.minus_linear_poly l1 l2) QDelta.zero_QDelta];
constraint_to_qdelta_constraint (EQPP l1 l2) =
  [LEQ_ns (Abstract_Linear_Poly.minus_linear_poly l1 l2) QDelta.zero_QDelta,
    GEQ_ns (Abstract_Linear_Poly.minus_linear_poly l1 l2) QDelta.zero_QDelta];

i_constraint_to_qdelta_constraint ::
  forall a. (a, Constraint) -> [(a, Ns_constraint QDelta.QDelta)];
i_constraint_to_qdelta_constraint (i, c) =
  map (\ a -> (i, a)) (constraint_to_qdelta_constraint c);

to_ns :: forall a. [(a, Constraint)] -> [(a, Ns_constraint QDelta.QDelta)];
to_ns l = concatMap i_constraint_to_qdelta_constraint l;

b_i_l :: forall a b. State a b -> Mapping.Mapping Arith.Nat (a, b);
b_i_l (State x1 x2 x3 x4 x5 x6) = x2;

indexl :: forall a b. State a b -> Arith.Nat -> a;
indexl s = (fst . Option.the) . Mapping.lookup (b_i_l s);

b_i_u :: forall a b. State a b -> Mapping.Mapping Arith.Nat (a, b);
b_i_u (State x1 x2 x3 x4 x5 x6) = x3;

indexu :: forall a b. State a b -> Arith.Nat -> a;
indexu s = (fst . Option.the) . Mapping.lookup (b_i_u s);

boundsl :: forall a b. State a b -> Arith.Nat -> Maybe b;
boundsl s = Option.map_option snd . Mapping.lookup (b_i_l s);

boundsu :: forall a b. State a b -> Arith.Nat -> Maybe b;
boundsu s = Option.map_option snd . Mapping.lookup (b_i_u s);

delta_0_val ::
  Ns_constraint QDelta.QDelta -> (Arith.Nat -> QDelta.QDelta) -> Rat.Rat;
delta_0_val (LEQ_ns lll rrr) vl =
  QDelta.delta_0 (Abstract_Linear_Poly.valuate lll vl) rrr;
delta_0_val (GEQ_ns lll rrr) vl =
  QDelta.delta_0 rrr (Abstract_Linear_Poly.valuate lll vl);

delta_0_val_min ::
  [Ns_constraint QDelta.QDelta] -> (Arith.Nat -> QDelta.QDelta) -> Rat.Rat;
delta_0_val_min [] vl = Rat.one_rat;
delta_0_val_min (h : t) vl =
  Orderings.min (delta_0_val_min t vl) (delta_0_val h vl);

map2fun ::
  forall a. (Arith.Zero a) => Mapping.Mapping Arith.Nat a -> Arith.Nat -> a;
map2fun v = (\ x -> (case Mapping.lookup v x of {
                      Nothing -> Arith.zero;
                      Just y -> y;
                    }));

from_ns ::
  Mapping.Mapping Arith.Nat QDelta.QDelta ->
    [Ns_constraint QDelta.QDelta] -> Mapping.Mapping Arith.Nat Rat.Rat;
from_ns vl cs =
  let {
    delta = delta_0_val_min cs (map2fun vl);
  } in Mapping.tabulate
         (List.remdups (concatMap Abstract_Linear_Poly.vars_list (map poly cs)))
         (\ var -> QDelta.val (map2fun vl var) delta);

uBI_upd ::
  forall a b.
    (Orderings.Linorder b) => Direction a b ->
                                (Mapping.Mapping Arith.Nat (a, b) ->
                                  Mapping.Mapping Arith.Nat (a, b)) ->
                                  State a b -> State a b;
uBI_upd (Direction x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = x8;

update_B_I ::
  forall a b c d e.
    (Orderings.Linorder a) => ((Mapping.Mapping a (b, c) ->
                                 Mapping.Mapping a (b, c)) ->
                                d -> e) ->
                                b -> a -> c -> d -> e;
update_B_I field_update i x c s = field_update (Mapping.update x (i, c)) s;

lt :: forall a b. (Orderings.Linorder b) => Direction a b -> b -> b -> Bool;
lt (Direction x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = x1;

ub :: forall a b.
        (Orderings.Linorder b) => Direction a b ->
                                    State a b -> Arith.Nat -> Maybe b;
ub (Direction x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = x5;

li :: forall a b.
        (Orderings.Linorder b) => Direction a b -> State a b -> Arith.Nat -> a;
li (Direction x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = x6;

lb :: forall a b.
        (Orderings.Linorder b) => Direction a b ->
                                    State a b -> Arith.Nat -> Maybe b;
lb (Direction x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = x4;

v_update :: forall a b. Mapping.Mapping Arith.Nat a -> State b a -> State b a;
v_update v (State t bil biu v_old u uc) = State t bil biu v u uc;

v :: forall a b. State a b -> Mapping.Mapping Arith.Nat b;
v (State x1 x2 x3 x4 x5 x6) = x4;

t :: forall a b. State a b -> [(Arith.Nat, Abstract_Linear_Poly.Linear_poly)];
t (State x1 x2 x3 x4 x5 x6) = x1;

rhs_eq_val ::
  forall a.
    (Arith.Minus a, Arith.Plus a, Arith.Zero a,
      Simplex_Algebra.ScaleRat a) => Mapping.Mapping Arith.Nat a ->
                                       Arith.Nat ->
 a -> (Arith.Nat, Abstract_Linear_Poly.Linear_poly) -> a;
rhs_eq_val v x_i c e =
  let {
    x_j = lhs e;
    a_i_j = Abstract_Linear_Poly.coeff (rhs e) x_i;
  } in Arith.plus (map2fun v x_j)
         (Simplex_Algebra.scaleRat a_i_j (Arith.minus c (map2fun v x_i)));

update_code ::
  forall a b.
    (Simplex_Algebra.Lrv a) => Arith.Nat -> a -> State b a -> State b a;
update_code x c s =
  v_update
    (Mapping.update x c
      (List.foldl (\ va e -> Mapping.update (lhs e) (rhs_eq_val (v s) x c e) va)
        (v s) (t s)))
    s;

set_unsat :: forall a b. (Eq a) => [a] -> State a b -> State a b;
set_unsat i (State t bil biu v u uc) =
  State t bil biu v True (Just (List.remdups i));

assert_bound_codea ::
  forall a b.
    (Eq a, Eq b,
      Simplex_Algebra.Lrv b) => Direction a b ->
                                  a -> Arith.Nat -> b -> State a b -> State a b;
assert_bound_codea dir i x c s =
  (if geub (lt dir) c (ub dir s x) then s
    else let {
           sa = update_B_I (uBI_upd dir) i x c s;
         } in (if ltlb (lt dir) c (lb dir s x) then set_unsat [i, li dir s x] sa
                else (if not (Set.member x (lvars (t sa))) &&
                           lt dir c (map2fun (v s) x)
                       then update_code x c sa else sa)));

b_i_u_update ::
  forall a b.
    (Mapping.Mapping Arith.Nat (a, b) -> Mapping.Mapping Arith.Nat (a, b)) ->
      State a b -> State a b;
b_i_u_update up (State t bil biu v u uc) = State t bil (up biu) v u uc;

positive :: forall a b. (Orderings.Linorder b) => Direction a b;
positive =
  Direction Orderings.less b_i_l b_i_u boundsl boundsu indexl indexu
    b_i_u_update Leq Geq Rat.less_eq_rat;

b_i_l_update ::
  forall a b.
    (Mapping.Mapping Arith.Nat (a, b) -> Mapping.Mapping Arith.Nat (a, b)) ->
      State a b -> State a b;
b_i_l_update up (State t bil biu v u uc) = State t (up bil) biu v u uc;

negative :: forall a b. (Orderings.Linorder b) => Direction a b;
negative =
  Direction (\ x y -> Orderings.less y x) b_i_u b_i_l boundsu boundsl indexu
    indexl b_i_l_update Geq Leq (\ x y -> Rat.less_eq_rat y x);

assert_bound_code ::
  forall a b.
    (Eq a, Eq b,
      Simplex_Algebra.Lrv b) => (a, Atom b) -> State a b -> State a b;
assert_bound_code (i, Leq x c) s = assert_bound_codea positive i x c s;
assert_bound_code (i, Geq x c) s = assert_bound_codea negative i x c s;

u :: forall a b. State a b -> Bool;
u (State x1 x2 x3 x4 x5 x6) = x5;

assert_bound_loop_code ::
  forall a b.
    (Eq a, Eq b,
      Simplex_Algebra.Lrv b) => [(a, Atom b)] -> State a b -> State a b;
assert_bound_loop_code ats s =
  List.foldl (\ sa a -> (if u sa then sa else assert_bound_code a sa)) s ats;

init_state ::
  forall a b.
    (Arith.Zero b) => [(Arith.Nat, Abstract_Linear_Poly.Linear_poly)] ->
                        State a b;
init_state t =
  State t Mapping.empty Mapping.empty
    (Mapping.tabulate
      (List.remdups
        (map lhs t ++ concatMap (Abstract_Linear_Poly.vars_list . rhs) t))
      (\ _ -> Arith.zero))
    False Nothing;

le_ubound :: forall a. (Eq a, Orderings.Linorder a) => a -> Maybe a -> Bool;
le_ubound c b = leub Orderings.less c b;

ge_lbound :: forall a. (Eq a, Orderings.Linorder a) => a -> Maybe a -> Bool;
ge_lbound c b = gelb Orderings.less c b;

in_bounds ::
  forall a b.
    (Eq b,
      Orderings.Linorder b) => a -> (a -> b) ->
                                      (a -> Maybe b, a -> Maybe b) -> Bool;
in_bounds x v (lb, ub) = ge_lbound (v x) (lb x) && le_ubound (v x) (ub x);

min_lvar_not_in_bounds ::
  forall a b.
    (Arith.Zero b, Eq b, Orderings.Linorder b) => State a b -> Maybe Arith.Nat;
min_lvar_not_in_bounds s =
  Simplex_Auxiliary.min_satisfying
    (\ x -> not (in_bounds x (map2fun (v s)) (boundsl s, boundsu s)))
    (map lhs (t s));

subst_var ::
  Arith.Nat ->
    Abstract_Linear_Poly.Linear_poly ->
      Abstract_Linear_Poly.Linear_poly -> Abstract_Linear_Poly.Linear_poly;
subst_var v lpa lp =
  Abstract_Linear_Poly.minus_linear_poly
    (Abstract_Linear_Poly.plus_linear_poly lp
      (Abstract_Linear_Poly.scaleRat_linear_poly
        (Abstract_Linear_Poly.coeff lp v) lpa))
    (Abstract_Linear_Poly.scaleRat_linear_poly (Abstract_Linear_Poly.coeff lp v)
      (Abstract_Linear_Poly.var v));

subst_var_eq_code ::
  Arith.Nat ->
    Abstract_Linear_Poly.Linear_poly ->
      (Arith.Nat, Abstract_Linear_Poly.Linear_poly) ->
        (Arith.Nat, Abstract_Linear_Poly.Linear_poly);
subst_var_eq_code v lp eq = (lhs eq, subst_var v lp (rhs eq));

eq_idx_for_lvar_aux ::
  [(Arith.Nat, Abstract_Linear_Poly.Linear_poly)] ->
    Arith.Nat -> Arith.Nat -> Arith.Nat;
eq_idx_for_lvar_aux [] x i = i;
eq_idx_for_lvar_aux (eq : t) x i =
  (if Arith.equal_nat (lhs eq) x then i
    else eq_idx_for_lvar_aux t x (Arith.plus_nat i Arith.one_nat));

eq_idx_for_lvar ::
  [(Arith.Nat, Abstract_Linear_Poly.Linear_poly)] -> Arith.Nat -> Arith.Nat;
eq_idx_for_lvar t x = eq_idx_for_lvar_aux t x Arith.zero_nat;

eq_for_lvar_code ::
  [(Arith.Nat, Abstract_Linear_Poly.Linear_poly)] ->
    Arith.Nat -> (Arith.Nat, Abstract_Linear_Poly.Linear_poly);
eq_for_lvar_code t v = List.nth t (eq_idx_for_lvar t v);

pivot_eq ::
  (Arith.Nat, Abstract_Linear_Poly.Linear_poly) ->
    Arith.Nat -> (Arith.Nat, Abstract_Linear_Poly.Linear_poly);
pivot_eq e y =
  let {
    cy = Abstract_Linear_Poly.coeff (rhs e) y;
  } in (y, Abstract_Linear_Poly.plus_linear_poly
             (Abstract_Linear_Poly.scaleRat_linear_poly
               (Rat.divide_rat (Rat.uminus_rat Rat.one_rat) cy)
               (Abstract_Linear_Poly.minus_linear_poly (rhs e)
                 (Abstract_Linear_Poly.scaleRat_linear_poly cy
                   (Abstract_Linear_Poly.var y))))
             (Abstract_Linear_Poly.scaleRat_linear_poly
               (Rat.divide_rat Rat.one_rat cy)
               (Abstract_Linear_Poly.var (lhs e))));

pivot_tableau_code ::
  Arith.Nat ->
    Arith.Nat ->
      [(Arith.Nat, Abstract_Linear_Poly.Linear_poly)] ->
        [(Arith.Nat, Abstract_Linear_Poly.Linear_poly)];
pivot_tableau_code x_i x_j t =
  let {
    eq = eq_for_lvar_code t x_i;
    eqa = pivot_eq eq x_j;
  } in map (\ e ->
             (if Arith.equal_nat (lhs e) (lhs eq) then eqa
               else subst_var_eq_code x_j (rhs eqa) e))
         t;

t_update ::
  forall a b.
    [(Arith.Nat, Abstract_Linear_Poly.Linear_poly)] -> State a b -> State a b;
t_update t (State t_old bil biu v u uc) = State t bil biu v u uc;

pivot_code ::
  forall a b.
    (Simplex_Algebra.Lrv b) => Arith.Nat -> Arith.Nat -> State a b -> State a b;
pivot_code x_i x_j s = t_update (pivot_tableau_code x_i x_j (t s)) s;

pivot_and_update_code ::
  forall a b.
    (Simplex_Algebra.Lrv a) => Arith.Nat ->
                                 Arith.Nat -> a -> State b a -> State b a;
pivot_and_update_code x_i x_j c s = update_code x_i c (pivot_code x_i x_j s);

ui :: forall a b.
        (Orderings.Linorder b) => Direction a b -> State a b -> Arith.Nat -> a;
ui (Direction x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = x7;

unsat_indices ::
  forall a b.
    (Eq a,
      Orderings.Linorder b) => Direction a b ->
                                 State a b ->
                                   [Arith.Nat] ->
                                     (Arith.Nat,
                                       Abstract_Linear_Poly.Linear_poly) ->
                                       [a];
unsat_indices dir s vs eq =
  let {
    r = rhs eq;
    lia = li dir s;
    uia = ui dir s;
  } in List.remdups
         (lia (lhs eq) :
           map (\ x ->
                 (if Rat.less_rat (Abstract_Linear_Poly.coeff r x) Rat.zero_rat
                   then lia x else uia x))
             vs);

min_rvar_incdec_eq ::
  forall a b.
    (Eq a,
      Simplex_Algebra.Lrv b) => Direction a b ->
                                  State a b ->
                                    (Arith.Nat,
                                      Abstract_Linear_Poly.Linear_poly) ->
                                      Sum_Type.Sum [a] Arith.Nat;
min_rvar_incdec_eq dir s eq =
  let {
    rvars = Abstract_Linear_Poly.vars_list (rhs eq);
  } in (case Simplex_Auxiliary.min_satisfying
               (\ x ->
                 Rat.less_rat Rat.zero_rat
                   (Abstract_Linear_Poly.coeff (rhs eq) x) &&
                   ltub (lt dir) (map2fun (v s) x) (ub dir s x) ||
                   Rat.less_rat (Abstract_Linear_Poly.coeff (rhs eq) x)
                     Rat.zero_rat &&
                     gtlb (lt dir) (map2fun (v s) x) (lb dir s x))
               rvars
         of {
         Nothing -> Sum_Type.Inl (unsat_indices dir s rvars eq);
         Just a -> Sum_Type.Inr a;
       });

check_codea ::
  forall a b.
    (Eq a,
      Simplex_Algebra.Lrv b) => Direction a b ->
                                  Arith.Nat -> State a b -> State a b;
check_codea dir x_i s =
  let {
    l_i = Option.the (lb dir s x_i);
  } in (case min_rvar_incdec_eq dir s (eq_for_lvar_code (t s) x_i) of {
         Sum_Type.Inl i -> set_unsat i s;
         Sum_Type.Inr x_j -> pivot_and_update_code x_i x_j l_i s;
       });

lt_lbound :: forall a. (Orderings.Linorder a) => a -> Maybe a -> Bool;
lt_lbound c b = ltlb Orderings.less c b;

check_code ::
  forall a b. (Eq a, Eq b, Simplex_Algebra.Lrv b) => State a b -> State a b;
check_code s =
  (if u s then s
    else (case min_lvar_not_in_bounds s of {
           Nothing -> s;
           Just x_i ->
             let {
               dir = (if lt_lbound (map2fun (v s) x_i) (boundsl s x_i)
                       then positive else negative);
             } in check_code (check_codea dir x_i s);
         }));

assert_all_state_code ::
  forall a b.
    (Eq a, Eq b,
      Simplex_Algebra.Lrv b) => [(Arith.Nat,
                                   Abstract_Linear_Poly.Linear_poly)] ->
                                  [(a, Atom b)] -> State a b;
assert_all_state_code t ats =
  check_code (assert_bound_loop_code ats (init_state t));

u_c :: forall a b. State a b -> Maybe [a];
u_c (State x1 x2 x3 x4 x5 x6) = x6;

assert_all_code ::
  forall a b.
    (Eq a, Eq b,
      Simplex_Algebra.Lrv b) => [(Arith.Nat,
                                   Abstract_Linear_Poly.Linear_poly)] ->
                                  [(a, Atom b)] ->
                                    Sum_Type.Sum [a]
                                      (Mapping.Mapping Arith.Nat b);
assert_all_code t asa =
  let {
    s = assert_all_state_code t asa;
  } in (if u s then Sum_Type.Inl (Option.the (u_c s)) else Sum_Type.Inr (v s));

normalize_ns_constraint ::
  forall a. (Simplex_Algebra.Lrv a) => Ns_constraint a -> Ns_constraint a;
normalize_ns_constraint (LEQ_ns l r) =
  let {
    v = Abstract_Linear_Poly.max_var l;
    c = Abstract_Linear_Poly.coeff l v;
  } in (if Rat.equal_rat c Rat.zero_rat then LEQ_ns l r
         else let {
                ic = Rat.inverse_rat c;
              } in (if Rat.less_rat c Rat.zero_rat
                     then GEQ_ns
                            (Abstract_Linear_Poly.scaleRat_linear_poly ic l)
                            (Simplex_Algebra.scaleRat ic r)
                     else LEQ_ns
                            (Abstract_Linear_Poly.scaleRat_linear_poly ic l)
                            (Simplex_Algebra.scaleRat ic r)));
normalize_ns_constraint (GEQ_ns l r) =
  let {
    v = Abstract_Linear_Poly.max_var l;
    c = Abstract_Linear_Poly.coeff l v;
  } in (if Rat.equal_rat c Rat.zero_rat then GEQ_ns l r
         else let {
                ic = Rat.inverse_rat c;
              } in (if Rat.less_rat c Rat.zero_rat
                     then LEQ_ns
                            (Abstract_Linear_Poly.scaleRat_linear_poly ic l)
                            (Simplex_Algebra.scaleRat ic r)
                     else GEQ_ns
                            (Abstract_Linear_Poly.scaleRat_linear_poly ic l)
                            (Simplex_Algebra.scaleRat ic r)));

pivot_tableau_eq ::
  [(Arith.Nat, Abstract_Linear_Poly.Linear_poly)] ->
    (Arith.Nat, Abstract_Linear_Poly.Linear_poly) ->
      [(Arith.Nat, Abstract_Linear_Poly.Linear_poly)] ->
        Arith.Nat ->
          ([(Arith.Nat, Abstract_Linear_Poly.Linear_poly)],
            ((Arith.Nat, Abstract_Linear_Poly.Linear_poly),
              [(Arith.Nat, Abstract_Linear_Poly.Linear_poly)]));
pivot_tableau_eq t1 eq t2 x = let {
                                eqa = pivot_eq eq x;
                                m = map (subst_var_eq_code x (rhs eqa));
                              } in (m t1, (eqa, m t2));

preprocess_opt ::
  forall a.
    (Simplex_Algebra.Lrv a) => Set.Set Arith.Nat ->
                                 [(Arith.Nat,
                                    Abstract_Linear_Poly.Linear_poly)] ->
                                   [(Arith.Nat,
                                      Abstract_Linear_Poly.Linear_poly)] ->
                                     ([(Arith.Nat,
 Abstract_Linear_Poly.Linear_poly)],
                                       Mapping.Mapping Arith.Nat a ->
 Mapping.Mapping Arith.Nat a);
preprocess_opt x t1 [] = (t1, id);
preprocess_opt xa t1 ((x, p) : t2) =
  (if not (Set.member x xa)
    then (case preprocess_opt xa t1 t2 of {
           (t, tv) ->
             (t, (\ v ->
                   Mapping.update x (Abstract_Linear_Poly.valuate p (map2fun v))
                     v) .
                   tv);
         })
    else (case List.find (\ xb -> not (Set.member xb xa))
                 (Abstract_Linear_Poly.vars_list p)
           of {
           Nothing -> preprocess_opt xa ((x, p) : t1) t2;
           Just y ->
             (case pivot_tableau_eq t1 (x, p) t2 y of {
               (tt1, ((z, q), tt2)) ->
                 (case preprocess_opt xa tt1 tt2 of {
                   (t, tv) ->
                     (t, (\ v ->
                           Mapping.update z
                             (Abstract_Linear_Poly.valuate q (map2fun v)) v) .
                           tv);
                 });
             });
         }));

atom_var :: forall a. Atom a -> Arith.Nat;
atom_var (Leq var a) = var;
atom_var (Geq var a) = var;

preprocess_part_2 ::
  forall a b c.
    (Simplex_Algebra.Lrv c) => [(a, Atom b)] ->
                                 [(Arith.Nat,
                                    Abstract_Linear_Poly.Linear_poly)] ->
                                   ([(Arith.Nat,
                                       Abstract_Linear_Poly.Linear_poly)],
                                     Mapping.Mapping Arith.Nat c ->
                                       Mapping.Mapping Arith.Nat c);
preprocess_part_2 asa t =
  preprocess_opt (Set.image atom_var (Set.image snd (Set.Set asa))) [] t;

start_fresh_variable ::
  forall a. [(a, Ns_constraint QDelta.QDelta)] -> Arith.Nat;
start_fresh_variable [] = Arith.zero_nat;
start_fresh_variable ((i, h) : t) =
  Orderings.max
    (Arith.plus_nat (Abstract_Linear_Poly.max_var (poly h)) Arith.one_nat)
    (start_fresh_variable t);

unsatIndices :: forall a. Istate a -> [a];
unsatIndices (IState x1 x2 x3 x4 x5) = x5;

tableau ::
  forall a. Istate a -> [(Arith.Nat, Abstract_Linear_Poly.Linear_poly)];
tableau (IState x1 x2 x3 x4 x5) = x2;

atoms :: forall a. Istate a -> [(a, Atom QDelta.QDelta)];
atoms (IState x1 x2 x3 x4 x5) = x3;

qdelta_constraint_to_atom ::
  Ns_constraint QDelta.QDelta -> Arith.Nat -> Atom QDelta.QDelta;
qdelta_constraint_to_atom (LEQ_ns l r) v = Leq v r;
qdelta_constraint_to_atom (GEQ_ns l r) v = Geq v r;

firstFreshVariable :: forall a. Istate a -> Arith.Nat;
firstFreshVariable (IState x1 x2 x3 x4 x5) = x1;

poly_Mapping ::
  forall a. Istate a -> Abstract_Linear_Poly.Linear_poly -> Maybe Arith.Nat;
poly_Mapping (IState x1 x2 x3 x4 x5) = x4;

linear_poly_to_eq ::
  Abstract_Linear_Poly.Linear_poly ->
    Arith.Nat -> (Arith.Nat, Abstract_Linear_Poly.Linear_poly);
linear_poly_to_eq p v = (v, p);

zero_satisfies :: forall a. (Simplex_Algebra.Lrv a) => Ns_constraint a -> Bool;
zero_satisfies (LEQ_ns l r) = Orderings.less_eq Arith.zero r;
zero_satisfies (GEQ_ns l r) = Orderings.less_eq r Arith.zero;

monom_to_atom :: Ns_constraint QDelta.QDelta -> Atom QDelta.QDelta;
monom_to_atom (LEQ_ns l r) =
  (if Rat.less_rat (Abstract_Linear_Poly.monom_coeff l) Rat.zero_rat
    then Geq (Abstract_Linear_Poly.monom_var l)
           (QDelta.scaleRat_QDelta
             (Rat.inverse_rat (Abstract_Linear_Poly.monom_coeff l)) r)
    else Leq (Abstract_Linear_Poly.monom_var l)
           (QDelta.scaleRat_QDelta
             (Rat.inverse_rat (Abstract_Linear_Poly.monom_coeff l)) r));
monom_to_atom (GEQ_ns l r) =
  (if Rat.less_rat (Abstract_Linear_Poly.monom_coeff l) Rat.zero_rat
    then Leq (Abstract_Linear_Poly.monom_var l)
           (QDelta.scaleRat_QDelta
             (Rat.inverse_rat (Abstract_Linear_Poly.monom_coeff l)) r)
    else Geq (Abstract_Linear_Poly.monom_var l)
           (QDelta.scaleRat_QDelta
             (Rat.inverse_rat (Abstract_Linear_Poly.monom_coeff l)) r));

preprocessa ::
  forall a. [(a, Ns_constraint QDelta.QDelta)] -> Arith.Nat -> Istate a;
preprocessa ((i, h) : t) v =
  let {
    s = preprocessa t v;
    p = poly h;
    is_monom_h = Abstract_Linear_Poly.is_monom p;
    va = firstFreshVariable s;
    ta = tableau s;
    a = atoms s;
    m = poly_Mapping s;
    u = unsatIndices s;
  } in (if is_monom_h then IState va ta ((i, monom_to_atom h) : a) m u
         else (if Linear_Poly_Maps.equal_linear_poly p
                    Abstract_Linear_Poly.zero_linear_poly
                then (if zero_satisfies h then s else IState va ta a m (i : u))
                else (case m p of {
                       Nothing ->
                         IState (Arith.plus_nat va Arith.one_nat)
                           (linear_poly_to_eq p va : ta)
                           ((i, qdelta_constraint_to_atom h va) : a)
                           (Fun.fun_upd m p (Just va)) u;
                       Just vaa ->
                         IState va ta ((i, qdelta_constraint_to_atom h vaa) : a)
                           m u;
                     })));
preprocessa [] v = IState v [] [] (\ _ -> Nothing) [];

preprocess_part_1 ::
  forall a.
    [(a, Ns_constraint QDelta.QDelta)] ->
      ([(Arith.Nat, Abstract_Linear_Poly.Linear_poly)],
        ([(a, Atom QDelta.QDelta)], [a]));
preprocess_part_1 l = let {
                        start = start_fresh_variable l;
                        is = preprocessa l start;
                      } in (tableau is, (atoms is, unsatIndices is));

preprocess ::
  forall a.
    [(a, Ns_constraint QDelta.QDelta)] ->
      ([(Arith.Nat, Abstract_Linear_Poly.Linear_poly)],
        ([(a, Atom QDelta.QDelta)],
          (Mapping.Mapping Arith.Nat QDelta.QDelta ->
             Mapping.Mapping Arith.Nat QDelta.QDelta,
            [a])));
preprocess l =
  (case preprocess_part_1
          (map (Product_Type.map_prod id normalize_ns_constraint) l)
    of {
    (t, (asa, ui)) -> (case preprocess_part_2 asa t of {
                        (ta, tv) -> (ta, (asa, (tv, ui)));
                      });
  });

solve_exec_ns_code ::
  forall a.
    (Eq a) => [(a, Ns_constraint QDelta.QDelta)] ->
                Sum_Type.Sum [a] (Mapping.Mapping Arith.Nat QDelta.QDelta);
solve_exec_ns_code s =
  (case preprocess s of {
    (t, (asa, (trans_v, []))) -> (case assert_all_code t asa of {
                                   Sum_Type.Inl a -> Sum_Type.Inl a;
                                   Sum_Type.Inr v -> Sum_Type.Inr (trans_v v);
                                 });
    (_, (_, (_, i : _))) -> Sum_Type.Inl [i];
  });

solve_exec_code ::
  forall a.
    (Eq a) => [(a, Constraint)] ->
                Sum_Type.Sum [a] (Mapping.Mapping Arith.Nat Rat.Rat);
solve_exec_code cs =
  let {
    csa = to_ns cs;
  } in (case solve_exec_ns_code csa of {
         Sum_Type.Inl a -> Sum_Type.Inl a;
         Sum_Type.Inr v -> Sum_Type.Inr (from_ns v (map snd csa));
       });

simplex_index ::
  forall a.
    (Eq a) => [(a, Constraint)] ->
                Sum_Type.Sum [a] (Mapping.Mapping Arith.Nat Rat.Rat);
simplex_index = solve_exec_code;

simplex ::
  [Constraint] -> Sum_Type.Sum [Arith.Nat] (Mapping.Mapping Arith.Nat Rat.Rat);
simplex cs =
  simplex_index (zip (List.upt Arith.zero_nat (List.size_list cs)) cs);

}
