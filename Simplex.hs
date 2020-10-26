{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module Simplex where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;

newtype Int = Int_of_integer Integer;

integer_of_int :: Int -> Integer;
integer_of_int (Int_of_integer k) = k;

equal_int :: Int -> Int -> Bool;
equal_int k l = integer_of_int k == integer_of_int l;

instance Eq Int where {
  a == b = equal_int a b;
};

newtype Nat = Nat Integer;

integer_of_nat :: Nat -> Integer;
integer_of_nat (Nat x) = x;

equal_nat :: Nat -> Nat -> Bool;
equal_nat m n = integer_of_nat m == integer_of_nat n;

instance Eq Nat where {
  a == b = equal_nat a b;
};

less_eq_nat :: Nat -> Nat -> Bool;
less_eq_nat m n = integer_of_nat m <= integer_of_nat n;

class Ord a where {
  less_eq :: a -> a -> Bool;
  less :: a -> a -> Bool;
};

less_nat :: Nat -> Nat -> Bool;
less_nat m n = integer_of_nat m < integer_of_nat n;

instance Ord Nat where {
  less_eq = less_eq_nat;
  less = less_nat;
};

class (Ord a) => Preorder a where {
};

class (Preorder a) => Order a where {
};

instance Preorder Nat where {
};

instance Order Nat where {
};

class (Order a) => Linorder a where {
};

instance Linorder Nat where {
};

newtype Rat = Frct (Int, Int);

quotient_of :: Rat -> (Int, Int);
quotient_of (Frct x) = x;

equal_rat :: Rat -> Rat -> Bool;
equal_rat a b = quotient_of a == quotient_of b;

instance Eq Rat where {
  a == b = equal_rat a b;
};

times_int :: Int -> Int -> Int;
times_int k l = Int_of_integer (integer_of_int k * integer_of_int l);

less_eq_int :: Int -> Int -> Bool;
less_eq_int k l = integer_of_int k <= integer_of_int l;

less_eq_rat :: Rat -> Rat -> Bool;
less_eq_rat p q =
  let {
    a = quotient_of p;
  } in (case a of {
         (aa, c) ->
           let {
             b = quotient_of q;
           } in (case b of {
                  (ba, d) -> less_eq_int (times_int aa d) (times_int c ba);
                });
       });

less_int :: Int -> Int -> Bool;
less_int k l = integer_of_int k < integer_of_int l;

less_rat :: Rat -> Rat -> Bool;
less_rat p q =
  let {
    a = quotient_of p;
  } in (case a of {
         (aa, c) ->
           let {
             b = quotient_of q;
           } in (case b of {
                  (ba, d) -> less_int (times_int aa d) (times_int c ba);
                });
       });

instance Ord Rat where {
  less_eq = less_eq_rat;
  less = less_rat;
};

data QDelta = QDelta Rat Rat;

equal_QDelta :: QDelta -> QDelta -> Bool;
equal_QDelta (QDelta x1 x2) (QDelta y1 y2) = equal_rat x1 y1 && equal_rat x2 y2;

instance Eq QDelta where {
  a == b = equal_QDelta a b;
};

zero_int :: Int;
zero_int = Int_of_integer (0 :: Integer);

data Num = One | Bit0 Num | Bit1 Num;

one_int :: Int;
one_int = Int_of_integer (1 :: Integer);

zero_rat :: Rat;
zero_rat = Frct (zero_int, one_int);

one_rat :: Rat;
one_rat = Frct (one_int, one_int);

one_QDelta :: QDelta;
one_QDelta = QDelta one_rat zero_rat;

class One a where {
  one :: a;
};

instance One QDelta where {
  one = one_QDelta;
};

plus_int :: Int -> Int -> Int;
plus_int k l = Int_of_integer (integer_of_int k + integer_of_int l);

apsnd :: forall a b c. (a -> b) -> (c, a) -> (c, b);
apsnd f (x, y) = (x, f y);

divmod_integer :: Integer -> Integer -> (Integer, Integer);
divmod_integer k l =
  (if k == (0 :: Integer) then ((0 :: Integer), (0 :: Integer))
    else (if (0 :: Integer) < l
           then (if (0 :: Integer) < k then divMod (abs k) (abs l)
                  else (case divMod (abs k) (abs l) of {
                         (r, s) ->
                           (if s == (0 :: Integer)
                             then (negate r, (0 :: Integer))
                             else (negate r - (1 :: Integer), l - s));
                       }))
           else (if l == (0 :: Integer) then ((0 :: Integer), k)
                  else apsnd negate
                         (if k < (0 :: Integer) then divMod (abs k) (abs l)
                           else (case divMod (abs k) (abs l) of {
                                  (r, s) ->
                                    (if s == (0 :: Integer)
                                      then (negate r, (0 :: Integer))
                                      else (negate r - (1 :: Integer),
     negate l - s));
                                })))));

divide_integer :: Integer -> Integer -> Integer;
divide_integer k l = fst (divmod_integer k l);

divide_int :: Int -> Int -> Int;
divide_int k l =
  Int_of_integer (divide_integer (integer_of_int k) (integer_of_int l));

uminus_int :: Int -> Int;
uminus_int k = Int_of_integer (negate (integer_of_int k));

gcd_int :: Int -> Int -> Int;
gcd_int (Int_of_integer x) (Int_of_integer y) =
  Int_of_integer (Prelude.gcd x y);

normalize :: (Int, Int) -> (Int, Int);
normalize p =
  (if less_int zero_int (snd p)
    then let {
           a = gcd_int (fst p) (snd p);
         } in (divide_int (fst p) a, divide_int (snd p) a)
    else (if equal_int (snd p) zero_int then (zero_int, one_int)
           else let {
                  a = uminus_int (gcd_int (fst p) (snd p));
                } in (divide_int (fst p) a, divide_int (snd p) a)));

plus_rat :: Rat -> Rat -> Rat;
plus_rat p q =
  Frct (let {
          a = quotient_of p;
        } in (case a of {
               (aa, c) ->
                 let {
                   b = quotient_of q;
                 } in (case b of {
                        (ba, d) ->
                          normalize
                            (plus_int (times_int aa d) (times_int ba c),
                              times_int c d);
                      });
             }));

qdsnd :: QDelta -> Rat;
qdsnd (QDelta a b) = b;

qdfst :: QDelta -> Rat;
qdfst (QDelta a b) = a;

plus_QDelta :: QDelta -> QDelta -> QDelta;
plus_QDelta qd1 qd2 =
  QDelta (plus_rat (qdfst qd1) (qdfst qd2)) (plus_rat (qdsnd qd1) (qdsnd qd2));

class Plus a where {
  plus :: a -> a -> a;
};

instance Plus QDelta where {
  plus = plus_QDelta;
};

zero_QDelta :: QDelta;
zero_QDelta = QDelta zero_rat zero_rat;

class Zero a where {
  zeroa :: a;
};

instance Zero QDelta where {
  zeroa = zero_QDelta;
};

minus_int :: Int -> Int -> Int;
minus_int k l = Int_of_integer (integer_of_int k - integer_of_int l);

minus_rat :: Rat -> Rat -> Rat;
minus_rat p q =
  Frct (let {
          a = quotient_of p;
        } in (case a of {
               (aa, c) ->
                 let {
                   b = quotient_of q;
                 } in (case b of {
                        (ba, d) ->
                          normalize
                            (minus_int (times_int aa d) (times_int ba c),
                              times_int c d);
                      });
             }));

minus_QDelta :: QDelta -> QDelta -> QDelta;
minus_QDelta qd1 qd2 =
  QDelta (minus_rat (qdfst qd1) (qdfst qd2))
    (minus_rat (qdsnd qd1) (qdsnd qd2));

class Minus a where {
  minus :: a -> a -> a;
};

instance Minus QDelta where {
  minus = minus_QDelta;
};

uminus_rat :: Rat -> Rat;
uminus_rat p = Frct (let {
                       a = quotient_of p;
                     } in (case a of {
                            (aa, b) -> (uminus_int aa, b);
                          }));

uminus_QDelta :: QDelta -> QDelta;
uminus_QDelta qd = QDelta (uminus_rat (qdfst qd)) (uminus_rat (qdsnd qd));

class Uminus a where {
  uminus :: a -> a;
};

instance Uminus QDelta where {
  uminus = uminus_QDelta;
};

less_eq_QDelta :: QDelta -> QDelta -> Bool;
less_eq_QDelta qd1 qd2 =
  less_rat (qdfst qd1) (qdfst qd2) ||
    equal_rat (qdfst qd1) (qdfst qd2) && less_eq_rat (qdsnd qd1) (qdsnd qd2);

less_QDelta :: QDelta -> QDelta -> Bool;
less_QDelta qd1 qd2 =
  less_rat (qdfst qd1) (qdfst qd2) ||
    equal_rat (qdfst qd1) (qdfst qd2) && less_rat (qdsnd qd1) (qdsnd qd2);

instance Ord QDelta where {
  less_eq = less_eq_QDelta;
  less = less_QDelta;
};

instance Preorder QDelta where {
};

instance Order QDelta where {
};

class (Plus a) => Semigroup_add a where {
};

class (Semigroup_add a) => Cancel_semigroup_add a where {
};

class (Semigroup_add a, Zero a) => Monoid_add a where {
};

class (Cancel_semigroup_add a, Minus a, Monoid_add a,
        Uminus a) => Group_add a where {
};

instance Semigroup_add QDelta where {
};

instance Cancel_semigroup_add QDelta where {
};

instance Monoid_add QDelta where {
};

instance Group_add QDelta where {
};

instance Linorder QDelta where {
};

class (Semigroup_add a) => Ab_semigroup_add a where {
};

class (Ab_semigroup_add a, Cancel_semigroup_add a,
        Minus a) => Cancel_ab_semigroup_add a where {
};

class (Ab_semigroup_add a, Monoid_add a) => Comm_monoid_add a where {
};

class (Cancel_ab_semigroup_add a,
        Comm_monoid_add a) => Cancel_comm_monoid_add a where {
};

class (Cancel_comm_monoid_add a, Group_add a) => Ab_group_add a where {
};

instance Ab_semigroup_add QDelta where {
};

instance Cancel_ab_semigroup_add QDelta where {
};

instance Comm_monoid_add QDelta where {
};

instance Cancel_comm_monoid_add QDelta where {
};

instance Ab_group_add QDelta where {
};

times_rat :: Rat -> Rat -> Rat;
times_rat p q =
  Frct (let {
          a = quotient_of p;
        } in (case a of {
               (aa, c) ->
                 let {
                   b = quotient_of q;
                 } in (case b of {
                        (ba, d) -> normalize (times_int aa ba, times_int c d);
                      });
             }));

scaleRat_QDelta :: Rat -> QDelta -> QDelta;
scaleRat_QDelta r qd = QDelta (times_rat r (qdfst qd)) (times_rat r (qdsnd qd));

class ScaleRat a where {
  scaleRat :: Rat -> a -> a;
};

class (Ab_group_add a, ScaleRat a) => Rational_vector a where {
};

class (Order a, Rational_vector a) => Ordered_rational_vector a where {
};

class (Ab_semigroup_add a, Order a) => Ordered_ab_semigroup_add a where {
};

class (Ordered_ab_semigroup_add a, Linorder a,
        Ordered_rational_vector a) => Linordered_rational_vector a where {
};

class (One a, Linordered_rational_vector a) => Lrv a where {
};

instance ScaleRat QDelta where {
  scaleRat = scaleRat_QDelta;
};

instance Rational_vector QDelta where {
};

instance Ordered_rational_vector QDelta where {
};

instance Ordered_ab_semigroup_add QDelta where {
};

instance Linordered_rational_vector QDelta where {
};

instance Lrv QDelta where {
};

instance Ord Integer where {
  less_eq = (\ a b -> a <= b);
  less = (\ a b -> a < b);
};

rel_option :: forall a b. (a -> b -> Bool) -> Maybe a -> Maybe b -> Bool;
rel_option r Nothing (Just y2) = False;
rel_option r (Just y2) Nothing = False;
rel_option r Nothing Nothing = True;
rel_option r (Just x2) (Just y2) = r x2 y2;

newtype Fmap a b = Fmap_of_list [(a, b)];

map_of :: forall a b. (Eq a) => [(a, b)] -> a -> Maybe b;
map_of ((l, v) : ps) k = (if l == k then Just v else map_of ps k);
map_of [] k = Nothing;

fmlookup :: forall a b. (Eq a) => Fmap a b -> a -> Maybe b;
fmlookup (Fmap_of_list m) = map_of m;

data Set a = Set [a] | Coset [a];

newtype Fset a = Abs_fset (Set a);

fset_of_list :: forall a. [a] -> Fset a;
fset_of_list xa = Abs_fset (Set xa);

fset :: forall a. Fset a -> Set a;
fset (Abs_fset x) = x;

image :: forall a b. (a -> b) -> Set a -> Set b;
image f (Set xs) = Set (map f xs);

fimage :: forall b a. (b -> a) -> Fset b -> Fset a;
fimage xb xc = Abs_fset (image xb (fset xc));

fmdom :: forall a b. Fmap a b -> Fset a;
fmdom (Fmap_of_list m) = fimage fst (fset_of_list m);

ball :: forall a. Set a -> (a -> Bool) -> Bool;
ball (Set xs) p = all p xs;

fBall :: forall a. Fset a -> (a -> Bool) -> Bool;
fBall xa = ball (fset xa);

fmrel ::
  forall a b c. (Eq c) => (a -> b -> Bool) -> Fmap c a -> Fmap c b -> Bool;
fmrel r m n =
  fBall (fmdom m) (\ x -> rel_option r (fmlookup m x) (fmlookup n x)) &&
    fBall (fmdom n) (\ x -> rel_option r (fmlookup m x) (fmlookup n x));

equal_fmap :: forall a b. (Eq a, Eq b) => Fmap a b -> Fmap a b -> Bool;
equal_fmap = fmrel (\ a b -> a == b);

newtype Linear_poly = LinearPoly (Fmap Nat Rat);

linear_poly_map :: Linear_poly -> Fmap Nat Rat;
linear_poly_map (LinearPoly x) = x;

equal_linear_poly :: Linear_poly -> Linear_poly -> Bool;
equal_linear_poly x y = equal_fmap (linear_poly_map x) (linear_poly_map y);

instance Eq Linear_poly where {
  a == b = equal_linear_poly a b;
};

data Color = R | B;

data Rbta a b = Empty | Branch Color (Rbta a b) a b (Rbta a b);

newtype Rbt b a = RBT (Rbta b a);

data Atom a = Leq Nat a | Geq Nat a;

data Sum a b = Inl a | Inr b;

newtype Mapping a b = Mapping (Rbt a b);

data State a b =
  State [(Nat, Linear_poly)] (Mapping Nat (a, b)) (Mapping Nat (a, b))
    (Mapping Nat b) Bool (Maybe [a]);

data Istate a =
  IState Nat [(Nat, Linear_poly)] [(a, Atom QDelta)] (Linear_poly -> Maybe Nat)
    [a];

data Direction a b =
  Direction (b -> b -> Bool) (State a b -> Mapping Nat (a, b))
    (State a b -> Mapping Nat (a, b)) (State a b -> Nat -> Maybe b)
    (State a b -> Nat -> Maybe b) (State a b -> Nat -> a)
    (State a b -> Nat -> a)
    ((Mapping Nat (a, b) -> Mapping Nat (a, b)) -> State a b -> State a b)
    (Nat -> b -> Atom b) (Nat -> b -> Atom b) (Rat -> Rat -> Bool);

data Constraint = LT Linear_poly Rat | GT Linear_poly Rat | LEQ Linear_poly Rat
  | GEQ Linear_poly Rat | EQ Linear_poly Rat | LTPP Linear_poly Linear_poly
  | GTPP Linear_poly Linear_poly | LEQPP Linear_poly Linear_poly
  | GEQPP Linear_poly Linear_poly | EQPP Linear_poly Linear_poly;

data Ns_constraint a = LEQ_ns Linear_poly a | GEQ_ns Linear_poly a;

plus_nat :: Nat -> Nat -> Nat;
plus_nat m n = Nat (integer_of_nat m + integer_of_nat n);

one_nat :: Nat;
one_nat = Nat (1 :: Integer);

suc :: Nat -> Nat;
suc n = plus_nat n one_nat;

max :: forall a. (Ord a) => a -> a -> a;
max a b = (if less_eq a b then b else a);

minus_nat :: Nat -> Nat -> Nat;
minus_nat m n = Nat (max (0 :: Integer) (integer_of_nat m - integer_of_nat n));

zero_nat :: Nat;
zero_nat = Nat (0 :: Integer);

nth :: forall a. [a] -> Nat -> a;
nth (x : xs) n =
  (if equal_nat n zero_nat then x else nth xs (minus_nat n one_nat));

upt :: Nat -> Nat -> [Nat];
upt i j = (if less_nat i j then i : upt (suc i) j else []);

find :: forall a. (a -> Bool) -> [a] -> Maybe a;
find uu [] = Nothing;
find p (x : xs) = (if p x then Just x else find p xs);

empty :: forall a b. (Linorder a) => Rbt a b;
empty = RBT Empty;

foldl :: forall a b. (a -> b -> a) -> a -> [b] -> a;
foldl f a [] = a;
foldl f a (x : xs) = foldl f (f a x) xs;

foldr :: forall a b. (a -> b -> b) -> [a] -> b -> b;
foldr f [] = id;
foldr f (x : xs) = f x . foldr f xs;

val :: QDelta -> Rat -> Rat;
val qd delta = plus_rat (qdfst qd) (times_rat delta (qdsnd qd));

balance :: forall a b. Rbta a b -> a -> b -> Rbta a b -> Rbta a b;
balance (Branch R a w x b) s t (Branch R c y z d) =
  Branch R (Branch B a w x b) s t (Branch B c y z d);
balance (Branch R (Branch R a w x b) s t c) y z Empty =
  Branch R (Branch B a w x b) s t (Branch B c y z Empty);
balance (Branch R (Branch R a w x b) s t c) y z (Branch B va vb vc vd) =
  Branch R (Branch B a w x b) s t (Branch B c y z (Branch B va vb vc vd));
balance (Branch R Empty w x (Branch R b s t c)) y z Empty =
  Branch R (Branch B Empty w x b) s t (Branch B c y z Empty);
balance (Branch R (Branch B va vb vc vd) w x (Branch R b s t c)) y z Empty =
  Branch R (Branch B (Branch B va vb vc vd) w x b) s t (Branch B c y z Empty);
balance (Branch R Empty w x (Branch R b s t c)) y z (Branch B va vb vc vd) =
  Branch R (Branch B Empty w x b) s t (Branch B c y z (Branch B va vb vc vd));
balance (Branch R (Branch B ve vf vg vh) w x (Branch R b s t c)) y z
  (Branch B va vb vc vd) =
  Branch R (Branch B (Branch B ve vf vg vh) w x b) s t
    (Branch B c y z (Branch B va vb vc vd));
balance Empty w x (Branch R b s t (Branch R c y z d)) =
  Branch R (Branch B Empty w x b) s t (Branch B c y z d);
balance (Branch B va vb vc vd) w x (Branch R b s t (Branch R c y z d)) =
  Branch R (Branch B (Branch B va vb vc vd) w x b) s t (Branch B c y z d);
balance Empty w x (Branch R (Branch R b s t c) y z Empty) =
  Branch R (Branch B Empty w x b) s t (Branch B c y z Empty);
balance Empty w x (Branch R (Branch R b s t c) y z (Branch B va vb vc vd)) =
  Branch R (Branch B Empty w x b) s t (Branch B c y z (Branch B va vb vc vd));
balance (Branch B va vb vc vd) w x (Branch R (Branch R b s t c) y z Empty) =
  Branch R (Branch B (Branch B va vb vc vd) w x b) s t (Branch B c y z Empty);
balance (Branch B va vb vc vd) w x
  (Branch R (Branch R b s t c) y z (Branch B ve vf vg vh)) =
  Branch R (Branch B (Branch B va vb vc vd) w x b) s t
    (Branch B c y z (Branch B ve vf vg vh));
balance Empty s t Empty = Branch B Empty s t Empty;
balance Empty s t (Branch B va vb vc vd) =
  Branch B Empty s t (Branch B va vb vc vd);
balance Empty s t (Branch v Empty vb vc Empty) =
  Branch B Empty s t (Branch v Empty vb vc Empty);
balance Empty s t (Branch v (Branch B ve vf vg vh) vb vc Empty) =
  Branch B Empty s t (Branch v (Branch B ve vf vg vh) vb vc Empty);
balance Empty s t (Branch v Empty vb vc (Branch B vf vg vh vi)) =
  Branch B Empty s t (Branch v Empty vb vc (Branch B vf vg vh vi));
balance Empty s t (Branch v (Branch B ve vj vk vl) vb vc (Branch B vf vg vh vi))
  = Branch B Empty s t
      (Branch v (Branch B ve vj vk vl) vb vc (Branch B vf vg vh vi));
balance (Branch B va vb vc vd) s t Empty =
  Branch B (Branch B va vb vc vd) s t Empty;
balance (Branch B va vb vc vd) s t (Branch B ve vf vg vh) =
  Branch B (Branch B va vb vc vd) s t (Branch B ve vf vg vh);
balance (Branch B va vb vc vd) s t (Branch v Empty vf vg Empty) =
  Branch B (Branch B va vb vc vd) s t (Branch v Empty vf vg Empty);
balance (Branch B va vb vc vd) s t (Branch v (Branch B vi vj vk vl) vf vg Empty)
  = Branch B (Branch B va vb vc vd) s t
      (Branch v (Branch B vi vj vk vl) vf vg Empty);
balance (Branch B va vb vc vd) s t (Branch v Empty vf vg (Branch B vj vk vl vm))
  = Branch B (Branch B va vb vc vd) s t
      (Branch v Empty vf vg (Branch B vj vk vl vm));
balance (Branch B va vb vc vd) s t
  (Branch v (Branch B vi vn vo vp) vf vg (Branch B vj vk vl vm)) =
  Branch B (Branch B va vb vc vd) s t
    (Branch v (Branch B vi vn vo vp) vf vg (Branch B vj vk vl vm));
balance (Branch v Empty vb vc Empty) s t Empty =
  Branch B (Branch v Empty vb vc Empty) s t Empty;
balance (Branch v Empty vb vc (Branch B ve vf vg vh)) s t Empty =
  Branch B (Branch v Empty vb vc (Branch B ve vf vg vh)) s t Empty;
balance (Branch v (Branch B vf vg vh vi) vb vc Empty) s t Empty =
  Branch B (Branch v (Branch B vf vg vh vi) vb vc Empty) s t Empty;
balance (Branch v (Branch B vf vg vh vi) vb vc (Branch B ve vj vk vl)) s t Empty
  = Branch B (Branch v (Branch B vf vg vh vi) vb vc (Branch B ve vj vk vl)) s t
      Empty;
balance (Branch v Empty vf vg Empty) s t (Branch B va vb vc vd) =
  Branch B (Branch v Empty vf vg Empty) s t (Branch B va vb vc vd);
balance (Branch v Empty vf vg (Branch B vi vj vk vl)) s t (Branch B va vb vc vd)
  = Branch B (Branch v Empty vf vg (Branch B vi vj vk vl)) s t
      (Branch B va vb vc vd);
balance (Branch v (Branch B vj vk vl vm) vf vg Empty) s t (Branch B va vb vc vd)
  = Branch B (Branch v (Branch B vj vk vl vm) vf vg Empty) s t
      (Branch B va vb vc vd);
balance (Branch v (Branch B vj vk vl vm) vf vg (Branch B vi vn vo vp)) s t
  (Branch B va vb vc vd) =
  Branch B (Branch v (Branch B vj vk vl vm) vf vg (Branch B vi vn vo vp)) s t
    (Branch B va vb vc vd);

rbt_ins ::
  forall a b. (Ord a) => (a -> b -> b -> b) -> a -> b -> Rbta a b -> Rbta a b;
rbt_ins f k v Empty = Branch R Empty k v Empty;
rbt_ins f k v (Branch B l x y r) =
  (if less k x then balance (rbt_ins f k v l) x y r
    else (if less x k then balance l x y (rbt_ins f k v r)
           else Branch B l x (f k y v) r));
rbt_ins f k v (Branch R l x y r) =
  (if less k x then Branch R (rbt_ins f k v l) x y r
    else (if less x k then Branch R l x y (rbt_ins f k v r)
           else Branch R l x (f k y v) r));

paint :: forall a b. Color -> Rbta a b -> Rbta a b;
paint c Empty = Empty;
paint c (Branch uu l k v r) = Branch c l k v r;

rbt_insert_with_key ::
  forall a b. (Ord a) => (a -> b -> b -> b) -> a -> b -> Rbta a b -> Rbta a b;
rbt_insert_with_key f k v t = paint B (rbt_ins f k v t);

rbt_insert :: forall a b. (Ord a) => a -> b -> Rbta a b -> Rbta a b;
rbt_insert = rbt_insert_with_key (\ _ _ nv -> nv);

impl_of :: forall b a. (Linorder b) => Rbt b a -> Rbta b a;
impl_of (RBT x) = x;

insert :: forall a b. (Linorder a) => a -> b -> Rbt a b -> Rbt a b;
insert xc xd xe = RBT (rbt_insert xc xd (impl_of xe));

rbt_lookup :: forall a b. (Ord a) => Rbta a b -> a -> Maybe b;
rbt_lookup Empty k = Nothing;
rbt_lookup (Branch uu l x y r) k =
  (if less k x then rbt_lookup l k
    else (if less x k then rbt_lookup r k else Just y));

lookup :: forall a b. (Linorder a) => Rbt a b -> a -> Maybe b;
lookup x = rbt_lookup (impl_of x);

membera :: forall a. (Eq a) => [a] -> a -> Bool;
membera [] y = False;
membera (x : xs) y = x == y || membera xs y;

member :: forall a. (Eq a) => a -> Set a -> Bool;
member x (Coset xs) = not (membera xs x);
member x (Set xs) = membera xs x;

update :: forall a b. (Eq a) => a -> b -> [(a, b)] -> [(a, b)];
update k v [] = [(k, v)];
update k v (p : ps) = (if fst p == k then (k, v) : ps else p : update k v ps);

merge :: forall a b. (Eq a) => [(a, b)] -> [(a, b)] -> [(a, b)];
merge qs ps = foldr (\ (a, b) -> update a b) ps qs;

fun_upd :: forall a b. (Eq a) => (a -> b) -> a -> b -> a -> b;
fun_upd f a b = (\ x -> (if x == a then b else f x));

lhs :: (Nat, Linear_poly) -> Nat;
lhs (l, r) = l;

rhs :: (Nat, Linear_poly) -> Linear_poly;
rhs (l, r) = r;

hd :: forall a. [a] -> a;
hd (x21 : x22) = x21;

tl :: forall a. [a] -> [a];
tl [] = [];
tl (x21 : x22) = x22;

remdups :: forall a. (Eq a) => [a] -> [a];
remdups [] = [];
remdups (x : xs) = (if membera xs x then remdups xs else x : remdups xs);

rbt_bulkload :: forall a b. (Ord a) => [(a, b)] -> Rbta a b;
rbt_bulkload xs = foldr (\ (a, b) -> rbt_insert a b) xs Empty;

bulkload :: forall a b. (Linorder a) => [(a, b)] -> Rbt a b;
bulkload xa = RBT (rbt_bulkload xa);

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

poly :: forall a. Ns_constraint a -> Linear_poly;
poly (LEQ_ns p a) = p;
poly (GEQ_ns p a) = p;

emptya :: forall a b. (Linorder a) => Mapping a b;
emptya = Mapping empty;

lvars :: [(Nat, Linear_poly)] -> Set Nat;
lvars t = Set (map lhs t);

fmmap :: forall a b c. (a -> b) -> Fmap c a -> Fmap c b;
fmmap f (Fmap_of_list m) = Fmap_of_list (map (apsnd f) m);

fmempty :: forall a b. Fmap a b;
fmempty = Fmap_of_list [];

scale :: Rat -> Fmap Nat Rat -> Fmap Nat Rat;
scale r lp = (if equal_rat r zero_rat then fmempty else fmmap (times_rat r) lp);

scaleRat_linear_poly :: Rat -> Linear_poly -> Linear_poly;
scaleRat_linear_poly r p = LinearPoly (scale r (linear_poly_map p));

uminus_linear_poly :: Linear_poly -> Linear_poly;
uminus_linear_poly lp = scaleRat_linear_poly (uminus_rat one_rat) lp;

get_var_coeff :: Fmap Nat Rat -> Nat -> Rat;
get_var_coeff lp v = (case fmlookup lp v of {
                       Nothing -> zero_rat;
                       Just c -> c;
                     });

divide_nat :: Nat -> Nat -> Nat;
divide_nat m n = Nat (divide_integer (integer_of_nat m) (integer_of_nat n));

gen_length :: forall a. Nat -> [a] -> Nat;
gen_length n (x : xs) = gen_length (suc n) xs;
gen_length n [] = n;

size_list :: forall a. [a] -> Nat;
size_list = gen_length zero_nat;

part :: forall a b. (Linorder b) => (a -> b) -> b -> [a] -> ([a], ([a], [a]));
part f pivot (x : xs) =
  (case part f pivot xs of {
    (lts, (eqs, gts)) ->
      let {
        xa = f x;
      } in (if less xa pivot then (x : lts, (eqs, gts))
             else (if less pivot xa then (lts, (eqs, x : gts))
                    else (lts, (x : eqs, gts))));
  });
part f pivot [] = ([], ([], []));

nat_of_integer :: Integer -> Nat;
nat_of_integer k = Nat (max (0 :: Integer) k);

sort_key :: forall a b. (Linorder b) => (a -> b) -> [a] -> [a];
sort_key f xs =
  (case xs of {
    [] -> [];
    [_] -> xs;
    [x, y] -> (if less_eq (f x) (f y) then xs else [y, x]);
    _ : _ : _ : _ ->
      (case part f
              (f (nth xs
                   (divide_nat (size_list xs) (nat_of_integer (2 :: Integer)))))
              xs
        of {
        (lts, (eqs, gts)) -> sort_key f lts ++ eqs ++ sort_key f gts;
      });
  });

sorted_list_of_set :: forall a. (Eq a, Linorder a) => Set a -> [a];
sorted_list_of_set (Set xs) = sort_key (\ x -> x) (remdups xs);

ordered_keys :: forall a b. (Eq a, Linorder a) => Fmap a b -> [a];
ordered_keys m = sorted_list_of_set (fset (fmdom m));

fmfilter :: forall a b. (a -> Bool) -> Fmap a b -> Fmap a b;
fmfilter p (Fmap_of_list m) = Fmap_of_list (filter (\ (k, _) -> p k) m);

fmdrop :: forall a b. (Eq a) => a -> Fmap a b -> Fmap a b;
fmdrop a = fmfilter (\ aa -> not (aa == a));

fmadd :: forall a b. (Eq a) => Fmap a b -> Fmap a b -> Fmap a b;
fmadd (Fmap_of_list m) (Fmap_of_list n) = Fmap_of_list (merge m n);

fmupd :: forall a b. (Eq a) => a -> b -> Fmap a b -> Fmap a b;
fmupd k v m = fmadd m (Fmap_of_list [(k, v)]);

set_var_coeff :: Nat -> Rat -> Fmap Nat Rat -> Fmap Nat Rat;
set_var_coeff v c lp =
  (if equal_rat c zero_rat then fmdrop v lp else fmupd v c lp);

add_monom :: Rat -> Nat -> Fmap Nat Rat -> Fmap Nat Rat;
add_monom c v lp = set_var_coeff v (plus_rat c (get_var_coeff lp v)) lp;

add :: Fmap Nat Rat -> Fmap Nat Rat -> Fmap Nat Rat;
add lp1 lp2 =
  foldl (\ lp v -> add_monom (get_var_coeff lp1 v) v lp) lp2 (ordered_keys lp1);

plus_linear_poly :: Linear_poly -> Linear_poly -> Linear_poly;
plus_linear_poly p1 p2 =
  LinearPoly (add (linear_poly_map p1) (linear_poly_map p2));

minus_linear_poly :: Linear_poly -> Linear_poly -> Linear_poly;
minus_linear_poly lp1 lp2 = plus_linear_poly lp1 (uminus_linear_poly lp2);

constraint_to_qdelta_constraint :: Constraint -> [Ns_constraint QDelta];
constraint_to_qdelta_constraint (LT l r) =
  [LEQ_ns l (QDelta r (uminus_rat one_rat))];
constraint_to_qdelta_constraint (GT l r) = [GEQ_ns l (QDelta r one_rat)];
constraint_to_qdelta_constraint (LEQ l r) = [LEQ_ns l (QDelta r zero_rat)];
constraint_to_qdelta_constraint (GEQ l r) = [GEQ_ns l (QDelta r zero_rat)];
constraint_to_qdelta_constraint (EQ l r) =
  [LEQ_ns l (QDelta r zero_rat), GEQ_ns l (QDelta r zero_rat)];
constraint_to_qdelta_constraint (LTPP l1 l2) =
  [LEQ_ns (minus_linear_poly l1 l2) (QDelta zero_rat (uminus_rat one_rat))];
constraint_to_qdelta_constraint (GTPP l1 l2) =
  [GEQ_ns (minus_linear_poly l1 l2) (QDelta zero_rat one_rat)];
constraint_to_qdelta_constraint (LEQPP l1 l2) =
  [LEQ_ns (minus_linear_poly l1 l2) zero_QDelta];
constraint_to_qdelta_constraint (GEQPP l1 l2) =
  [GEQ_ns (minus_linear_poly l1 l2) zero_QDelta];
constraint_to_qdelta_constraint (EQPP l1 l2) =
  [LEQ_ns (minus_linear_poly l1 l2) zero_QDelta,
    GEQ_ns (minus_linear_poly l1 l2) zero_QDelta];

i_constraint_to_qdelta_constraint ::
  forall a. (a, Constraint) -> [(a, Ns_constraint QDelta)];
i_constraint_to_qdelta_constraint (i, c) =
  map (\ a -> (i, a)) (constraint_to_qdelta_constraint c);

to_ns :: forall a. [(a, Constraint)] -> [(a, Ns_constraint QDelta)];
to_ns l = concatMap i_constraint_to_qdelta_constraint l;

lookupa :: forall a b. (Linorder a) => Mapping a b -> a -> Maybe b;
lookupa (Mapping t) = lookup t;

updatea :: forall a b. (Linorder a) => a -> b -> Mapping a b -> Mapping a b;
updatea k v (Mapping t) = Mapping (insert k v t);

b_i_l :: forall a b. State a b -> Mapping Nat (a, b);
b_i_l (State x1 x2 x3 x4 x5 x6) = x2;

the :: forall a. Maybe a -> a;
the (Just x2) = x2;

indexl :: forall a b. State a b -> Nat -> a;
indexl s = (fst . the) . lookupa (b_i_l s);

b_i_u :: forall a b. State a b -> Mapping Nat (a, b);
b_i_u (State x1 x2 x3 x4 x5 x6) = x3;

indexu :: forall a b. State a b -> Nat -> a;
indexu s = (fst . the) . lookupa (b_i_u s);

map_option :: forall a b. (a -> b) -> Maybe a -> Maybe b;
map_option f Nothing = Nothing;
map_option f (Just x2) = Just (f x2);

boundsl :: forall a b. State a b -> Nat -> Maybe b;
boundsl s = map_option snd . lookupa (b_i_l s);

boundsu :: forall a b. State a b -> Nat -> Maybe b;
boundsu s = map_option snd . lookupa (b_i_u s);

vars_list :: Linear_poly -> [Nat];
vars_list lp = ordered_keys (linear_poly_map lp);

min :: forall a. (Ord a) => a -> a -> a;
min a b = (if less_eq a b then a else b);

sum_list :: forall a. (Monoid_add a) => [a] -> a;
sum_list xs = foldr plus xs zeroa;

valuate :: forall a. (Rational_vector a) => Linear_poly -> (Nat -> a) -> a;
valuate lp val =
  let {
    lpm = linear_poly_map lp;
  } in sum_list
         (map (\ x -> scaleRat (the (fmlookup lpm x)) (val x)) (vars_list lp));

divide_rat :: Rat -> Rat -> Rat;
divide_rat p q =
  Frct (let {
          a = quotient_of p;
        } in (case a of {
               (aa, c) ->
                 let {
                   b = quotient_of q;
                 } in (case b of {
                        (ba, d) -> normalize (times_int aa d, times_int c ba);
                      });
             }));

delta_0 :: QDelta -> QDelta -> Rat;
delta_0 qd1 qd2 =
  let {
    c1 = qdfst qd1;
    c2 = qdfst qd2;
    k1 = qdsnd qd1;
    k2 = qdsnd qd2;
  } in (if less_rat c1 c2 && less_rat k2 k1
         then divide_rat (minus_rat c2 c1) (minus_rat k1 k2) else one_rat);

delta_0_val :: Ns_constraint QDelta -> (Nat -> QDelta) -> Rat;
delta_0_val (LEQ_ns lll rrr) vl = delta_0 (valuate lll vl) rrr;
delta_0_val (GEQ_ns lll rrr) vl = delta_0 rrr (valuate lll vl);

delta_0_val_min :: [Ns_constraint QDelta] -> (Nat -> QDelta) -> Rat;
delta_0_val_min [] vl = one_rat;
delta_0_val_min (h : t) vl = min (delta_0_val_min t vl) (delta_0_val h vl);

tabulate :: forall a b. (Linorder a) => [a] -> (a -> b) -> Mapping a b;
tabulate ks f = Mapping (bulkload (map (\ k -> (k, f k)) ks));

map2fun :: forall a. (Zero a) => Mapping Nat a -> Nat -> a;
map2fun v = (\ x -> (case lookupa v x of {
                      Nothing -> zeroa;
                      Just y -> y;
                    }));

from_ns :: Mapping Nat QDelta -> [Ns_constraint QDelta] -> Mapping Nat Rat;
from_ns vl cs =
  let {
    delta = delta_0_val_min cs (map2fun vl);
  } in tabulate (remdups (concatMap vars_list (map poly cs)))
         (\ var -> val (map2fun vl var) delta);

uBI_upd ::
  forall a b.
    (Linorder b) => Direction a b ->
                      (Mapping Nat (a, b) -> Mapping Nat (a, b)) ->
                        State a b -> State a b;
uBI_upd (Direction x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = x8;

update_B_I ::
  forall a b c d e.
    (Linorder a) => ((Mapping a (b, c) -> Mapping a (b, c)) -> d -> e) ->
                      b -> a -> c -> d -> e;
update_B_I field_update i x c s = field_update (updatea x (i, c)) s;

lt :: forall a b. (Linorder b) => Direction a b -> b -> b -> Bool;
lt (Direction x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = x1;

ub :: forall a b. (Linorder b) => Direction a b -> State a b -> Nat -> Maybe b;
ub (Direction x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = x5;

li :: forall a b. (Linorder b) => Direction a b -> State a b -> Nat -> a;
li (Direction x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = x6;

lb :: forall a b. (Linorder b) => Direction a b -> State a b -> Nat -> Maybe b;
lb (Direction x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = x4;

v_update :: forall a b. Mapping Nat a -> State b a -> State b a;
v_update v (State t bil biu v_old u uc) = State t bil biu v u uc;

v :: forall a b. State a b -> Mapping Nat b;
v (State x1 x2 x3 x4 x5 x6) = x4;

t :: forall a b. State a b -> [(Nat, Linear_poly)];
t (State x1 x2 x3 x4 x5 x6) = x1;

coeff :: Linear_poly -> Nat -> Rat;
coeff lp = get_var_coeff (linear_poly_map lp);

rhs_eq_val ::
  forall a.
    (Minus a, Plus a, Zero a,
      ScaleRat a) => Mapping Nat a -> Nat -> a -> (Nat, Linear_poly) -> a;
rhs_eq_val v x_i c e =
  let {
    x_j = lhs e;
    a_i_j = coeff (rhs e) x_i;
  } in plus (map2fun v x_j) (scaleRat a_i_j (minus c (map2fun v x_i)));

update_code :: forall a b. (Lrv a) => Nat -> a -> State b a -> State b a;
update_code x c s =
  v_update
    (updatea x c
      (foldl (\ va e -> updatea (lhs e) (rhs_eq_val (v s) x c e) va) (v s)
        (t s)))
    s;

set_unsat :: forall a b. (Eq a) => [a] -> State a b -> State a b;
set_unsat i (State t bil biu v u uc) =
  State t bil biu v True (Just (remdups i));

assert_bound_codea ::
  forall a b.
    (Eq a, Eq b,
      Lrv b) => Direction a b -> a -> Nat -> b -> State a b -> State a b;
assert_bound_codea dir i x c s =
  (if geub (lt dir) c (ub dir s x) then s
    else let {
           sa = update_B_I (uBI_upd dir) i x c s;
         } in (if ltlb (lt dir) c (lb dir s x) then set_unsat [i, li dir s x] sa
                else (if not (member x (lvars (t sa))) &&
                           lt dir c (map2fun (v s) x)
                       then update_code x c sa else sa)));

b_i_u_update ::
  forall a b.
    (Mapping Nat (a, b) -> Mapping Nat (a, b)) -> State a b -> State a b;
b_i_u_update up (State t bil biu v u uc) = State t bil (up biu) v u uc;

positive :: forall a b. (Linorder b) => Direction a b;
positive =
  Direction less b_i_l b_i_u boundsl boundsu indexl indexu b_i_u_update Leq Geq
    less_eq_rat;

b_i_l_update ::
  forall a b.
    (Mapping Nat (a, b) -> Mapping Nat (a, b)) -> State a b -> State a b;
b_i_l_update up (State t bil biu v u uc) = State t (up bil) biu v u uc;

negative :: forall a b. (Linorder b) => Direction a b;
negative =
  Direction (\ x y -> less y x) b_i_u b_i_l boundsu boundsl indexu indexl
    b_i_l_update Geq Leq (\ x y -> less_eq_rat y x);

assert_bound_code ::
  forall a b. (Eq a, Eq b, Lrv b) => (a, Atom b) -> State a b -> State a b;
assert_bound_code (i, Leq x c) s = assert_bound_codea positive i x c s;
assert_bound_code (i, Geq x c) s = assert_bound_codea negative i x c s;

u :: forall a b. State a b -> Bool;
u (State x1 x2 x3 x4 x5 x6) = x5;

assert_bound_loop_code ::
  forall a b. (Eq a, Eq b, Lrv b) => [(a, Atom b)] -> State a b -> State a b;
assert_bound_loop_code ats s =
  foldl (\ sa a -> (if u sa then sa else assert_bound_code a sa)) s ats;

init_state :: forall a b. (Zero b) => [(Nat, Linear_poly)] -> State a b;
init_state t =
  State t emptya emptya
    (tabulate (remdups (map lhs t ++ concatMap (vars_list . rhs) t))
      (\ _ -> zeroa))
    False Nothing;

min_satisfying :: forall a. (Linorder a) => (a -> Bool) -> [a] -> Maybe a;
min_satisfying p l =
  let {
    xs = filter p l;
  } in (if null xs then Nothing else Just (foldl min (hd xs) (tl xs)));

le_ubound :: forall a. (Eq a, Linorder a) => a -> Maybe a -> Bool;
le_ubound c b = leub less c b;

ge_lbound :: forall a. (Eq a, Linorder a) => a -> Maybe a -> Bool;
ge_lbound c b = gelb less c b;

in_bounds ::
  forall a b.
    (Eq b, Linorder b) => a -> (a -> b) -> (a -> Maybe b, a -> Maybe b) -> Bool;
in_bounds x v (lb, ub) = ge_lbound (v x) (lb x) && le_ubound (v x) (ub x);

min_lvar_not_in_bounds ::
  forall a b. (Zero b, Eq b, Linorder b) => State a b -> Maybe Nat;
min_lvar_not_in_bounds s =
  min_satisfying
    (\ x -> not (in_bounds x (map2fun (v s)) (boundsl s, boundsu s)))
    (map lhs (t s));

var :: Nat -> Linear_poly;
var x = LinearPoly (set_var_coeff x one_rat fmempty);

subst_var :: Nat -> Linear_poly -> Linear_poly -> Linear_poly;
subst_var v lpa lp =
  minus_linear_poly
    (plus_linear_poly lp (scaleRat_linear_poly (coeff lp v) lpa))
    (scaleRat_linear_poly (coeff lp v) (var v));

subst_var_eq_code ::
  Nat -> Linear_poly -> (Nat, Linear_poly) -> (Nat, Linear_poly);
subst_var_eq_code v lp eq = (lhs eq, subst_var v lp (rhs eq));

eq_idx_for_lvar_aux :: [(Nat, Linear_poly)] -> Nat -> Nat -> Nat;
eq_idx_for_lvar_aux [] x i = i;
eq_idx_for_lvar_aux (eq : t) x i =
  (if equal_nat (lhs eq) x then i
    else eq_idx_for_lvar_aux t x (plus_nat i one_nat));

eq_idx_for_lvar :: [(Nat, Linear_poly)] -> Nat -> Nat;
eq_idx_for_lvar t x = eq_idx_for_lvar_aux t x zero_nat;

eq_for_lvar_code :: [(Nat, Linear_poly)] -> Nat -> (Nat, Linear_poly);
eq_for_lvar_code t v = nth t (eq_idx_for_lvar t v);

pivot_eq :: (Nat, Linear_poly) -> Nat -> (Nat, Linear_poly);
pivot_eq e y =
  let {
    cy = coeff (rhs e) y;
  } in (y, plus_linear_poly
             (scaleRat_linear_poly (divide_rat (uminus_rat one_rat) cy)
               (minus_linear_poly (rhs e) (scaleRat_linear_poly cy (var y))))
             (scaleRat_linear_poly (divide_rat one_rat cy) (var (lhs e))));

pivot_tableau_code ::
  Nat -> Nat -> [(Nat, Linear_poly)] -> [(Nat, Linear_poly)];
pivot_tableau_code x_i x_j t =
  let {
    eq = eq_for_lvar_code t x_i;
    eqa = pivot_eq eq x_j;
  } in map (\ e ->
             (if equal_nat (lhs e) (lhs eq) then eqa
               else subst_var_eq_code x_j (rhs eqa) e))
         t;

t_update :: forall a b. [(Nat, Linear_poly)] -> State a b -> State a b;
t_update t (State t_old bil biu v u uc) = State t bil biu v u uc;

pivot_code :: forall a b. (Lrv b) => Nat -> Nat -> State a b -> State a b;
pivot_code x_i x_j s = t_update (pivot_tableau_code x_i x_j (t s)) s;

pivot_and_update_code ::
  forall a b. (Lrv a) => Nat -> Nat -> a -> State b a -> State b a;
pivot_and_update_code x_i x_j c s = update_code x_i c (pivot_code x_i x_j s);

ui :: forall a b. (Linorder b) => Direction a b -> State a b -> Nat -> a;
ui (Direction x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = x7;

unsat_indices ::
  forall a b.
    (Eq a,
      Linorder b) => Direction a b ->
                       State a b -> [Nat] -> (Nat, Linear_poly) -> [a];
unsat_indices dir s vs eq =
  let {
    r = rhs eq;
    lia = li dir s;
    uia = ui dir s;
  } in remdups
         (lia (lhs eq) :
           map (\ x -> (if less_rat (coeff r x) zero_rat then lia x else uia x))
             vs);

min_rvar_incdec_eq ::
  forall a b.
    (Eq a,
      Lrv b) => Direction a b -> State a b -> (Nat, Linear_poly) -> Sum [a] Nat;
min_rvar_incdec_eq dir s eq =
  let {
    rvars = vars_list (rhs eq);
  } in (case min_satisfying
               (\ x ->
                 less_rat zero_rat (coeff (rhs eq) x) &&
                   ltub (lt dir) (map2fun (v s) x) (ub dir s x) ||
                   less_rat (coeff (rhs eq) x) zero_rat &&
                     gtlb (lt dir) (map2fun (v s) x) (lb dir s x))
               rvars
         of {
         Nothing -> Inl (unsat_indices dir s rvars eq);
         Just a -> Inr a;
       });

check_codea ::
  forall a b. (Eq a, Lrv b) => Direction a b -> Nat -> State a b -> State a b;
check_codea dir x_i s =
  let {
    l_i = the (lb dir s x_i);
  } in (case min_rvar_incdec_eq dir s (eq_for_lvar_code (t s) x_i) of {
         Inl i -> set_unsat i s;
         Inr x_j -> pivot_and_update_code x_i x_j l_i s;
       });

lt_lbound :: forall a. (Linorder a) => a -> Maybe a -> Bool;
lt_lbound c b = ltlb less c b;

check_code :: forall a b. (Eq a, Eq b, Lrv b) => State a b -> State a b;
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
    (Eq a, Eq b, Lrv b) => [(Nat, Linear_poly)] -> [(a, Atom b)] -> State a b;
assert_all_state_code t ats =
  check_code (assert_bound_loop_code ats (init_state t));

u_c :: forall a b. State a b -> Maybe [a];
u_c (State x1 x2 x3 x4 x5 x6) = x6;

assert_all_code ::
  forall a b.
    (Eq a, Eq b,
      Lrv b) => [(Nat, Linear_poly)] ->
                  [(a, Atom b)] -> Sum [a] (Mapping Nat b);
assert_all_code t asa = let {
                          s = assert_all_state_code t asa;
                        } in (if u s then Inl (the (u_c s)) else Inr (v s));

sgn_int :: Int -> Int;
sgn_int i =
  (if equal_int i zero_int then zero_int
    else (if less_int zero_int i then one_int else uminus_int one_int));

abs_int :: Int -> Int;
abs_int i = (if less_int i zero_int then uminus_int i else i);

inverse_rat :: Rat -> Rat;
inverse_rat p =
  Frct (let {
          a = quotient_of p;
        } in (case a of {
               (aa, b) ->
                 (if equal_int aa zero_int then (zero_int, one_int)
                   else (times_int (sgn_int aa) b, abs_int aa));
             }));

max_var :: Linear_poly -> Nat;
max_var lp = let {
               vl = vars_list lp;
             } in (if null vl then zero_nat else foldl max (hd vl) (tl vl));

normalize_ns_constraint ::
  forall a. (Lrv a) => Ns_constraint a -> Ns_constraint a;
normalize_ns_constraint (LEQ_ns l r) =
  let {
    v = max_var l;
    c = coeff l v;
  } in (if equal_rat c zero_rat then LEQ_ns l r
         else let {
                ic = inverse_rat c;
              } in (if less_rat c zero_rat
                     then GEQ_ns (scaleRat_linear_poly ic l) (scaleRat ic r)
                     else LEQ_ns (scaleRat_linear_poly ic l) (scaleRat ic r)));
normalize_ns_constraint (GEQ_ns l r) =
  let {
    v = max_var l;
    c = coeff l v;
  } in (if equal_rat c zero_rat then GEQ_ns l r
         else let {
                ic = inverse_rat c;
              } in (if less_rat c zero_rat
                     then LEQ_ns (scaleRat_linear_poly ic l) (scaleRat ic r)
                     else GEQ_ns (scaleRat_linear_poly ic l) (scaleRat ic r)));

pivot_tableau_eq ::
  [(Nat, Linear_poly)] ->
    (Nat, Linear_poly) ->
      [(Nat, Linear_poly)] ->
        Nat ->
          ([(Nat, Linear_poly)], ((Nat, Linear_poly), [(Nat, Linear_poly)]));
pivot_tableau_eq t1 eq t2 x = let {
                                eqa = pivot_eq eq x;
                                m = map (subst_var_eq_code x (rhs eqa));
                              } in (m t1, (eqa, m t2));

preprocess_opt ::
  forall a.
    (Lrv a) => Set Nat ->
                 [(Nat, Linear_poly)] ->
                   [(Nat, Linear_poly)] ->
                     ([(Nat, Linear_poly)], Mapping Nat a -> Mapping Nat a);
preprocess_opt x t1 [] = (t1, id);
preprocess_opt xa t1 ((x, p) : t2) =
  (if not (member x xa)
    then (case preprocess_opt xa t1 t2 of {
           (t, tv) -> (t, (\ v -> updatea x (valuate p (map2fun v)) v) . tv);
         })
    else (case find (\ xb -> not (member xb xa)) (vars_list p) of {
           Nothing -> preprocess_opt xa ((x, p) : t1) t2;
           Just y ->
             (case pivot_tableau_eq t1 (x, p) t2 y of {
               (tt1, ((z, q), tt2)) ->
                 (case preprocess_opt xa tt1 tt2 of {
                   (t, tv) ->
                     (t, (\ v -> updatea z (valuate q (map2fun v)) v) . tv);
                 });
             });
         }));

atom_var :: forall a. Atom a -> Nat;
atom_var (Leq var a) = var;
atom_var (Geq var a) = var;

preprocess_part_2 ::
  forall a b c.
    (Lrv c) => [(a, Atom b)] ->
                 [(Nat, Linear_poly)] ->
                   ([(Nat, Linear_poly)], Mapping Nat c -> Mapping Nat c);
preprocess_part_2 asa t =
  preprocess_opt (image atom_var (image snd (Set asa))) [] t;

start_fresh_variable :: forall a. [(a, Ns_constraint QDelta)] -> Nat;
start_fresh_variable [] = zero_nat;
start_fresh_variable ((i, h) : t) =
  max (plus_nat (max_var (poly h)) one_nat) (start_fresh_variable t);

unsatIndices :: forall a. Istate a -> [a];
unsatIndices (IState x1 x2 x3 x4 x5) = x5;

tableau :: forall a. Istate a -> [(Nat, Linear_poly)];
tableau (IState x1 x2 x3 x4 x5) = x2;

atoms :: forall a. Istate a -> [(a, Atom QDelta)];
atoms (IState x1 x2 x3 x4 x5) = x3;

zero :: Fmap Nat Rat;
zero = fmempty;

zero_linear_poly :: Linear_poly;
zero_linear_poly = LinearPoly zero;

qdelta_constraint_to_atom :: Ns_constraint QDelta -> Nat -> Atom QDelta;
qdelta_constraint_to_atom (LEQ_ns l r) v = Leq v r;
qdelta_constraint_to_atom (GEQ_ns l r) v = Geq v r;

firstFreshVariable :: forall a. Istate a -> Nat;
firstFreshVariable (IState x1 x2 x3 x4 x5) = x1;

is_monom :: Linear_poly -> Bool;
is_monom l = equal_nat (size_list (vars_list l)) one_nat;

poly_Mapping :: forall a. Istate a -> Linear_poly -> Maybe Nat;
poly_Mapping (IState x1 x2 x3 x4 x5) = x4;

linear_poly_to_eq :: Linear_poly -> Nat -> (Nat, Linear_poly);
linear_poly_to_eq p v = (v, p);

zero_satisfies :: forall a. (Lrv a) => Ns_constraint a -> Bool;
zero_satisfies (LEQ_ns l r) = less_eq zeroa r;
zero_satisfies (GEQ_ns l r) = less_eq r zeroa;

monom_var :: Linear_poly -> Nat;
monom_var l = max_var l;

monom_coeff :: Linear_poly -> Rat;
monom_coeff l = coeff l (monom_var l);

monom_to_atom :: Ns_constraint QDelta -> Atom QDelta;
monom_to_atom (LEQ_ns l r) =
  (if less_rat (monom_coeff l) zero_rat
    then Geq (monom_var l) (scaleRat_QDelta (inverse_rat (monom_coeff l)) r)
    else Leq (monom_var l) (scaleRat_QDelta (inverse_rat (monom_coeff l)) r));
monom_to_atom (GEQ_ns l r) =
  (if less_rat (monom_coeff l) zero_rat
    then Leq (monom_var l) (scaleRat_QDelta (inverse_rat (monom_coeff l)) r)
    else Geq (monom_var l) (scaleRat_QDelta (inverse_rat (monom_coeff l)) r));

preprocessa :: forall a. [(a, Ns_constraint QDelta)] -> Nat -> Istate a;
preprocessa ((i, h) : t) v =
  let {
    s = preprocessa t v;
    p = poly h;
    is_monom_h = is_monom p;
    va = firstFreshVariable s;
    ta = tableau s;
    a = atoms s;
    m = poly_Mapping s;
    u = unsatIndices s;
  } in (if is_monom_h then IState va ta ((i, monom_to_atom h) : a) m u
         else (if equal_linear_poly p zero_linear_poly
                then (if zero_satisfies h then s else IState va ta a m (i : u))
                else (case m p of {
                       Nothing ->
                         IState (plus_nat va one_nat)
                           (linear_poly_to_eq p va : ta)
                           ((i, qdelta_constraint_to_atom h va) : a)
                           (fun_upd m p (Just va)) u;
                       Just vaa ->
                         IState va ta ((i, qdelta_constraint_to_atom h vaa) : a)
                           m u;
                     })));
preprocessa [] v = IState v [] [] (\ _ -> Nothing) [];

preprocess_part_1 ::
  forall a.
    [(a, Ns_constraint QDelta)] ->
      ([(Nat, Linear_poly)], ([(a, Atom QDelta)], [a]));
preprocess_part_1 l = let {
                        start = start_fresh_variable l;
                        is = preprocessa l start;
                      } in (tableau is, (atoms is, unsatIndices is));

map_prod :: forall a b c d. (a -> b) -> (c -> d) -> (a, c) -> (b, d);
map_prod f g (a, b) = (f a, g b);

preprocess ::
  forall a.
    [(a, Ns_constraint QDelta)] ->
      ([(Nat, Linear_poly)],
        ([(a, Atom QDelta)], (Mapping Nat QDelta -> Mapping Nat QDelta, [a])));
preprocess l =
  (case preprocess_part_1 (map (map_prod id normalize_ns_constraint) l) of {
    (t, (asa, ui)) -> (case preprocess_part_2 asa t of {
                        (ta, tv) -> (ta, (asa, (tv, ui)));
                      });
  });

solve_exec_ns_code ::
  forall a.
    (Eq a) => [(a, Ns_constraint QDelta)] -> Sum [a] (Mapping Nat QDelta);
solve_exec_ns_code s =
  (case preprocess s of {
    (t, (asa, (trans_v, []))) -> (case assert_all_code t asa of {
                                   Inl a -> Inl a;
                                   Inr v -> Inr (trans_v v);
                                 });
    (_, (_, (_, i : _))) -> Inl [i];
  });

solve_exec_code ::
  forall a. (Eq a) => [(a, Constraint)] -> Sum [a] (Mapping Nat Rat);
solve_exec_code cs = let {
                       csa = to_ns cs;
                     } in (case solve_exec_ns_code csa of {
                            Inl a -> Inl a;
                            Inr v -> Inr (from_ns v (map snd csa));
                          });

simplex_index ::
  forall a. (Eq a) => [(a, Constraint)] -> Sum [a] (Mapping Nat Rat);
simplex_index = solve_exec_code;

simplex :: [Constraint] -> Sum [Nat] (Mapping Nat Rat);
simplex cs = simplex_index (zip (upt zero_nat (size_list cs)) cs);

}
