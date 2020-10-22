{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module QDelta(QDelta(..), zero_QDelta, scaleRat_QDelta, val, delta_0) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified HOL;
import qualified Rat;
import qualified Simplex_Algebra;
import qualified Orderings;
import qualified Arith;

data QDelta = QDelta Rat.Rat Rat.Rat;

equal_QDelta :: QDelta -> QDelta -> Bool;
equal_QDelta (QDelta x1 x2) (QDelta y1 y2) =
  Rat.equal_rat x1 y1 && Rat.equal_rat x2 y2;

instance Eq QDelta where {
  a == b = equal_QDelta a b;
};

one_QDelta :: QDelta;
one_QDelta = QDelta Rat.one_rat Rat.zero_rat;

instance Arith.One QDelta where {
  one = one_QDelta;
};

qdsnd :: QDelta -> Rat.Rat;
qdsnd (QDelta a b) = b;

qdfst :: QDelta -> Rat.Rat;
qdfst (QDelta a b) = a;

plus_QDelta :: QDelta -> QDelta -> QDelta;
plus_QDelta qd1 qd2 =
  QDelta (Rat.plus_rat (qdfst qd1) (qdfst qd2))
    (Rat.plus_rat (qdsnd qd1) (qdsnd qd2));

instance Arith.Plus QDelta where {
  plus = plus_QDelta;
};

zero_QDelta :: QDelta;
zero_QDelta = QDelta Rat.zero_rat Rat.zero_rat;

instance Arith.Zero QDelta where {
  zero = zero_QDelta;
};

minus_QDelta :: QDelta -> QDelta -> QDelta;
minus_QDelta qd1 qd2 =
  QDelta (Rat.minus_rat (qdfst qd1) (qdfst qd2))
    (Rat.minus_rat (qdsnd qd1) (qdsnd qd2));

instance Arith.Minus QDelta where {
  minus = minus_QDelta;
};

uminus_QDelta :: QDelta -> QDelta;
uminus_QDelta qd =
  QDelta (Rat.uminus_rat (qdfst qd)) (Rat.uminus_rat (qdsnd qd));

instance Arith.Uminus QDelta where {
  uminus = uminus_QDelta;
};

less_eq_QDelta :: QDelta -> QDelta -> Bool;
less_eq_QDelta qd1 qd2 =
  Rat.less_rat (qdfst qd1) (qdfst qd2) ||
    Rat.equal_rat (qdfst qd1) (qdfst qd2) &&
      Rat.less_eq_rat (qdsnd qd1) (qdsnd qd2);

less_QDelta :: QDelta -> QDelta -> Bool;
less_QDelta qd1 qd2 =
  Rat.less_rat (qdfst qd1) (qdfst qd2) ||
    Rat.equal_rat (qdfst qd1) (qdfst qd2) &&
      Rat.less_rat (qdsnd qd1) (qdsnd qd2);

instance Orderings.Ord QDelta where {
  less_eq = less_eq_QDelta;
  less = less_QDelta;
};

instance Orderings.Preorder QDelta where {
};

instance Orderings.Order QDelta where {
};

instance Arith.Semigroup_add QDelta where {
};

instance Arith.Cancel_semigroup_add QDelta where {
};

instance Arith.Monoid_add QDelta where {
};

instance Arith.Group_add QDelta where {
};

instance Orderings.Linorder QDelta where {
};

instance Arith.Ab_semigroup_add QDelta where {
};

instance Arith.Cancel_ab_semigroup_add QDelta where {
};

instance Arith.Comm_monoid_add QDelta where {
};

instance Arith.Cancel_comm_monoid_add QDelta where {
};

instance Arith.Ab_group_add QDelta where {
};

scaleRat_QDelta :: Rat.Rat -> QDelta -> QDelta;
scaleRat_QDelta r qd =
  QDelta (Rat.times_rat r (qdfst qd)) (Rat.times_rat r (qdsnd qd));

instance Simplex_Algebra.ScaleRat QDelta where {
  scaleRat = scaleRat_QDelta;
};

instance Simplex_Algebra.Rational_vector QDelta where {
};

instance Simplex_Algebra.Ordered_rational_vector QDelta where {
};

instance Arith.Ordered_ab_semigroup_add QDelta where {
};

instance Simplex_Algebra.Linordered_rational_vector QDelta where {
};

instance Simplex_Algebra.Lrv QDelta where {
};

val :: QDelta -> Rat.Rat -> Rat.Rat;
val qd delta = Rat.plus_rat (qdfst qd) (Rat.times_rat delta (qdsnd qd));

delta_0 :: QDelta -> QDelta -> Rat.Rat;
delta_0 qd1 qd2 =
  let {
    c1 = qdfst qd1;
    c2 = qdfst qd2;
    k1 = qdsnd qd1;
    k2 = qdsnd qd2;
  } in (if Rat.less_rat c1 c2 && Rat.less_rat k2 k1
         then Rat.divide_rat (Rat.minus_rat c2 c1) (Rat.minus_rat k1 k2)
         else Rat.one_rat);

}
