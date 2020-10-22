{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  Simplex_Algebra(ScaleRat(..), Rational_vector, Ordered_rational_vector,
                   Linordered_rational_vector, Lrv)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified Rat;
import qualified Orderings;
import qualified Arith;

class ScaleRat a where {
  scaleRat :: Rat.Rat -> a -> a;
};

class (Arith.Ab_group_add a, ScaleRat a) => Rational_vector a where {
};

class (Orderings.Order a,
        Rational_vector a) => Ordered_rational_vector a where {
};

class (Arith.Ordered_ab_semigroup_add a, Orderings.Linorder a,
        Ordered_rational_vector a) => Linordered_rational_vector a where {
};

class (Arith.One a, Linordered_rational_vector a) => Lrv a where {
};

}
