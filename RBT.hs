{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module RBT(Rbt, empty, insert, lookup, bulkload) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified RBT_Impl;
import qualified Orderings;

newtype Rbt b a = RBT (RBT_Impl.Rbt b a);

empty :: forall a b. (Orderings.Linorder a) => Rbt a b;
empty = RBT RBT_Impl.Empty;

impl_of :: forall b a. (Orderings.Linorder b) => Rbt b a -> RBT_Impl.Rbt b a;
impl_of (RBT x) = x;

insert :: forall a b. (Orderings.Linorder a) => a -> b -> Rbt a b -> Rbt a b;
insert xc xd xe = RBT (RBT_Impl.rbt_insert xc xd (impl_of xe));

lookup :: forall a b. (Orderings.Linorder a) => Rbt a b -> a -> Maybe b;
lookup x = RBT_Impl.rbt_lookup (impl_of x);

bulkload :: forall a b. (Orderings.Linorder a) => [(a, b)] -> Rbt a b;
bulkload xa = RBT (RBT_Impl.rbt_bulkload xa);

}
