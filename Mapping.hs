{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module Mapping(Mapping, empty, lookup, update, tabulate) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified Orderings;
import qualified RBT;

newtype Mapping a b = Mapping (RBT.Rbt a b);

empty :: forall a b. (Orderings.Linorder a) => Mapping a b;
empty = Mapping RBT.empty;

lookup :: forall a b. (Orderings.Linorder a) => Mapping a b -> a -> Maybe b;
lookup (Mapping t) = RBT.lookup t;

update ::
  forall a b. (Orderings.Linorder a) => a -> b -> Mapping a b -> Mapping a b;
update k v (Mapping t) = Mapping (RBT.insert k v t);

tabulate ::
  forall a b. (Orderings.Linorder a) => [a] -> (a -> b) -> Mapping a b;
tabulate ks f = Mapping (RBT.bulkload (map (\ k -> (k, f k)) ks));

}
