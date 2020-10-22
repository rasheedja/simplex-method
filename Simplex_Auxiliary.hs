{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module Simplex_Auxiliary(min_satisfying) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified List;
import qualified HOL;
import qualified Orderings;

min_satisfying ::
  forall a. (Orderings.Linorder a) => (a -> Bool) -> [a] -> Maybe a;
min_satisfying p l =
  let {
    xs = filter p l;
  } in (if null xs then Nothing
         else Just (List.foldl Orderings.min (List.hd xs) (List.tl xs)));

}
