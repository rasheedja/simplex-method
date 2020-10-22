{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module FSet(Fset, fset, fBall, fimage, fset_of_list) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified Set;

newtype Fset a = Abs_fset (Set.Set a);

fset :: forall a. Fset a -> Set.Set a;
fset (Abs_fset x) = x;

fBall :: forall a. Fset a -> (a -> Bool) -> Bool;
fBall xa = Set.ball (fset xa);

fimage :: forall b a. (b -> a) -> Fset b -> Fset a;
fimage xb xc = Abs_fset (Set.image xb (fset xc));

fset_of_list :: forall a. [a] -> Fset a;
fset_of_list xa = Abs_fset (Set.Set xa);

}
