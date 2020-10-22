{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module GCD(gcd_int) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified Arith;

gcd_int :: Arith.Int -> Arith.Int -> Arith.Int;
gcd_int (Arith.Int_of_integer x) (Arith.Int_of_integer y) =
  Arith.Int_of_integer (Prelude.gcd x y);

}
