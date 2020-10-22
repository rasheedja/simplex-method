{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  Finite_Map(Fmap, fmdom, fmupd, fmdrop, fmempty, fmmap, fmlookup, equal_fmap)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified Map;
import qualified Option;
import qualified Product_Type;
import qualified FSet;
import qualified AList;

newtype Fmap a b = Fmap_of_list [(a, b)];

fmadd :: forall a b. (Eq a) => Fmap a b -> Fmap a b -> Fmap a b;
fmadd (Fmap_of_list m) (Fmap_of_list n) = Fmap_of_list (AList.merge m n);

fmdom :: forall a b. Fmap a b -> FSet.Fset a;
fmdom (Fmap_of_list m) = FSet.fimage fst (FSet.fset_of_list m);

fmupd :: forall a b. (Eq a) => a -> b -> Fmap a b -> Fmap a b;
fmupd k v m = fmadd m (Fmap_of_list [(k, v)]);

fmfilter :: forall a b. (a -> Bool) -> Fmap a b -> Fmap a b;
fmfilter p (Fmap_of_list m) = Fmap_of_list (filter (\ (k, _) -> p k) m);

fmdrop :: forall a b. (Eq a) => a -> Fmap a b -> Fmap a b;
fmdrop a = fmfilter (\ aa -> not (aa == a));

fmempty :: forall a b. Fmap a b;
fmempty = Fmap_of_list [];

fmmap :: forall a b c. (a -> b) -> Fmap c a -> Fmap c b;
fmmap f (Fmap_of_list m) = Fmap_of_list (map (Product_Type.apsnd f) m);

fmlookup :: forall a b. (Eq a) => Fmap a b -> a -> Maybe b;
fmlookup (Fmap_of_list m) = Map.map_of m;

fmrel ::
  forall a b c. (Eq c) => (a -> b -> Bool) -> Fmap c a -> Fmap c b -> Bool;
fmrel r m n =
  FSet.fBall (fmdom m)
    (\ x -> Option.rel_option r (fmlookup m x) (fmlookup n x)) &&
    FSet.fBall (fmdom n)
      (\ x -> Option.rel_option r (fmlookup m x) (fmlookup n x));

equal_fmap :: forall a b. (Eq a, Eq b) => Fmap a b -> Fmap a b -> Bool;
equal_fmap = fmrel (\ a b -> a == b);

}
