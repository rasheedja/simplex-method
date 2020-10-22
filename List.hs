{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  List(nth, upt, find, foldl, foldr, member, hd, tl, remdups, size_list,
        sorted_list_of_set)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified Set;
import qualified Multiset;
import qualified Product_Type;
import qualified HOL;
import qualified Orderings;
import qualified Arith;

nth :: forall a. [a] -> Arith.Nat -> a;
nth (x : xs) n =
  (if Arith.equal_nat n Arith.zero_nat then x
    else nth xs (Arith.minus_nat n Arith.one_nat));

upt :: Arith.Nat -> Arith.Nat -> [Arith.Nat];
upt i j = (if Arith.less_nat i j then i : upt (Arith.suc i) j else []);

find :: forall a. (a -> Bool) -> [a] -> Maybe a;
find uu [] = Nothing;
find p (x : xs) = (if p x then Just x else find p xs);

foldl :: forall a b. (a -> b -> a) -> a -> [b] -> a;
foldl f a [] = a;
foldl f a (x : xs) = foldl f (f a x) xs;

foldr :: forall a b. (a -> b -> b) -> [a] -> b -> b;
foldr f [] = id;
foldr f (x : xs) = f x . foldr f xs;

member :: forall a. (Eq a) => [a] -> a -> Bool;
member [] y = False;
member (x : xs) y = x == y || member xs y;

hd :: forall a. [a] -> a;
hd (x21 : x22) = x21;

tl :: forall a. [a] -> [a];
tl [] = [];
tl (x21 : x22) = x22;

remdups :: forall a. (Eq a) => [a] -> [a];
remdups [] = [];
remdups (x : xs) = (if member xs x then remdups xs else x : remdups xs);

gen_length :: forall a. Arith.Nat -> [a] -> Arith.Nat;
gen_length n (x : xs) = gen_length (Arith.suc n) xs;
gen_length n [] = n;

size_list :: forall a. [a] -> Arith.Nat;
size_list = gen_length Arith.zero_nat;

sort_key :: forall a b. (Orderings.Linorder b) => (a -> b) -> [a] -> [a];
sort_key f xs =
  (case xs of {
    [] -> [];
    [_] -> xs;
    [x, y] -> (if Orderings.less_eq (f x) (f y) then xs else [y, x]);
    _ : _ : _ : _ ->
      (case Multiset.part f
              (f (nth xs
                   (Arith.divide_nat (size_list xs)
                     (Arith.nat_of_integer (2 :: Integer)))))
              xs
        of {
        (lts, (eqs, gts)) -> sort_key f lts ++ eqs ++ sort_key f gts;
      });
  });

sorted_list_of_set ::
  forall a. (Eq a, Orderings.Linorder a) => Set.Set a -> [a];
sorted_list_of_set (Set.Set xs) = sort_key (\ x -> x) (remdups xs);

}
