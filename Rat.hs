{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  Rat(Rat, equal_rat, less_eq_rat, less_rat, one_rat, plus_rat, zero_rat,
       minus_rat, times_rat, divide_rat, uminus_rat, inverse_rat)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified Product_Type;
import qualified GCD;
import qualified HOL;
import qualified Arith;
import qualified Orderings;

newtype Rat = Frct (Arith.Int, Arith.Int);

quotient_of :: Rat -> (Arith.Int, Arith.Int);
quotient_of (Frct x) = x;

equal_rat :: Rat -> Rat -> Bool;
equal_rat a b = quotient_of a == quotient_of b;

instance Eq Rat where {
  a == b = equal_rat a b;
};

less_eq_rat :: Rat -> Rat -> Bool;
less_eq_rat p q =
  let {
    a = quotient_of p;
  } in (case a of {
         (aa, c) ->
           let {
             b = quotient_of q;
           } in (case b of {
                  (ba, d) ->
                    Arith.less_eq_int (Arith.times_int aa d)
                      (Arith.times_int c ba);
                });
       });

less_rat :: Rat -> Rat -> Bool;
less_rat p q =
  let {
    a = quotient_of p;
  } in (case a of {
         (aa, c) ->
           let {
             b = quotient_of q;
           } in (case b of {
                  (ba, d) ->
                    Arith.less_int (Arith.times_int aa d)
                      (Arith.times_int c ba);
                });
       });

instance Orderings.Ord Rat where {
  less_eq = less_eq_rat;
  less = less_rat;
};

normalize :: (Arith.Int, Arith.Int) -> (Arith.Int, Arith.Int);
normalize p =
  (if Arith.less_int Arith.zero_int (snd p)
    then let {
           a = GCD.gcd_int (fst p) (snd p);
         } in (Arith.divide_int (fst p) a, Arith.divide_int (snd p) a)
    else (if Arith.equal_int (snd p) Arith.zero_int
           then (Arith.zero_int, Arith.one_int)
           else let {
                  a = Arith.uminus_int (GCD.gcd_int (fst p) (snd p));
                } in (Arith.divide_int (fst p) a, Arith.divide_int (snd p) a)));

one_rat :: Rat;
one_rat = Frct (Arith.one_int, Arith.one_int);

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
                            (Arith.plus_int (Arith.times_int aa d)
                               (Arith.times_int ba c),
                              Arith.times_int c d);
                      });
             }));

zero_rat :: Rat;
zero_rat = Frct (Arith.zero_int, Arith.one_int);

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
                            (Arith.minus_int (Arith.times_int aa d)
                               (Arith.times_int ba c),
                              Arith.times_int c d);
                      });
             }));

times_rat :: Rat -> Rat -> Rat;
times_rat p q =
  Frct (let {
          a = quotient_of p;
        } in (case a of {
               (aa, c) ->
                 let {
                   b = quotient_of q;
                 } in (case b of {
                        (ba, d) ->
                          normalize
                            (Arith.times_int aa ba, Arith.times_int c d);
                      });
             }));

divide_rat :: Rat -> Rat -> Rat;
divide_rat p q =
  Frct (let {
          a = quotient_of p;
        } in (case a of {
               (aa, c) ->
                 let {
                   b = quotient_of q;
                 } in (case b of {
                        (ba, d) ->
                          normalize
                            (Arith.times_int aa d, Arith.times_int c ba);
                      });
             }));

uminus_rat :: Rat -> Rat;
uminus_rat p = Frct (let {
                       a = quotient_of p;
                     } in (case a of {
                            (aa, b) -> (Arith.uminus_int aa, b);
                          }));

inverse_rat :: Rat -> Rat;
inverse_rat p =
  Frct (let {
          a = quotient_of p;
        } in (case a of {
               (aa, b) ->
                 (if Arith.equal_int aa Arith.zero_int
                   then (Arith.zero_int, Arith.one_int)
                   else (Arith.times_int (Arith.sgn_int aa) b,
                          Arith.abs_int aa));
             }));

}
