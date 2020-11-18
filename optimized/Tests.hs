module Tests where

import Util as Util
import Simplex

runTestsList =
  [optSimplex test1 0 Max,
  optSimplex test2 0 Min]

test1 =
  [
    Util.EQ ([(0, 1), (1, 0-2), (2, 0-1)]) 0,
    Util.LEQ ([(0, 0), (1, 4), (2, 1)]) 150,
    Util.LEQ ([(0, 1), (1, 2), (2, 0-3)]) (0-40),
    Util.GEQ ([(1, 1)]) 0,
    Util.GEQ ([(2, 1)]) 0
  ]

test2 =
  [
    Util.EQ [(0, (0-1)), (1, (0-6)), (2, (0-4)), (3, 2)] 0,
    Util.LEQ [(0, 0), (1, 1), (2, 1), (3, 4)] 20,
    Util.LEQ [(0, 0), (1, 0), (2, (0-5)), (3, 5)] 100,
    Util.LEQ [(0, 0), (1, 1), (2, 3), (3, 1)] 400,
    Util.GEQ [(1, 1)] 0,
    Util.GEQ [(2, 1)] 0,
    Util.GEQ [(3, 1)] 0
  ]

test3 =
  [
    Util.EQ [(0, 1), (1, -3), (2, -5)] 0,
    Util.LEQ [(1, 3), (2, 1)] 15,
    Util.LEQ [(1, 1), (2, 1)] 7,
    Util.LEQ [(2, 1)] 4,
    Util.LEQ [(1, -1), (2, 2)] 6
  ]

-- Prelude testing

test3slack =
  [
    Util.EQ [(0, 1), (1, -3), (2, -5)] 0,
    Util.EQ [(1, 3), (2, 1), (3, 1)] 15,
    Util.EQ [(1, 1), (2, 1), (4, 1)] 7,
    Util.EQ [(2, 1), (5, 1)] 4,
    Util.EQ [(1, -1), (2, 2), (6, 1)] 6
  ]


-- Vals
-- 0 = 14
-- 1 = -2
-- 2 = 4
-- 3 = 17
-- 4 = 5
-- 5 = 0
-- 6 = 0

-- test3phase1 :: [PolyConstraint]
-- test3phase1 =
--   [
--     Util.EQ [(0, 1), (1, -3), (2, -5)] 0,
--     Util.EQ [(1, 3), (2, 1), (3, 1)] 15,
--     Util.EQ [(1, 1), (2, 1), (4, 1)] 7,
--     Util.EQ [(2, 1), (5, 1)] 4,
--     Util.EQ [(1, -1), (2, 1), (6, 1)] 6
--   ]
  
-- test3tableau :: [(Nat, Linear_poly)]
-- test3tableau =
--   [
--     (Nat 0, LinearPoly (Fmap_of_list [(Nat 0, 1), (Nat 1, -3), (Nat 2, -5)])),
--     (Nat 3, LinearPoly (Fmap_of_list [(Nat 1, 3), (Nat 2, 1), (Nat 3, 1)])),
--     (Nat 4, LinearPoly (Fmap_of_list [(Nat 1, 1), (Nat 2, 1), (Nat 4, 1)])),
--     (Nat 5, LinearPoly (Fmap_of_list [(Nat 2, 1), (Nat 5, 1)])),
--     (Nat 6, LinearPoly (Fmap_of_list [(Nat 1, -1), (Nat 2, 2), (Nat 6, 1)]))
--   ]

-- test3tableau :: [(Nat, Linear_poly)]
-- test3tableau =
--   [
--     (Nat 0, LinearPoly (Fmap_of_list [(Nat 1, -3), (Nat 2, -5)])),
--     (Nat 3, LinearPoly (Fmap_of_list [(Nat 1, 3), (Nat 2, 1)])),
--     (Nat 4, LinearPoly (Fmap_of_list [(Nat 1, 1), (Nat 2, 1)])),
--     (Nat 5, LinearPoly (Fmap_of_list [(Nat 2, 1)])),
--     (Nat 6, LinearPoly (Fmap_of_list [(Nat 1, -1), (Nat 2, 2)]))
--   ]

-- test3tableau :: [(Nat, Linear_poly)]
-- test3tableau =
--   [
--     (Nat 0, LinearPoly (Fmap_of_list [(Nat 1, 3), (Nat 2, 5)])),
--     (Nat 3, LinearPoly (Fmap_of_list [(Nat 1, -3), (Nat 2, -1)])),
--     (Nat 4, LinearPoly (Fmap_of_list [(Nat 1, -1), (Nat 2, -1)])),
--     (Nat 5, LinearPoly (Fmap_of_list [(Nat 2, -1)])),
--     (Nat 6, LinearPoly (Fmap_of_list [(Nat 1, 1), (Nat 2, -2)]))
--   ]

-- test3tableau :: [(Nat, Linear_poly)]
-- test3tableau =
--   [
--     (Nat 0, LinearPoly (Fmap_of_list [(Nat 0, 2), (Nat 1, -3), (Nat 2, -5)])),
--     (Nat 3, LinearPoly (Fmap_of_list [(Nat 1, 3), (Nat 2, 1), (Nat 3, 2)])),
--     (Nat 4, LinearPoly (Fmap_of_list [(Nat 1, 1), (Nat 2, 1), (Nat 4, 2)])),
--     (Nat 5, LinearPoly (Fmap_of_list [(Nat 2, 1), (Nat 5, 2)])),
--     (Nat 6, LinearPoly (Fmap_of_list [(Nat 1, -1), (Nat 2, 2), (Nat 6, 2)]))
--   ]

-- TODO: two ideas
-- Idea one, instead of putting 2 for the basic variables, try to negate the entire row and remove the basic vars from the RHS. This works
-- Idea two, add the RHS column from the book as a new variable. Ensure that these variables cannot be pivoted. This also works. Use (Nat (-1), Val)

-- test3tableau :: [(Nat, Linear_poly)]
-- test3tableau =
--   [
--     (Nat 0, LinearPoly (Fmap_of_list [(Nat 0, 2), (Nat 1, -3), (Nat 2, -5)])),
--     (Nat 3, LinearPoly (Fmap_of_list [(Nat 1, 3), (Nat 2, 1), (Nat 3, 2)])),
--     (Nat 4, LinearPoly (Fmap_of_list [(Nat 1, 1), (Nat 2, 1), (Nat 4, 2)])),
--     (Nat 5, LinearPoly (Fmap_of_list [(Nat 2, 1), (Nat 5, 2)])),
--     (Nat 6, LinearPoly (Fmap_of_list [(Nat 1, -1), (Nat 2, 2), (Nat 6, 2)]))
--   ]

test3tableau :: [(Nat, Linear_poly)]
test3tableau =
  [
    (Nat 0, LinearPoly (Fmap_of_list [(Nat 1, 3), (Nat 2, 5), (Nat (-1), 0)])),
    (Nat 3, LinearPoly (Fmap_of_list [(Nat 1, -3), (Nat 2, -1), (Nat (-1), -15)])),
    (Nat 4, LinearPoly (Fmap_of_list [(Nat 1, -1), (Nat 2, -1), (Nat (-1), -7)])),
    (Nat 5, LinearPoly (Fmap_of_list [(Nat 2, -1), (Nat (-1), -4)])),
    (Nat 6, LinearPoly (Fmap_of_list [(Nat 1, 1), (Nat 2, -2), (Nat (-1), -6)]))
  ]



-- test3vals :: Mapping Nat Rational
-- test3vals = final
--   where
--     init = Mapping (RBT (Empty))
--     p1 = updatea (Nat 0) 0 init
--     p2 = updatea (Nat 3) 15 p1
--     p3 = updatea (Nat 4) 7 p2
--     p4 = updatea (Nat 5) 4 p3
--     final = updatea (Nat 6) 0 p4

test3slackVals = (Mapping (RBT (Branch B (Branch B (Branch B Empty (Nat 0) (26 ) Empty) (Nat 1) (2 ) (Branch B Empty (Nat 2) (4 ) Empty)) (Nat 3) (5 ) (Branch B (Branch B Empty (Nat 4) (1 ) Empty) (Nat 5) (0 ) (Branch B Empty (Nat 6) (0 ) Empty)))))

test3WithNoBasicVars =
  [
    Util.EQ [(0,1),(1,(-3)),(2,(-5))] (0),
    Util.EQ [(1,3),(2,1),(3,1)] (15),
    Util.EQ [(1,1),(2,1),(4,1)] (7),
    Util.EQ [(2,1),(5,1)] (4),
    Util.EQ [(1,(-1)),(2,2),(6,1)] (6)
  ]

test3WithPhase1BasicVars =
  [
    (0,Util.EQ [(0,1),(1,(-3)),(2,(-5))] (0)),
    (3,Util.EQ [(1,3),(2,1),(3,1)] (15)),
    (1,Util.EQ [(1,1),(2,1),(4,1)] (7)),
    (2,Util.EQ [(2,1),(5,1)] (4)),
    (6,Util.EQ [(1,(-1)),(2,2),(6,1)] (6))
  ]

simpleTestTable =
  [
    (0,Util.EQ [(0,1)] (2)),
    (1,Util.EQ [(0,1), (1,2)] (15))
  ]

-- This problem will not be feasible if we just add slack vars
test4 =
  [
    Util.LEQ [(0, 2), (1, -1), (2, 2)] 4,
    Util.LEQ [(0, 2), (1, -3), (2, 1)] (-5),
    Util.LEQ [(0, -1), (1, 1), (2, -2)] (-1),
    Util.GEQ [(0, 1)] 0,
    Util.GEQ [(1, 1)] 0,
    Util.GEQ [(2, 1)] 0
  ]

-- test3 =
--   [
--     Util.EQ [(0, 1), (1, -3), (2, -5)] 0,
--     Util.LEQ [(1, 3), (2, 1)] 15,
--     Util.LEQ [(1, 1), (2, 1)] 7,
--     Util.LEQ [(2, 1)] 4,
--     Util.LEQ [(1, -1), (2, 2)] 6
--   ]

-- test with formula to maximize
test5 :: ([(Integer, Rational)], [PolyConstraint])
test5 =
  (
    [(0, 1), (1, -3), (2, -5)],
    [
      Util.LEQ [(1, 3), (2, 1)] 15,
      Util.LEQ [(1, 1), (2, 1)] 7,
      Util.LEQ [(2, 1)] 4,
      Util.LEQ [(1, -1), (2, 2)] 6
    ]
  )

-- basic vars
-- 0 = 26
-- 1 = 2
-- 2 = 4
-- 3 = 5
-- 4 = 1
-- non-basic vars
-- 5 = 0
-- 6 = 0

-- test3tableau :: [(Nat, Linear_poly)]
-- test3tableau =
--   [
--     (Nat 0, LinearPoly (Fmap_of_list [(Nat 0, 1), (Nat 1, -3), (Nat 2, -5)])),
--     (Nat 3, LinearPoly (Fmap_of_list [(Nat 1, 3), (Nat 2, 1), (Nat 3, 1)])),
--     (Nat 4, LinearPoly (Fmap_of_list [(Nat 1, 1), (Nat 2, 1), (Nat 4, 1)])),
--     (Nat 5, LinearPoly (Fmap_of_list [(Nat 2, 1), (Nat 5, 1)])),
--     (Nat 6, LinearPoly (Fmap_of_list [(Nat 1, -1), (Nat 2, 2), (Nat 6, 1)]))
--   ]

-- zip (map fst (fst row)) $ map ((/c) . snd) (fst row)

-- test3GeneratedWithAddedBasicVars :: [(Integer, PolyConstraint)]
-- test3GeneratedWithAddedBasicVars = fixedHead : tail indexed
--   where
--     fixedHead = (0, snd (head indexed))
--     indexed = zip [2..] rhs
--     rhs = systemInStandardFormToTableau $ systemInStandardForm test3

test3GeneratedButManuallyRemovedBasicVarsLHS = 
  [
    (0,Util.EQ [(1,(-3)),(2,(-5))] (0)),
    (3,Util.EQ [(1,3),(2,1)] (15)),
    (4,Util.EQ [(1,1),(2,1)] (7)),
    (5,Util.EQ [(2,1)] (4)),
    (6,Util.EQ [(1,(-1)),(2,2)] (6))]

-- So if we plug in values we get from the simplex algo, we ge the same value on the RHS
-- Makes sense, the algo is just finding values which fit the current valuation

test6 :: ([(Integer, Rational)], [PolyConstraint])
test6 =
  (
    [(1, 1), (2, -1), (3, 1)],
    [
      Util.LEQ [(1, 2), (2, -1), (3, 2)] 4,
      Util.LEQ [(1, 2), (2, -3), (3, 1)] (-5),
      Util.LEQ [(1, -1), (2, 1), (3, -2)] (-1)
    ]
  )

cs = map polyConstraintToConstraint test3slack

ics = (zip (upt zero_nat (size_list cs)) cs)
icsa = to_ns ics
(t1, (asa, (trans_v, []))) = preprocess icsa
Inr v = assert_all_code t1 asa

-- (trans_vv, Inr state1) = simplex_index_phase1 ics
-- Update function updates the RHS of a tableau and updates the LHS to ensure the tableau is satisfiable
-- Rest of RHS is unchanged

-- Pivoting swaps lhs with rhs (basic and nonbasic). If we want to update some variable in the lhs, we call this

-- data State a b =
  -- State [(Nat, Linear_poly)] (Mapping Nat (a, b)) (Mapping Nat (a, b))
    -- (Mapping Nat b) Bool (Maybe [a]) deriving Prelude.Show;
-- data State a b = tableau, lowerBounds, upperBounds, candidate value, unsat, unsatVals? 


-- Modify phase 2 stuff to work on the following form
-- (
--   formulaToMaximize :: [(Integer, Rational)]
--   system
-- )

-- Modify the system in standard form to return the slack vars added
-- Introduce a final artificial variable equal to formulaToMaximize, add this to the first row of the tableau
-- If any slack vars/artificial var are equal to a negative number, perform phase 1 of the simplex method
-- Can use the original simplex before doing all this to check if the system is satisfiable
