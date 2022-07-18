module TestFunctions where

import Prelude hiding (EQ)
import Linear.Simplex.Types
import Data.Ratio

testsList :: [((ObjectiveFunction, [PolyConstraint]), Maybe (Integer, [(Integer, Rational)]))]
testsList =
  [
      (test1,                    Just (7,[(7,29 % 1),(1,3 % 1),(2,4 % 1)]))
    , (test2,                    Just (7,[(7,0 % 1)]))
    , (test3,                    Nothing)
    , (test4,                    Just (11,[(11,237 % 7),(1,24 % 7),(2,33 % 7)]))
    , (test5,                    Just (9,[(9,3 % 5),(2,14 % 5),(3,17 % 5)]))
    , (test6,                    Nothing)
    , (test7,                    Just (8,[(8,1 % 1),(2,2 % 1),(1,3 % 1)]))
    , (test8,                    Just (8,[(8,(-1) % 4),(2,9 % 2),(1,17 % 4)]))
    , (test9,                    Just (7,[(7,5 % 1),(3,2 % 1),(4,1 % 1)]))
    , (test10,                   Just (7,[(7,8 % 1),(1,2 % 1),(2,6 % 1)]))
    , (test11,                   Just (8,[(8,20 % 1),(4,16 % 1),(3,6 % 1)]))
    , (test12,                   Just (8,[(8,6 % 1),(4,2 % 1),(5,2 % 1)]))
    , (test13,                   Just (6,[(6,150 % 1),(2,150 % 1)]))
    , (test14,                   Just (6,[(6,40 % 3),(2,40 % 3)]))
    , (test15,                   Nothing)
    , (test16,                   Just (6,[(6,75 % 1),(1,75 % 2)]))
    , (test17,                   Just (7,[(7,(-120) % 1),(1,20 % 1)]))
    , (test18,                   Just (7,[(7,10 % 1),(3,5 % 1)]))
    , (test19,                   Nothing)
    , (test20,                   Nothing)
    , (test21,                   Just (7,[(7,250 % 1),(2,50 % 1)]))
    , (test22,                   Just (7,[(7,0 % 1)]))
    , (test23,                   Nothing)
    , (test24,                   Just (10,[(10,300 % 1),(3,150 % 1)]))
    , (test25,                   Just (3,[(3,15 % 1),(1,15 % 1)]))
    , (test26,                   Just (6,[(6,20 % 1),(1,10 % 1),(2,10 % 1)]))
    , (test27,                   Just (3,[(3,0 % 1)]))
    , (test28,                   Just (6,[(6,0 % 1),(2,10 % 1)]))
    , (test29,                   Nothing)
    , (test30,                   Nothing)
    , (testPolyPaver1,           Just (12,[(12,7 % 4),(2,5 % 2),(1,7 % 4),(3,0 % 1)]))
    , (testPolyPaver2,           Just (12,[(12,5 % 2),(2,5 % 3),(1,5 % 2),(3,0 % 1)]))
    , (testPolyPaver3,           Just (12,[(12,5 % 3),(2,5 % 3),(1,5 % 2),(3,0 % 1)]))
    , (testPolyPaver4,           Just (12,[(12,5 % 2),(2,5 % 2),(1,5 % 2),(3,0 % 1)]))
    , (testPolyPaver5,           Nothing)
    , (testPolyPaver6,           Nothing)
    , (testPolyPaver7,           Nothing)
    , (testPolyPaver8,           Nothing)
    , (testPolyPaver9,           Just (12,[(12,7 % 2),(2,5 % 9),(1,7 % 2),(3,0 % 1)]))
    , (testPolyPaver10,          Just (12,[(12,17 % 20),(2,7 % 2),(1,17 % 20),(3,0 % 1)]))
    , (testPolyPaver11,          Just (12,[(12,7 % 2),(2,7 % 2),(1,22 % 9)]))
    , (testPolyPaver12,          Just (12,[(12,5 % 9),(2,5 % 9),(1,7 % 2),(3,0 % 1)]))
    , (testPolyPaverTwoFs1,      Nothing)
    , (testPolyPaverTwoFs2,      Nothing)
    , (testPolyPaverTwoFs3,      Nothing)
    , (testPolyPaverTwoFs4,      Nothing)
    , (testPolyPaverTwoFs5,      Just (17,[(17,5 % 2),(2,45 % 22),(1,5 % 2),(4,0 % 1)]))
    , (testPolyPaverTwoFs6,      Just (17,[(17,45 % 22),(2,5 % 2),(1,45 % 22),(4,0 % 1)]))
    , (testPolyPaverTwoFs7,      Just (17,[(17,5 % 2),(2,5 % 2),(1,5 % 2),(4,0 % 1)]))
    , (testPolyPaverTwoFs8,      Just (17,[(17,45 % 22),(2,45 % 22),(1,5 % 2),(4,0 % 1)]))
    , (testLeqGeqBugMin1,        Just (5,[(5,3 % 1),(1,3 % 1),(2,3 % 1)]))
    , (testLeqGeqBugMax1,        Just (5,[(5,3 % 1),(1,3 % 1),(2,3 % 1)]))
    , (testLeqGeqBugMin2,        Just (5,[(5,3 % 1),(1,3 % 1),(2,3 % 1)]))
    , (testLeqGeqBugMax2,        Just (5,[(5,3 % 1),(1,3 % 1),(2,3 % 1)]))
    , (testQuickCheck1,          Just (10,[(10,(-370) % 1),(2,26 % 1),(1,5 % 3)]))
    , (testQuickCheck2,          Just (8,[(8,(-2) % 9),(1,14 % 9),(2,8 % 9)]))
    , (testQuickCheck3,          Just (7,[(7,(-8) % 1),(2,2 % 1)]))
  ]

testLeqGeqBugMin1 =
  (
    Min [(1, 1)],
    [
      GEQ [(1,1 % 1)] (3 % 1),
      LEQ [(1,1 % 1)] (3 % 1),
      GEQ [(2,1 % 1)] (3 % 1),
      LEQ [(2,1 % 1)] (3 % 1)
    ]
  )
  
testLeqGeqBugMax1 =
  (
    Min [(1, 1)],
    [
      GEQ [(1,1 % 1)] (3 % 1),
      LEQ [(1,1 % 1)] (3 % 1),
      GEQ [(2,1 % 1)] (3 % 1),
      LEQ [(2,1 % 1)] (3 % 1)
    ]
  )

testLeqGeqBugMin2 =
  (
    Min [(1, 1)],
    [
      GEQ [(1,1 % 1)] (3 % 1),
      LEQ [(1,1 % 1)] (3 % 1),
      GEQ [(2,1 % 1)] (3 % 1),
      LEQ [(2,1 % 1)] (3 % 1)
    ]
  )
  
testLeqGeqBugMax2 =
  (
    Min [(1, 1)],
    [
      GEQ [(1,1 % 1)] (3 % 1),
      LEQ [(1,1 % 1)] (3 % 1),
      GEQ [(2,1 % 1)] (3 % 1),
      LEQ [(2,1 % 1)] (3 % 1)
    ]
  )

-- From page 50 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 29, 1 = 3, 2 = 4, 
test1 :: (ObjectiveFunction, [PolyConstraint])
test1 =
  (
    Max [(1, 3), (2, 5)],
    [
      LEQ [(1, 3), (2, 1)] 15,
      LEQ [(1, 1), (2, 1)] 7,
      LEQ [(2, 1)] 4,
      LEQ [(1, -1), (2, 2)] 6
    ]
  )

test2 :: (ObjectiveFunction, [PolyConstraint])
test2 =
  (
    Min [(1, 3), (2, 5)],
    [
      LEQ [(1, 3), (2, 1)] 15,
      LEQ [(1, 1), (2, 1)] 7,
      LEQ [(2, 1)] 4,
      LEQ [(1, -1), (2, 2)] 6
    ]
  )

test3 :: (ObjectiveFunction, [PolyConstraint])
test3 =
  (
    Max [(1, 3), (2, 5)],
    [
      GEQ [(1, 3), (2, 1)] 15,
      GEQ [(1, 1), (2, 1)] 7,
      GEQ [(2, 1)] 4,
      GEQ [(1, -1), (2, 2)] 6
    ]
  )

test4 :: (ObjectiveFunction, [PolyConstraint])
test4 =
  (
    Min [(1, 3), (2, 5)],
    [
      GEQ [(1, 3), (2, 1)] 15,
      GEQ [(1, 1), (2, 1)] 7,
      GEQ [(2, 1)] 4,
      GEQ [(1, -1), (2, 2)] 6
    ]
  )

-- From https://www.eng.uwaterloo.ca/~syde05/phase1.pdf
-- Solution: obj = 3/5, 2 = 14/5, 3 = 17/5
-- requires two phases
test5 :: (ObjectiveFunction, [PolyConstraint])
test5 =
  (
    Max [(1, 1), (2, -1), (3, 1)],
    [
      LEQ [(1, 2), (2, -1), (3, 2)] 4,
      LEQ [(1, 2), (2, -3), (3, 1)] (-5),
      LEQ [(1, -1), (2, 1), (3, -2)] (-1)
    ]
  )

test6 :: (ObjectiveFunction, [PolyConstraint])
test6 =
  (
    Min [(1, 1), (2, -1), (3, 1)],
    [
      LEQ [(1, 2), (2, -1), (3, 2)] 4,
      LEQ [(1, 2), (2, -3), (3, 1)] (-5),
      LEQ [(1, -1), (2, 1), (3, -2)] (-1)
    ]
  )
test7 :: (ObjectiveFunction, [PolyConstraint])
test7 =
  (
    Max [(1, 1), (2, -1), (3, 1)],
    [
      GEQ [(1, 2), (2, -1), (3, 2)] 4,
      GEQ [(1, 2), (2, -3), (3, 1)] (-5),
      GEQ [(1, -1), (2, 1), (3, -2)] (-1)
    ]
  )
test8 :: (ObjectiveFunction, [PolyConstraint])
test8 =
  (
    Min [(1, 1), (2, -1), (3, 1)],
    [
      GEQ [(1, 2), (2, -1), (3, 2)] 4,
      GEQ [(1, 2), (2, -3), (3, 1)] (-5),
      GEQ [(1, -1), (2, 1), (3, -2)] (-1)
    ]
  )

-- From page 49 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = -5, 3 = 2, 4 = 1, objVar was negated so actual val is 5 wa
-- requires two phases
test9 :: (ObjectiveFunction, [PolyConstraint])
test9 =
  (
    Min [(1, 1), (2, 1), (3, 2), (4, 1)],
    [
      EQ [(1, 1), (3, 2), (4, -2)] 2,
      EQ [(2, 1), (3, 1), (4, 4)] 6
    ]
  )

test10 :: (ObjectiveFunction, [PolyConstraint])
test10 =
  (
    Max [(1, 1), (2, 1), (3, 2), (4, 1)],
    [
      EQ [(1, 1), (3, 2), (4, -2)] 2,
      EQ [(2, 1), (3, 1), (4, 4)] 6
    ]
  )

-- Adapted from page 52 of 'Linear and Integer Programming Made Easy'
-- Removed variables which do not appear in the system (these should be artificial variables)
-- Solution: obj = 20, 3 = 6, 4 = 16 wq
test11 :: (ObjectiveFunction, [PolyConstraint])
test11 =
  (
    Max [(3, -2), (4, 2), (5, 1)],
    [
      EQ [(3, -2), (4, 1), (5, 1)] 4,
      EQ [(3, 3), (4, -1), (5, 2)] 2
    ]
  )

test12 :: (ObjectiveFunction, [PolyConstraint])
test12 =
  (
    Min [(3, -2), (4, 2), (5, 1)],
    [
      EQ [(3, -2), (4, 1), (5, 1)] 4,
      EQ [(3, 3), (4, -1), (5, 2)] 2
    ]
  )

-- From page 59 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 150, 1 = 0, 2 = 150
-- requires two phases
test13 :: (ObjectiveFunction, [PolyConstraint])
test13 =
  (
    Max [(1, 2), (2, 1)],
    [
      LEQ [(1, 4), (2, 1)] 150,
      LEQ [(1, 2), (2, -3)] (-40)
    ]
  )

test14 :: (ObjectiveFunction, [PolyConstraint])
test14 =
  (
    Min [(1, 2), (2, 1)],
    [
      LEQ [(1, 4), (2, 1)] 150,
      LEQ [(1, 2), (2, -3)] (-40)
    ]
  )

test15 :: (ObjectiveFunction, [PolyConstraint])
test15 =
  (
    Max [(1, 2), (2, 1)],
    [
      GEQ [(1, 4), (2, 1)] 150,
      GEQ [(1, 2), (2, -3)] (-40)
    ]
  )

test16 :: (ObjectiveFunction, [PolyConstraint])
test16 =
  (
    Min [(1, 2), (2, 1)],
    [
      GEQ [(1, 4), (2, 1)] 150,
      GEQ [(1, 2), (2, -3)] (-40)
    ]
  )

-- From page 59 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 120, 1 = 20, 2 = 0, 3 = 0, objVar was negated so actual val is -120
test17 :: (ObjectiveFunction, [PolyConstraint])
test17 =
  (
    Min [(1, -6), (2, -4), (3, 2)],
    [
      LEQ [(1, 1), (2, 1), (3, 4)] 20,
      LEQ [(2, -5), (3, 5)] 100,
      LEQ [(1, 1), (3, 1), (1, 1)] 400
    ]
  )

test18 :: (ObjectiveFunction, [PolyConstraint])
test18 =
  (
    Max [(1, -6), (2, -4), (3, 2)],
    [
      LEQ [(1, 1), (2, 1), (3, 4)] 20,
      LEQ [(2, -5), (3, 5)] 100,
      LEQ [(1, 1), (3, 1), (1, 1)] 400
    ]
  )

test19 :: (ObjectiveFunction, [PolyConstraint])
test19 =
  (
    Min [(1, -6), (2, -4), (3, 2)],
    [
      GEQ [(1, 1), (2, 1), (3, 4)] 20,
      GEQ [(2, -5), (3, 5)] 100,
      GEQ [(1, 1), (3, 1), (1, 1)] 400
    ]
  )

test20 :: (ObjectiveFunction, [PolyConstraint])
test20 =
  (
    Max [(1, -6), (2, -4), (3, 2)],
    [
      GEQ [(1, 1), (2, 1), (3, 4)] 20,
      GEQ [(2, -5), (3, 5)] 100,
      GEQ [(1, 1), (3, 1), (1, 1)] 400
    ]
  )

-- From page 59 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 250, 1 = 0, 2 = 50, 3 = 0
test21 :: (ObjectiveFunction, [PolyConstraint])
test21 =
  (
    Max [(1, 3), (2, 5), (3, 2)],
    [
      LEQ [(1, 5), (2, 1), (3, 4)] 50,
      LEQ [(1, 1), (2, -1), (3, 1)] 150,
      LEQ [(1, 2), (2, 1), (3, 2)] 100
    ]
  )

test22 :: (ObjectiveFunction, [PolyConstraint])
test22 =
  (
    Min [(1, 3), (2, 5), (3, 2)],
    [
      LEQ [(1, 5), (2, 1), (3, 4)] 50,
      LEQ [(1, 1), (2, -1), (3, 1)] 150,
      LEQ [(1, 2), (2, 1), (3, 2)] 100
    ]
  )

test23 :: (ObjectiveFunction, [PolyConstraint])
test23 =
  (
    Max [(1, 3), (2, 5), (3, 2)],
    [
      GEQ [(1, 5), (2, 1), (3, 4)] 50,
      GEQ [(1, 1), (2, -1), (3, 1)] 150,
      GEQ [(1, 2), (2, 1), (3, 2)] 100
    ]
  )
  
test24 :: (ObjectiveFunction, [PolyConstraint])
test24 =
  (
    Min [(1, 3), (2, 5), (3, 2)],
    [
      GEQ [(1, 5), (2, 1), (3, 4)] 50,
      GEQ [(1, 1), (2, -1), (3, 1)] 150,
      GEQ [(1, 2), (2, 1), (3, 2)] 100
    ]
  )

test25 :: (ObjectiveFunction, [PolyConstraint])
test25 =
  (
    Max [(1, 1)],
    [
      LEQ [(1, 1)] 15
    ]
  )

test26 :: (ObjectiveFunction, [PolyConstraint])
test26 =
  (
    Max [(1, 2)],
    [
      LEQ [(1, 2)] 20,
      GEQ [(2, 1)] 10
    ]
  )

test27 :: (ObjectiveFunction, [PolyConstraint])
test27 =
  (
    Min [(1, 1)],
    [
      LEQ [(1, 1)] 15
    ]
  )

test28 :: (ObjectiveFunction, [PolyConstraint])
test28 =
  (
    Min [(1, 2)],
    [
      LEQ [(1, 2)] 20,
      GEQ [(2, 1)] 10
    ]
  )
  
test29 :: (ObjectiveFunction, [PolyConstraint])
test29 =
    (
    Max [(1, 1)],
    [
      LEQ [(1, 1)] 15,
      GEQ [(1, 1)] 15.01
    ]
  )

test30 :: (ObjectiveFunction, [PolyConstraint])
test30 =
    (
    Max [(1, 1)],
    [
      LEQ [(1, 1)] 15,
      GEQ [(1, 1)] 15.01,
      GEQ [(2, 1)] 10
    ]
  )

-- Tests for systems similar to those from PolyPaver2
testPolyPaver1 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaver1 =
  (
    Min [(1 , 1)],
    [
        LEQ [(1, dx1l), (2, dx2l), (3, (-1))] ((-yl) + (dx1l * x1l) + (dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, dx1r), (2, dx2r), (3, (-1))] ((-yr) + (dx1r * x1l) + (dx2r * x2l)), -- -5
        GEQ [(1, 1)] x1l,
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0
    ]
  )
  where
    x1l = 0.0
    x1r = 2.5
    x2l = 0.0
    x2r = 2.5
    dx1l = -1
    dx1r = -0.9
    dx2l = -0.9
    dx2r = -0.8
    yl = 4
    yr = 5

testPolyPaver2 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaver2 =
  (
    Max [(1 , 1)],
    [
        LEQ [(1, dx1l), (2, dx2l), (3, (-1))] ((-yl) + (dx1l * x1l) + (dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, dx1r), (2, dx2r), (3, (-1))] ((-yr) + (dx1r * x1l) + (dx2r * x2l)), -- -5
        GEQ [(1, 1)] x1l,
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0
    ]
  )
  where
    x1l = 0.0
    x1r = 2.5
    x2l = 0.0
    x2r = 2.5
    dx1l = -1
    dx1r = -0.9
    dx2l = -0.9
    dx2r = -0.8
    yl = 4
    yr = 5

testPolyPaver3 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaver3 =
  (
    Min [(2 , 1)],
    [
        LEQ [(1, dx1l), (2, dx2l), (3, (-1))] ((-yl) + (dx1l * x1l) + (dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, dx1r), (2, dx2r), (3, (-1))] ((-yr) + (dx1r * x1l) + (dx2r * x2l)), -- -5
        GEQ [(1, 1)] x1l,
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0
    ]
  )
  where
    x1l = 0.0
    x1r = 2.5
    x2l = 0.0
    x2r = 2.5
    dx1l = -1
    dx1r = -0.9
    dx2l = -0.9
    dx2r = -0.8
    yl = 4
    yr = 5

testPolyPaver4 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaver4 =
  (
    Max [(2 , 1)],
    [
        LEQ [(1, dx1l), (2, dx2l), (3, (-1))] ((-yl) + (dx1l * x1l) + (dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, dx1r), (2, dx2r), (3, (-1))] ((-yr) + (dx1r * x1l) + (dx2r * x2l)), -- -5
        GEQ [(1, 1)] x1l,
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0
    ]
  )
  where
    x1l = 0.0
    x1r = 2.5
    x2l = 0.0
    x2r = 2.5
    dx1l = -1
    dx1r = -0.9
    dx2l = -0.9
    dx2r = -0.8
    yl = 4
    yr = 5

testPolyPaver5 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaver5 =
  (
    Max [(1 , 1)],
    [
        LEQ [(1, dx1l), (2, dx2l), (3, (-1))] ((-yl) + (dx1l * x1l) + (dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, dx1r), (2, dx2r), (3, (-1))] ((-yr) + (dx1r * x1l) + (dx2r * x2l)), -- -5
        GEQ [(1, 1)] x1l,
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0
    ]
  )
  where
    x1l = 0.0
    x1r = 1.5
    x2l = 0.0
    x2r = 1.5
    dx1l = -1
    dx1r = -0.9
    dx2l = -0.9
    dx2r = -0.8
    yl = 4
    yr = 5

testPolyPaver6 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaver6 =
  (
    Min [(1 , 1)],
    [
        LEQ [(1, dx1l), (2, dx2l), (3, (-1))] ((-yl) + (dx1l * x1l) + (dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, dx1r), (2, dx2r), (3, (-1))] ((-yr) + (dx1r * x1l) + (dx2r * x2l)), -- -5
        GEQ [(1, 1)] x1l, 
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0
    ]
  )
  where
    x1l = 0.0
    x1r = 1.5
    x2l = 0.0
    x2r = 1.5
    dx1l = -1
    dx1r = -0.9
    dx2l = -0.9
    dx2r = -0.8
    yl = 4
    yr = 5

testPolyPaver7 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaver7 =
  (
    Max [(2 , 1)],
    [
        LEQ [(1, dx1l), (2, dx2l), (3, (-1))] ((-yl) + (dx1l * x1l) + (dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, dx1r), (2, dx2r), (3, (-1))] ((-yr) + (dx1r * x1l) + (dx2r * x2l)), -- -5
        GEQ [(1, 1)] x1l, 
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0
    ]
  )
  where
    x1l = 0.0
    x1r = 1.5
    x2l = 0.0
    x2r = 1.5
    dx1l = -1
    dx1r = -0.9
    dx2l = -0.9
    dx2r = -0.8
    yl = 4
    yr = 5

testPolyPaver8 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaver8 =
  (
    Min [(2 , 1)],
    [
        LEQ [(1, dx1l), (2, dx2l), (3, (-1))] ((-yl) + (dx1l * x1l) + (dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, dx1r), (2, dx2r), (3, (-1))] ((-yr) + (dx1r * x1l) + (dx2r * x2l)), -- -5
        GEQ [(1, 1)] x1l, 
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0
    ]
  )
  where
    x1l = 0.0
    x1r = 1.5
    x2l = 0.0
    x2r = 1.5
    dx1l = -1
    dx1r = -0.9
    dx2l = -0.9
    dx2r = -0.8
    yl = 4
    yr = 5

testPolyPaver9 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaver9 =
  (
    Max [(1 , 1)],
    [
        LEQ [(1, dx1l), (2, dx2l), (3, (-1))] ((-yl) + (dx1l * x1l) + (dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, dx1r), (2, dx2r), (3, (-1))] ((-yr) + (dx1r * x1l) + (dx2r * x2l)), -- -5
        GEQ [(1, 1)] x1l,
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0
    ]
  )
  where
    x1l = 0.0
    x1r = 3.5
    x2l = 0.0
    x2r = 3.5
    dx1l = -1
    dx1r = -0.9
    dx2l = -0.9
    dx2r = -0.8
    yl = 4
    yr = 5

testPolyPaver10 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaver10 =
  (
    Min [(1 , 1)],
    [
        LEQ [(1, dx1l), (2, dx2l), (3, (-1))] ((-yl) + (dx1l * x1l) + (dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, dx1r), (2, dx2r), (3, (-1))] ((-yr) + (dx1r * x1l) + (dx2r * x2l)), -- -5
        GEQ [(1, 1)] x1l,
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0
    ]
  )
  where
    x1l = 0.0
    x1r = 3.5
    x2l = 0.0
    x2r = 3.5
    dx1l = -1
    dx1r = -0.9
    dx2l = -0.9
    dx2r = -0.8
    yl = 4
    yr = 5

testPolyPaver11 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaver11 =
  (
    Max [(2 , 1)],
    [
        LEQ [(1, dx1l), (2, dx2l), (3, (-1))] ((-yl) + (dx1l * x1l) + (dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, dx1r), (2, dx2r), (3, (-1))] ((-yr) + (dx1r * x1l) + (dx2r * x2l)), -- -5
        GEQ [(1, 1)] x1l,
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0
    ]
  )
  where
    x1l = 0.0
    x1r = 3.5
    x2l = 0.0
    x2r = 3.5
    dx1l = -1
    dx1r = -0.9
    dx2l = -0.9
    dx2r = -0.8
    yl = 4
    yr = 5

testPolyPaver12 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaver12 =
  (
    Min [(2 , 1)],
    [
        LEQ [(1, dx1l), (2, dx2l), (3, (-1))] ((-yl) + (dx1l * x1l) + (dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, dx1r), (2, dx2r), (3, (-1))] ((-yr) + (dx1r * x1l) + (dx2r * x2l)), -- -5
        GEQ [(1, 1)] x1l,
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0
    ]
  )
  where
    x1l = 0.0
    x1r = 3.5
    x2l = 0.0
    x2r = 3.5
    dx1l = -1
    dx1r = -0.9
    dx2l = -0.9
    dx2r = -0.8
    yl = 4
    yr = 5

testPolyPaverTwoFs1 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaverTwoFs1 =
  (
    Max [(1 , 1)],
    [
        LEQ [(1, f1dx1l), (2, f1dx2l), (3, (-1))] ((-f1yl) + (f1dx1l * x1l) + (f1dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, f1dx1r), (2, f1dx2r), (3, (-1))] ((-f1yr) + (f1dx1r * x1l) + (f1dx2r * x2l)),        
        LEQ [(1, f2dx1l), (2, f2dx2l), (4, (-1))] ((-f2yl) + (f2dx1l * x1l) + (f2dx2l * x2l)),
        GEQ [(1, f2dx1r), (2, f2dx2r), (4, (-1))] ((-f2yr) + (f2dx1r * x1l) + (f2dx2r * x2l)), 
        GEQ [(1, 1)] x1l,
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0,
        LEQ [(4, 1)] 0
    ]
  )
  where
    x1l = 0.0
    x1r = 2.5
    x2l = 0.0
    x2r = 2.5
    f1dx1l = -1
    f1dx1r = -0.9
    f1dx2l = -0.9
    f1dx2r = -0.8
    f1yl = 4
    f1yr = 5    
    f2dx1l = -1
    f2dx1r = -0.9
    f2dx2l = -0.9
    f2dx2r = -0.8
    f2yl = 1
    f2yr = 2

testPolyPaverTwoFs2 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaverTwoFs2 =
  (
    Min [(1 , 1)],
    [
        LEQ [(1, f1dx1l), (2, f1dx2l), (3, (-1))] ((-f1yl) + (f1dx1l * x1l) + (f1dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, f1dx1r), (2, f1dx2r), (3, (-1))] ((-f1yr) + (f1dx1r * x1l) + (f1dx2r * x2l)),        
        LEQ [(1, f2dx1l), (2, f2dx2l), (4, (-1))] ((-f2yl) + (f2dx1l * x1l) + (f2dx2l * x2l)),
        GEQ [(1, f2dx1r), (2, f2dx2r), (4, (-1))] ((-f2yr) + (f2dx1r * x1l) + (f2dx2r * x2l)), 
        GEQ [(1, 1)] x1l,
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0,
        LEQ [(4, 1)] 0
    ]
  )
  where
    x1l = 0.0
    x1r = 2.5
    x2l = 0.0
    x2r = 2.5
    f1dx1l = -1
    f1dx1r = -0.9
    f1dx2l = -0.9
    f1dx2r = -0.8
    f1yl = 4
    f1yr = 5    
    f2dx1l = -1
    f2dx1r = -0.9
    f2dx2l = -0.9
    f2dx2r = -0.8
    f2yl = 1
    f2yr = 2

testPolyPaverTwoFs3 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaverTwoFs3 =
  (
    Max [(2 , 1)],
    [
        LEQ [(1, f1dx1l), (2, f1dx2l), (3, (-1))] ((-f1yl) + (f1dx1l * x1l) + (f1dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, f1dx1r), (2, f1dx2r), (3, (-1))] ((-f1yr) + (f1dx1r * x1l) + (f1dx2r * x2l)),        
        LEQ [(1, f2dx1l), (2, f2dx2l), (4, (-1))] ((-f2yl) + (f2dx1l * x1l) + (f2dx2l * x2l)),
        GEQ [(1, f2dx1r), (2, f2dx2r), (4, (-1))] ((-f2yr) + (f2dx1r * x1l) + (f2dx2r * x2l)), 
        GEQ [(1, 1)] x1l,
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0,
        LEQ [(4, 1)] 0
    ]
  )
  where
    x1l = 0.0
    x1r = 2.5
    x2l = 0.0
    x2r = 2.5
    f1dx1l = -1
    f1dx1r = -0.9
    f1dx2l = -0.9
    f1dx2r = -0.8
    f1yl = 4
    f1yr = 5    
    f2dx1l = -1
    f2dx1r = -0.9
    f2dx2l = -0.9
    f2dx2r = -0.8
    f2yl = 1
    f2yr = 2

testPolyPaverTwoFs4 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaverTwoFs4 =
  (
    Min [(2 , 1)],
    [
        LEQ [(1, f1dx1l), (2, f1dx2l), (3, (-1))] ((-f1yl) + (f1dx1l * x1l) + (f1dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, f1dx1r), (2, f1dx2r), (3, (-1))] ((-f1yr) + (f1dx1r * x1l) + (f1dx2r * x2l)),        
        LEQ [(1, f2dx1l), (2, f2dx2l), (4, (-1))] ((-f2yl) + (f2dx1l * x1l) + (f2dx2l * x2l)),
        GEQ [(1, f2dx1r), (2, f2dx2r), (4, (-1))] ((-f2yr) + (f2dx1r * x1l) + (f2dx2r * x2l)), 
        GEQ [(1, 1)] x1l,
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0,
        LEQ [(4, 1)] 0
    ]
  )
  where
    x1l = 0.0
    x1r = 2.5
    x2l = 0.0
    x2r = 2.5
    f1dx1l = -1
    f1dx1r = -0.9
    f1dx2l = -0.9
    f1dx2r = -0.8
    f1yl = 4
    f1yr = 5    
    f2dx1l = -1
    f2dx1r = -0.9
    f2dx2l = -0.9
    f2dx2r = -0.8
    f2yl = 1
    f2yr = 2

testPolyPaverTwoFs5 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaverTwoFs5 =
  (
    Max [(1 , 1)],
    [
        LEQ [(1, f1dx1l), (2, f1dx2l), (3, (-1))] ((-f1yl) + (f1dx1l * x1l) + (f1dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, f1dx1r), (2, f1dx2r), (3, (-1))] ((-f1yr) + (f1dx1r * x1l) + (f1dx2r * x2l)),        
        LEQ [(1, f2dx1l), (2, f2dx2l), (4, (-1))] ((-f2yl) + (f2dx1l * x1l) + (f2dx2l * x2l)),
        GEQ [(1, f2dx1r), (2, f2dx2r), (4, (-1))] ((-f2yr) + (f2dx1r * x1l) + (f2dx2r * x2l)), 
        GEQ [(1, 1)] x1l, -- don't need variable >= 0, already assumed
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0,
        LEQ [(4, 1)] 0 
    ]
  )
  where
    x1l = 0.0
    x1r = 2.5
    x2l = 0.0
    x2r = 2.5
    f1dx1l = -1
    f1dx1r = -0.9
    f1dx2l = -0.9
    f1dx2r = -0.8
    f1yl = 4
    f1yr = 5    
    f2dx1l = -0.66
    f2dx1r = -0.66
    f2dx2l = -0.66
    f2dx2r = -0.66
    f2yl = 3
    f2yr = 4

testPolyPaverTwoFs6 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaverTwoFs6 =
  (
    Min [(1 , 1)],
    [
        LEQ [(1, f1dx1l), (2, f1dx2l), (3, (-1))] ((-f1yl) + (f1dx1l * x1l) + (f1dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, f1dx1r), (2, f1dx2r), (3, (-1))] ((-f1yr) + (f1dx1r * x1l) + (f1dx2r * x2l)),        
        LEQ [(1, f2dx1l), (2, f2dx2l), (4, (-1))] ((-f2yl) + (f2dx1l * x1l) + (f2dx2l * x2l)),
        GEQ [(1, f2dx1r), (2, f2dx2r), (4, (-1))] ((-f2yr) + (f2dx1r * x1l) + (f2dx2r * x2l)), 
        GEQ [(1, 1)] x1l, -- don't need variable >= 0, already assumed
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0,
        LEQ [(4, 1)] 0 
    ]
  )
  where
    x1l = 0.0
    x1r = 2.5
    x2l = 0.0
    x2r = 2.5
    f1dx1l = -1
    f1dx1r = -0.9
    f1dx2l = -0.9
    f1dx2r = -0.8
    f1yl = 4
    f1yr = 5    
    f2dx1l = -0.66
    f2dx1r = -0.66
    f2dx2l = -0.66
    f2dx2r = -0.66
    f2yl = 3
    f2yr = 4

testPolyPaverTwoFs7 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaverTwoFs7 =
  (
    Max [(2 , 1)],
    [
        LEQ [(1, f1dx1l), (2, f1dx2l), (3, (-1))] ((-f1yl) + (f1dx1l * x1l) + (f1dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, f1dx1r), (2, f1dx2r), (3, (-1))] ((-f1yr) + (f1dx1r * x1l) + (f1dx2r * x2l)),        
        LEQ [(1, f2dx1l), (2, f2dx2l), (4, (-1))] ((-f2yl) + (f2dx1l * x1l) + (f2dx2l * x2l)),
        GEQ [(1, f2dx1r), (2, f2dx2r), (4, (-1))] ((-f2yr) + (f2dx1r * x1l) + (f2dx2r * x2l)), 
        GEQ [(1, 1)] x1l, -- don't need variable >= 0, already assumed
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0,
        LEQ [(4, 1)] 0 
    ]
  )
  where
    x1l = 0.0
    x1r = 2.5
    x2l = 0.0
    x2r = 2.5
    f1dx1l = -1
    f1dx1r = -0.9
    f1dx2l = -0.9
    f1dx2r = -0.8
    f1yl = 4
    f1yr = 5    
    f2dx1l = -0.66
    f2dx1r = -0.66
    f2dx2l = -0.66
    f2dx2r = -0.66
    f2yl = 3
    f2yr = 4

testPolyPaverTwoFs8 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaverTwoFs8 =
  (
    Min [(2 , 1)],
    [
        LEQ [(1, f1dx1l), (2, f1dx2l), (3, (-1))] ((-f1yl) + (f1dx1l * x1l) + (f1dx2l * x2l)), -- -4, This will need an artificial variable
        GEQ [(1, f1dx1r), (2, f1dx2r), (3, (-1))] ((-f1yr) + (f1dx1r * x1l) + (f1dx2r * x2l)),        
        LEQ [(1, f2dx1l), (2, f2dx2l), (4, (-1))] ((-f2yl) + (f2dx1l * x1l) + (f2dx2l * x2l)),
        GEQ [(1, f2dx1r), (2, f2dx2r), (4, (-1))] ((-f2yr) + (f2dx1r * x1l) + (f2dx2r * x2l)), 
        GEQ [(1, 1)] x1l, -- don't need variable >= 0, already assumed
        LEQ [(1, 1)] x1r,
        GEQ [(2, 1)] x2l,
        LEQ [(2, 1)] x2r,
        LEQ [(3, 1)] 0,
        LEQ [(4, 1)] 0 
    ]
  )
  where
    x1l = 0.0
    x1r = 2.5
    x2l = 0.0
    x2r = 2.5
    f1dx1l = -1
    f1dx1r = -0.9
    f1dx2l = -0.9
    f1dx2r = -0.8
    f1yl = 4
    f1yr = 5    
    f2dx1l = -0.66
    f2dx1r = -0.66
    f2dx2l = -0.66
    f2dx2r = -0.66
    f2yl = 3
    f2yr = 4

-- Test cases produced by old simplex-haskell/SoPlex QuickCheck prop

-- SoPlex gives -400 for the following system but -370 is the optimized solution
-- simplex-haskell gives -370
-- SoPlex gives -370 if we simplify the system before sending it to SoPlex
testQuickCheck1 =
  (
    Max [(1, -6), (1, -8), (1, 9), (1, 10), (1, 8), (2, -15), (1, 13), (1, -14), (2, 0)],
    [
      EQ [(1, 5), (1, 6), (2, -2), (1, 7), (1, 6), (2, 0)] (-12),
      GEQ [(1, 11), (1, 0), (1, -5), (1, -12), (1, -14), (2, 11)] (-7),
      GEQ [(1, -12), (1, -7), (1, -2), (2, -9), (1, 3), (1, 5), (1, -15), (2, 14)] (-8), GEQ [(1, 13), (1, 1), (1, -11), (2, 0)] 5,
      LEQ [(1, -10), (1, -14), (1, 4), (1, -2), (1, -10), (1, -5), (1, -11)] (-1)
    ]
  )

-- If we do not call simplifyPolyConstraints before we start the simplex algorithm, the following return a wrong solution
-- Correct solution is -2/9
testQuickCheck2 =
  (
    Max [(1, -3), (2, 5)],
    [
      LEQ [(2, -1), (1, -6), (2, 7)] 4,
      LEQ [(1, 1), (2, -4), (3, 3)] (-2),
      LEQ [(2, 6), (1, -4), (2, 1)] 0]
  )

-- This test will fail if the objective function is not simplified
testQuickCheck3 = 
  (
    Min [(2, 0), (2, -4)],
    [
      GEQ [(1, 5), (2, 4)] (-4),
      LEQ [(1, -1), (2, -1)] 2,
      LEQ [(2, 1)] 2,
      GEQ [(1, -5), (2, -1), (2, 1)] (-5)
    ]
  )