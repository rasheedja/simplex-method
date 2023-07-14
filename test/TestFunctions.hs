module TestFunctions where

import Data.Map qualified as M
import Data.Ratio
import Linear.Simplex.Types
import Prelude hiding (EQ)

testsList :: [((ObjectiveFunction, [PolyConstraint]), Maybe Result)]
testsList =
  [ (test1, Just (Result 7 (M.fromList [(7, 29), (1, 3), (2, 4)])))
  , (test2, Just (Result 7 (M.fromList [(7, 0)])))
  , (test3, Nothing)
  , (test4, Just (Result 11 (M.fromList [(11, 237 % 7), (1, 24 % 7), (2, 33 % 7)])))
  , (test5, Just (Result 9 (M.fromList [(9, 3 % 5), (2, 14 % 5), (3, 17 % 5)])))
  , (test6, Nothing)
  , (test7, Just (Result 8 (M.fromList [(8, 1), (2, 2), (1, 3)])))
  , (test8, Just (Result 8 (M.fromList [(8, (-1) % 4), (2, 9 % 2), (1, 17 % 4)])))
  , (test9, Just (Result 7 (M.fromList [(7, 5), (3, 2), (4, 1)])))
  , (test10, Just (Result 7 (M.fromList [(7, 8), (1, 2), (2, 6)])))
  , (test11, Just (Result 8 (M.fromList [(8, 20), (4, 16), (3, 6)])))
  , (test12, Just (Result 8 (M.fromList [(8, 6), (4, 2), (5, 2)])))
  , (test13, Just (Result 6 (M.fromList [(6, 150), (2, 150)])))
  , (test14, Just (Result 6 (M.fromList [(6, 40 % 3), (2, 40 % 3)])))
  , (test15, Nothing)
  , (test16, Just (Result 6 (M.fromList [(6, 75), (1, 75 % 2)])))
  , (test17, Just (Result 7 (M.fromList [(7, (-120)), (1, 20)])))
  , (test18, Just (Result 7 (M.fromList [(7, 10), (3, 5)])))
  , (test19, Nothing)
  , (test20, Nothing)
  , (test21, Just (Result 7 (M.fromList [(7, 250), (2, 50)])))
  , (test22, Just (Result 7 (M.fromList [(7, 0)])))
  , (test23, Nothing)
  , (test24, Just (Result 10 (M.fromList [(10, 300), (3, 150)])))
  , (test25, Just (Result 3 (M.fromList [(3, 15), (1, 15)])))
  , (test26, Just (Result 6 (M.fromList [(6, 20), (1, 10), (2, 10)])))
  , (test27, Just (Result 3 (M.fromList [(3, 0)])))
  , (test28, Just (Result 6 (M.fromList [(6, 0), (2, 10)])))
  , (test29, Nothing)
  , (test30, Nothing)
  , (testPolyPaver1, Just (Result 12 (M.fromList [(12, 7 % 4), (2, 5 % 2), (1, 7 % 4), (3, 0)])))
  , (testPolyPaver2, Just (Result 12 (M.fromList [(12, 5 % 2), (2, 5 % 3), (1, 5 % 2), (3, 0)])))
  , (testPolyPaver3, Just (Result 12 (M.fromList [(12, 5 % 3), (2, 5 % 3), (1, 5 % 2), (3, 0)])))
  , (testPolyPaver4, Just (Result 12 (M.fromList [(12, 5 % 2), (2, 5 % 2), (1, 5 % 2), (3, 0)])))
  , (testPolyPaver5, Nothing)
  , (testPolyPaver6, Nothing)
  , (testPolyPaver7, Nothing)
  , (testPolyPaver8, Nothing)
  , (testPolyPaver9, Just (Result 12 (M.fromList [(12, 7 % 2), (2, 5 % 9), (1, 7 % 2), (3, 0)])))
  , (testPolyPaver10, Just (Result 12 (M.fromList [(12, 17 % 20), (2, 7 % 2), (1, 17 % 20), (3, 0)])))
  , (testPolyPaver11, Just (Result 12 (M.fromList [(12, 7 % 2), (2, 7 % 2), (1, 22 % 9)])))
  , (testPolyPaver12, Just (Result 12 (M.fromList [(12, 5 % 9), (2, 5 % 9), (1, 7 % 2), (3, 0)])))
  , (testPolyPaverTwoFs1, Nothing)
  , (testPolyPaverTwoFs2, Nothing)
  , (testPolyPaverTwoFs3, Nothing)
  , (testPolyPaverTwoFs4, Nothing)
  , (testPolyPaverTwoFs5, Just (Result 17 (M.fromList [(17, 5 % 2), (2, 45 % 22), (1, 5 % 2), (4, 0)])))
  , (testPolyPaverTwoFs6, Just (Result 17 (M.fromList [(17, 45 % 22), (2, 5 % 2), (1, 45 % 22), (4, 0)])))
  , (testPolyPaverTwoFs7, Just (Result 17 (M.fromList [(17, 5 % 2), (2, 5 % 2), (1, 5 % 2), (4, 0)])))
  , (testPolyPaverTwoFs8, Just (Result 17 (M.fromList [(17, 45 % 22), (2, 45 % 22), (1, 5 % 2), (4, 0)])))
  , (testLeqGeqBugMin1, Just (Result 5 (M.fromList [(5, 3), (1, 3), (2, 3)])))
  , (testLeqGeqBugMax1, Just (Result 5 (M.fromList [(5, 3), (1, 3), (2, 3)])))
  , (testLeqGeqBugMin2, Just (Result 5 (M.fromList [(5, 3), (1, 3), (2, 3)])))
  , (testLeqGeqBugMax2, Just (Result 5 (M.fromList [(5, 3), (1, 3), (2, 3)])))
  , (testQuickCheck1, Just (Result 10 (M.fromList [(10, (-370)), (2, 26), (1, 5 % 3)])))
  , (testQuickCheck2, Just (Result 8 (M.fromList [(8, (-2) % 9), (1, 14 % 9), (2, 8 % 9)])))
  , (testQuickCheck3, Just (Result 7 (M.fromList [(7, (-8)), (2, 2)])))
  ]

testLeqGeqBugMin1 :: (ObjectiveFunction, [PolyConstraint])
testLeqGeqBugMin1 =
  ( Min (M.fromList [(1, 1)])
  ,
    [ GEQ (M.fromList [(1, 1)]) 3
    , LEQ (M.fromList [(1, 1)]) 3
    , GEQ (M.fromList [(2, 1)]) 3
    , LEQ (M.fromList [(2, 1)]) 3
    ]
  )

testLeqGeqBugMax1 :: (ObjectiveFunction, [PolyConstraint])
testLeqGeqBugMax1 =
  ( Min (M.fromList [(1, 1)])
  ,
    [ GEQ (M.fromList [(1, 1)]) 3
    , LEQ (M.fromList [(1, 1)]) 3
    , GEQ (M.fromList [(2, 1)]) 3
    , LEQ (M.fromList [(2, 1)]) 3
    ]
  )

testLeqGeqBugMin2 :: (ObjectiveFunction, [PolyConstraint])
testLeqGeqBugMin2 =
  ( Min (M.fromList [(1, 1)])
  ,
    [ GEQ (M.fromList [(1, 1)]) 3
    , LEQ (M.fromList [(1, 1)]) 3
    , GEQ (M.fromList [(2, 1)]) 3
    , LEQ (M.fromList [(2, 1)]) 3
    ]
  )

testLeqGeqBugMax2 :: (ObjectiveFunction, [PolyConstraint])
testLeqGeqBugMax2 =
  ( Min (M.fromList [(1, 1)])
  ,
    [ GEQ (M.fromList [(1, 1)]) 3
    , LEQ (M.fromList [(1, 1)]) 3
    , GEQ (M.fromList [(2, 1)]) 3
    , LEQ (M.fromList [(2, 1)]) 3
    ]
  )

-- From page 50 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 29, 1 = 3, 2 = 4,
test1 :: (ObjectiveFunction, [PolyConstraint])
test1 =
  ( Max (M.fromList [(1, 3), (2, 5)])
  ,
    [ LEQ (M.fromList [(1, 3), (2, 1)]) 15
    , LEQ (M.fromList [(1, 1), (2, 1)]) 7
    , LEQ (M.fromList [(2, 1)]) 4
    , LEQ (M.fromList [(1, -1), (2, 2)]) 6
    ]
  )

test2 :: (ObjectiveFunction, [PolyConstraint])
test2 =
  ( Min (M.fromList [(1, 3), (2, 5)])
  ,
    [ LEQ (M.fromList [(1, 3), (2, 1)]) 15
    , LEQ (M.fromList [(1, 1), (2, 1)]) 7
    , LEQ (M.fromList [(2, 1)]) 4
    , LEQ (M.fromList [(1, -1), (2, 2)]) 6
    ]
  )

test3 :: (ObjectiveFunction, [PolyConstraint])
test3 =
  ( Max (M.fromList [(1, 3), (2, 5)])
  ,
    [ GEQ (M.fromList [(1, 3), (2, 1)]) 15
    , GEQ (M.fromList [(1, 1), (2, 1)]) 7
    , GEQ (M.fromList [(2, 1)]) 4
    , GEQ (M.fromList [(1, -1), (2, 2)]) 6
    ]
  )

test4 :: (ObjectiveFunction, [PolyConstraint])
test4 =
  ( Min (M.fromList [(1, 3), (2, 5)])
  ,
    [ GEQ (M.fromList [(1, 3), (2, 1)]) 15
    , GEQ (M.fromList [(1, 1), (2, 1)]) 7
    , GEQ (M.fromList [(2, 1)]) 4
    , GEQ (M.fromList [(1, -1), (2, 2)]) 6
    ]
  )

-- From https://www.eng.uwaterloo.ca/~syde05/phase1.pdf
-- Solution: obj = 3/5, 2 = 14/5, 3 = 17/5
-- requires two phases
test5 :: (ObjectiveFunction, [PolyConstraint])
test5 =
  ( Max (M.fromList [(1, 1), (2, -1), (3, 1)])
  ,
    [ LEQ (M.fromList [(1, 2), (2, -1), (3, 2)]) 4
    , LEQ (M.fromList [(1, 2), (2, -3), (3, 1)]) (-5)
    , LEQ (M.fromList [(1, -1), (2, 1), (3, -2)]) (-1)
    ]
  )

test6 :: (ObjectiveFunction, [PolyConstraint])
test6 =
  ( Min (M.fromList [(1, 1), (2, -1), (3, 1)])
  ,
    [ LEQ (M.fromList [(1, 2), (2, -1), (3, 2)]) 4
    , LEQ (M.fromList [(1, 2), (2, -3), (3, 1)]) (-5)
    , LEQ (M.fromList [(1, -1), (2, 1), (3, -2)]) (-1)
    ]
  )

test7 :: (ObjectiveFunction, [PolyConstraint])
test7 =
  ( Max (M.fromList [(1, 1), (2, -1), (3, 1)])
  ,
    [ GEQ (M.fromList [(1, 2), (2, -1), (3, 2)]) 4
    , GEQ (M.fromList [(1, 2), (2, -3), (3, 1)]) (-5)
    , GEQ (M.fromList [(1, -1), (2, 1), (3, -2)]) (-1)
    ]
  )

test8 :: (ObjectiveFunction, [PolyConstraint])
test8 =
  ( Min (M.fromList [(1, 1), (2, -1), (3, 1)])
  ,
    [ GEQ (M.fromList [(1, 2), (2, -1), (3, 2)]) 4
    , GEQ (M.fromList [(1, 2), (2, -3), (3, 1)]) (-5)
    , GEQ (M.fromList [(1, -1), (2, 1), (3, -2)]) (-1)
    ]
  )

-- From page 49 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = -5, 3 = 2, 4 = 1, objVar was negated so actual val is 5 wa
-- requires two phases
test9 :: (ObjectiveFunction, [PolyConstraint])
test9 =
  ( Min (M.fromList [(1, 1), (2, 1), (3, 2), (4, 1)])
  ,
    [ EQ (M.fromList [(1, 1), (3, 2), (4, -2)]) 2
    , EQ (M.fromList [(2, 1), (3, 1), (4, 4)]) 6
    ]
  )

test10 :: (ObjectiveFunction, [PolyConstraint])
test10 =
  ( Min (M.fromList [(1, 1), (2, 1), (3, 2), (4, 1)])
  ,
    [ EQ (M.fromList [(1, 1), (3, 2), (4, -2)]) 2
    , EQ (M.fromList [(2, 1), (3, 1), (4, 4)]) 6
    , GEQ (M.fromList [(1, -1), (2, -1), (3, -2), (4, -1)]) (-2)
    ]
  )

-- Adapted from page 52 of 'Linear and Integer Programming Made Easy'
-- Removed variables which do not appear in the system (these should be artificial variables)
-- Solution: obj = 20, 3 = 6, 4 = 16 wq
test11 :: (ObjectiveFunction, [PolyConstraint])
test11 =
  ( Max (M.fromList [(3, -2), (4, 2), (5, 1)])
  ,
    [ EQ (M.fromList [(3, -2), (4, 1), (5, 1)]) 4
    , EQ (M.fromList [(3, 3), (4, -1), (5, 2)]) 2
    ]
  )

test12 :: (ObjectiveFunction, [PolyConstraint])
test12 =
  ( Min (M.fromList [(3, -2), (4, 2), (5, 1)])
  ,
    [ EQ (M.fromList [(3, -2), (4, 1), (5, 1)]) 4
    , EQ (M.fromList [(3, 3), (4, -1), (5, 2)]) 2
    ]
  )

-- From page 59 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 150, 1 = 0, 2 = 150
-- requires two phases
test13 :: (ObjectiveFunction, [PolyConstraint])
test13 =
  ( Max (M.fromList [(1, 2), (2, 1)])
  ,
    [ LEQ (M.fromList [(1, 4), (2, 1)]) 150
    , LEQ (M.fromList [(1, 2), (2, -3)]) (-40)
    ]
  )

test14 :: (ObjectiveFunction, [PolyConstraint])
test14 =
  ( Min (M.fromList [(1, 2), (2, 1)])
  ,
    [ LEQ (M.fromList [(1, 4), (2, 1)]) 150
    , LEQ (M.fromList [(1, 2), (2, -3)]) (-40)
    ]
  )

test15 :: (ObjectiveFunction, [PolyConstraint])
test15 =
  ( Max (M.fromList [(1, 2), (2, 1)])
  ,
    [ GEQ (M.fromList [(1, 4), (2, 1)]) 150
    , GEQ (M.fromList [(1, 2), (2, -3)]) (-40)
    ]
  )

test16 :: (ObjectiveFunction, [PolyConstraint])
test16 =
  ( Min (M.fromList [(1, 2), (2, 1)])
  ,
    [ GEQ (M.fromList [(1, 4), (2, 1)]) 150
    , GEQ (M.fromList [(1, 2), (2, -3)]) (-40)
    ]
  )

-- From page 59 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 120, 1 = 20, 2 = 0, 3 = 0, objVar was negated so actual val is -120
test17 :: (ObjectiveFunction, [PolyConstraint])
test17 =
  ( Min (M.fromList [(1, -6), (2, -4), (3, 2)])
  ,
    [ LEQ (M.fromList [(1, 1), (2, 1), (3, 4)]) 20
    , LEQ (M.fromList [(2, -5), (3, 5)]) 100
    , LEQ (M.fromList [(1, 1), (3, 1), (1, 1)]) 400
    ]
  )

test18 :: (ObjectiveFunction, [PolyConstraint])
test18 =
  ( Max (M.fromList [(1, -6), (2, -4), (3, 2)])
  ,
    [ LEQ (M.fromList [(1, 1), (2, 1), (3, 4)]) 20
    , LEQ (M.fromList [(2, -5), (3, 5)]) 100
    , LEQ (M.fromList [(1, 1), (3, 1), (1, 1)]) 400
    ]
  )

test19 :: (ObjectiveFunction, [PolyConstraint])
test19 =
  ( Min (M.fromList [(1, -6), (2, -4), (3, 2)])
  ,
    [ GEQ (M.fromList [(1, 1), (2, 1), (3, 4)]) 20
    , GEQ (M.fromList [(2, -5), (3, 5)]) 100
    , GEQ (M.fromList [(1, 1), (3, 1), (1, 1)]) 400
    ]
  )

test20 :: (ObjectiveFunction, [PolyConstraint])
test20 =
  ( Max (M.fromList [(1, -6), (2, -4), (3, 2)])
  ,
    [ GEQ (M.fromList [(1, 1), (2, 1), (3, 4)]) 20
    , GEQ (M.fromList [(2, -5), (3, 5)]) 100
    , GEQ (M.fromList [(1, 1), (3, 1), (1, 1)]) 400
    ]
  )

-- From page 59 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 250, 1 = 0, 2 = 50, 3 = 0
test21 :: (ObjectiveFunction, [PolyConstraint])
test21 =
  ( Max (M.fromList [(1, 3), (2, 5), (3, 2)])
  ,
    [ LEQ (M.fromList [(1, 5), (2, 1), (3, 4)]) 50
    , LEQ (M.fromList [(1, 1), (2, -1), (3, 1)]) 150
    , LEQ (M.fromList [(1, 2), (2, 1), (3, 2)]) 100
    ]
  )

test22 :: (ObjectiveFunction, [PolyConstraint])
test22 =
  ( Min (M.fromList [(1, 3), (2, 5), (3, 2)])
  ,
    [ LEQ (M.fromList [(1, 5), (2, 1), (3, 4)]) 50
    , LEQ (M.fromList [(1, 1), (2, -1), (3, 1)]) 150
    , LEQ (M.fromList [(1, 2), (2, 1), (3, 2)]) 100
    ]
  )

test23 :: (ObjectiveFunction, [PolyConstraint])
test23 =
  ( Max (M.fromList [(1, 3), (2, 5), (3, 2)])
  ,
    [ GEQ (M.fromList [(1, 5), (2, 1), (3, 4)]) 50
    , GEQ (M.fromList [(1, 1), (2, -1), (3, 1)]) 150
    , GEQ (M.fromList [(1, 2), (2, 1), (3, 2)]) 100
    ]
  )

test24 :: (ObjectiveFunction, [PolyConstraint])
test24 =
  ( Min (M.fromList [(1, 3), (2, 5), (3, 2)])
  ,
    [ GEQ (M.fromList [(1, 5), (2, 1), (3, 4)]) 50
    , GEQ (M.fromList [(1, 1), (2, -1), (3, 1)]) 150
    , GEQ (M.fromList [(1, 2), (2, 1), (3, 2)]) 100
    ]
  )

test25 :: (ObjectiveFunction, [PolyConstraint])
test25 =
  ( Max (M.fromList [(1, 1)])
  ,
    [ LEQ (M.fromList [(1, 1)]) 15
    ]
  )

test26 :: (ObjectiveFunction, [PolyConstraint])
test26 =
  ( Max (M.fromList [(1, 2)])
  ,
    [ LEQ (M.fromList [(1, 2)]) 20
    , GEQ (M.fromList [(2, 1)]) 10
    ]
  )

test27 :: (ObjectiveFunction, [PolyConstraint])
test27 =
  ( Min (M.fromList [(1, 1)])
  ,
    [ LEQ (M.fromList [(1, 1)]) 15
    ]
  )

test28 :: (ObjectiveFunction, [PolyConstraint])
test28 =
  ( Min (M.fromList [(1, 2)])
  ,
    [ LEQ (M.fromList [(1, 2)]) 20
    , GEQ (M.fromList [(2, 1)]) 10
    ]
  )

test29 :: (ObjectiveFunction, [PolyConstraint])
test29 =
  ( Max (M.fromList [(1, 1)])
  ,
    [ LEQ (M.fromList [(1, 1)]) 15
    , GEQ (M.fromList [(1, 1)]) 15.01
    ]
  )

test30 :: (ObjectiveFunction, [PolyConstraint])
test30 =
  ( Max (M.fromList [(1, 1)])
  ,
    [ LEQ (M.fromList [(1, 1)]) 15
    , GEQ (M.fromList [(1, 1)]) 15.01
    , GEQ (M.fromList [(2, 1)]) 10
    ]
  )

-- Tests for systems similar to those from PolyPaver2
testPolyPaver1 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaver1 =
  ( Min (M.fromList [(1, 1)])
  ,
    [ LEQ (M.fromList [(1, dx1l), (2, dx2l), (3, (-1))]) (-yl + dx1l * x1l + dx2l * x2l)
    , GEQ (M.fromList [(1, dx1r), (2, dx2r), (3, (-1))]) (-yr + dx1r * x1l + dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
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
  ( Max (M.fromList [(1, 1)])
  ,
    [ LEQ (M.fromList [(1, dx1l), (2, dx2l), (3, (-1))]) (-yl + dx1l * x1l + dx2l * x2l)
    , GEQ (M.fromList [(1, dx1r), (2, dx2r), (3, (-1))]) (-yr + dx1r * x1l + dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
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
  ( Min (M.fromList [(2, 1)])
  ,
    [ LEQ (M.fromList [(1, dx1l), (2, dx2l), (3, (-1))]) (-yl + dx1l * x1l + dx2l * x2l)
    , GEQ (M.fromList [(1, dx1r), (2, dx2r), (3, (-1))]) (-yr + dx1r * x1l + dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
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
  ( Max (M.fromList [(2, 1)])
  ,
    [ LEQ (M.fromList [(1, dx1l), (2, dx2l), (3, (-1))]) (-yl + dx1l * x1l + dx2l * x2l)
    , GEQ (M.fromList [(1, dx1r), (2, dx2r), (3, (-1))]) (-yr + dx1r * x1l + dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
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
  ( Max (M.fromList [(1, 1)])
  ,
    [ LEQ (M.fromList [(1, dx1l), (2, dx2l), (3, (-1))]) (-yl + dx1l * x1l + dx2l * x2l)
    , GEQ (M.fromList [(1, dx1r), (2, dx2r), (3, (-1))]) (-yr + dx1r * x1l + dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
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
  ( Min (M.fromList [(1, 1)])
  ,
    [ LEQ (M.fromList [(1, dx1l), (2, dx2l), (3, (-1))]) (-yl + dx1l * x1l + dx2l * x2l)
    , GEQ (M.fromList [(1, dx1r), (2, dx2r), (3, (-1))]) (-yr + dx1r * x1l + dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
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
  ( Max (M.fromList [(2, 1)])
  ,
    [ LEQ (M.fromList [(1, dx1l), (2, dx2l), (3, (-1))]) (-yl + dx1l * x1l + dx2l * x2l)
    , GEQ (M.fromList [(1, dx1r), (2, dx2r), (3, (-1))]) (-yr + dx1r * x1l + dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
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
  ( Min (M.fromList [(2, 1)])
  ,
    [ LEQ (M.fromList [(1, dx1l), (2, dx2l), (3, (-1))]) (-yl + dx1l * x1l + dx2l * x2l)
    , GEQ (M.fromList [(1, dx1r), (2, dx2r), (3, (-1))]) (-yr + dx1r * x1l + dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
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
  ( Max (M.fromList [(1, 1)])
  ,
    [ LEQ (M.fromList [(1, dx1l), (2, dx2l), (3, (-1))]) (-yl + dx1l * x1l + dx2l * x2l)
    , GEQ (M.fromList [(1, dx1r), (2, dx2r), (3, (-1))]) (-yr + dx1r * x1l + dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
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
  ( Min (M.fromList [(1, 1)])
  ,
    [ LEQ (M.fromList [(1, dx1l), (2, dx2l), (3, (-1))]) (-yl + dx1l * x1l + dx2l * x2l)
    , GEQ (M.fromList [(1, dx1r), (2, dx2r), (3, (-1))]) (-yr + dx1r * x1l + dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
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
  ( Max (M.fromList [(2, 1)])
  ,
    [ LEQ (M.fromList [(1, dx1l), (2, dx2l), (3, (-1))]) (-yl + dx1l * x1l + dx2l * x2l)
    , GEQ (M.fromList [(1, dx1r), (2, dx2r), (3, (-1))]) (-yr + dx1r * x1l + dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
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
  ( Min (M.fromList [(2, 1)])
  ,
    [ LEQ (M.fromList [(1, dx1l), (2, dx2l), (3, (-1))]) (-yl + dx1l * x1l + dx2l * x2l)
    , GEQ (M.fromList [(1, dx1r), (2, dx2r), (3, (-1))]) (-yr + dx1r * x1l + dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
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
  ( Max (M.fromList [(1, 1)])
  ,
    [ LEQ (M.fromList [(1, f1dx1l), (2, f1dx2l), (3, (-1))]) (-f1yl + f1dx1l * x1l + f1dx2l * x2l)
    , GEQ (M.fromList [(1, f1dx1r), (2, f1dx2r), (3, (-1))]) (-f1yr + f1dx1r * x1l + f1dx2r * x2l)
    , LEQ (M.fromList [(1, f2dx1l), (2, f2dx2l), (4, (-1))]) (-f2yl + f2dx1l * x1l + f2dx2l * x2l)
    , GEQ (M.fromList [(1, f2dx1r), (2, f2dx2r), (4, (-1))]) (-f2yr + f2dx1r * x1l + f2dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
    , LEQ (M.fromList [(4, 1)]) 0
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
  ( Min (M.fromList [(1, 1)])
  ,
    [ LEQ (M.fromList [(1, f1dx1l), (2, f1dx2l), (3, (-1))]) (-f1yl + f1dx1l * x1l + f1dx2l * x2l)
    , GEQ (M.fromList [(1, f1dx1r), (2, f1dx2r), (3, (-1))]) (-f1yr + f1dx1r * x1l + f1dx2r * x2l)
    , LEQ (M.fromList [(1, f2dx1l), (2, f2dx2l), (4, (-1))]) (-f2yl + f2dx1l * x1l + f2dx2l * x2l)
    , GEQ (M.fromList [(1, f2dx1r), (2, f2dx2r), (4, (-1))]) (-f2yr + f2dx1r * x1l + f2dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
    , LEQ (M.fromList [(4, 1)]) 0
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
  ( Max (M.fromList [(2, 1)])
  ,
    [ LEQ (M.fromList [(1, f1dx1l), (2, f1dx2l), (3, (-1))]) (-f1yl + f1dx1l * x1l + f1dx2l * x2l)
    , GEQ (M.fromList [(1, f1dx1r), (2, f1dx2r), (3, (-1))]) (-f1yr + f1dx1r * x1l + f1dx2r * x2l)
    , LEQ (M.fromList [(1, f2dx1l), (2, f2dx2l), (4, (-1))]) (-f2yl + f2dx1l * x1l + f2dx2l * x2l)
    , GEQ (M.fromList [(1, f2dx1r), (2, f2dx2r), (4, (-1))]) (-f2yr + f2dx1r * x1l + f2dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
    , LEQ (M.fromList [(4, 1)]) 0
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
  ( Min (M.fromList [(2, 1)])
  ,
    [ LEQ (M.fromList [(1, f1dx1l), (2, f1dx2l), (3, (-1))]) (-f1yl + f1dx1l * x1l + f1dx2l * x2l)
    , GEQ (M.fromList [(1, f1dx1r), (2, f1dx2r), (3, (-1))]) (-f1yr + f1dx1r * x1l + f1dx2r * x2l)
    , LEQ (M.fromList [(1, f2dx1l), (2, f2dx2l), (4, (-1))]) (-f2yl + f2dx1l * x1l + f2dx2l * x2l)
    , GEQ (M.fromList [(1, f2dx1r), (2, f2dx2r), (4, (-1))]) (-f2yr + f2dx1r * x1l + f2dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
    , LEQ (M.fromList [(4, 1)]) 0
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
  ( Max (M.fromList [(1, 1)])
  ,
    [ LEQ (M.fromList [(1, f1dx1l), (2, f1dx2l), (3, (-1))]) (-f1yl + f1dx1l * x1l + f1dx2l * x2l)
    , GEQ (M.fromList [(1, f1dx1r), (2, f1dx2r), (3, (-1))]) (-f1yr + f1dx1r * x1l + f1dx2r * x2l)
    , LEQ (M.fromList [(1, f2dx1l), (2, f2dx2l), (4, (-1))]) (-f2yl + f2dx1l * x1l + f2dx2l * x2l)
    , GEQ (M.fromList [(1, f2dx1r), (2, f2dx2r), (4, (-1))]) (-f2yr + f2dx1r * x1l + f2dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
    , LEQ (M.fromList [(4, 1)]) 0
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
  ( Min (M.fromList [(1, 1)])
  ,
    [ LEQ (M.fromList [(1, f1dx1l), (2, f1dx2l), (3, (-1))]) (-f1yl + f1dx1l * x1l + f1dx2l * x2l)
    , GEQ (M.fromList [(1, f1dx1r), (2, f1dx2r), (3, (-1))]) (-f1yr + f1dx1r * x1l + f1dx2r * x2l)
    , LEQ (M.fromList [(1, f2dx1l), (2, f2dx2l), (4, (-1))]) (-f2yl + f2dx1l * x1l + f2dx2l * x2l)
    , GEQ (M.fromList [(1, f2dx1r), (2, f2dx2r), (4, (-1))]) (-f2yr + f2dx1r * x1l + f2dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
    , LEQ (M.fromList [(4, 1)]) 0
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
  ( Max (M.fromList [(2, 1)])
  ,
    [ LEQ (M.fromList [(1, f1dx1l), (2, f1dx2l), (3, (-1))]) (-f1yl + f1dx1l * x1l + f1dx2l * x2l)
    , GEQ (M.fromList [(1, f1dx1r), (2, f1dx2r), (3, (-1))]) (-f1yr + f1dx1r * x1l + f1dx2r * x2l)
    , LEQ (M.fromList [(1, f2dx1l), (2, f2dx2l), (4, (-1))]) (-f2yl + f2dx1l * x1l + f2dx2l * x2l)
    , GEQ (M.fromList [(1, f2dx1r), (2, f2dx2r), (4, (-1))]) (-f2yr + f2dx1r * x1l + f2dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
    , LEQ (M.fromList [(4, 1)]) 0
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
  ( Min (M.fromList [(2, 1)])
  ,
    [ LEQ (M.fromList [(1, f1dx1l), (2, f1dx2l), (3, (-1))]) (-f1yl + f1dx1l * x1l + f1dx2l * x2l)
    , GEQ (M.fromList [(1, f1dx1r), (2, f1dx2r), (3, (-1))]) (-f1yr + f1dx1r * x1l + f1dx2r * x2l)
    , LEQ (M.fromList [(1, f2dx1l), (2, f2dx2l), (4, (-1))]) (-f2yl + f2dx1l * x1l + f2dx2l * x2l)
    , GEQ (M.fromList [(1, f2dx1r), (2, f2dx2r), (4, (-1))]) (-f2yr + f2dx1r * x1l + f2dx2r * x2l)
    , GEQ (M.fromList [(1, 1)]) x1l
    , LEQ (M.fromList [(1, 1)]) x1r
    , GEQ (M.fromList [(2, 1)]) x2l
    , LEQ (M.fromList [(2, 1)]) x2r
    , LEQ (M.fromList [(3, 1)]) 0
    , LEQ (M.fromList [(4, 1)]) 0
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
testQuickCheck1 :: (ObjectiveFunction, [PolyConstraint])
testQuickCheck1 =
  ( Max (M.fromList [(1, -6), (1, -8), (1, 9), (1, 10), (1, 8), (2, -15), (1, 13), (1, -14), (2, 0)])
  ,
    [ EQ (M.fromList [(1, 5), (1, 6), (2, -2), (1, 7), (1, 6), (2, 0)]) (-12)
    , GEQ (M.fromList [(1, 11), (1, 0), (1, -5), (1, -12), (1, -14), (2, 11)]) (-7)
    , GEQ (M.fromList [(1, -12), (1, -7), (1, -2), (2, -9), (1, 3), (1, 5), (1, -15), (2, 14)]) (-8)
    , GEQ (M.fromList [(1, 13), (1, 1), (1, -11), (2, 0)]) 5
    , LEQ (M.fromList [(1, -10), (1, -14), (1, 4), (1, -2), (1, -10), (1, -5), (1, -11)]) (-1)
    ]
  )

-- If we do not call simplifyPolyConstraints before we start the simplex algorithm, the following return a wrong solution
-- Correct solution is -2/9
testQuickCheck2 :: (ObjectiveFunction, [PolyConstraint])
testQuickCheck2 =
  ( Max (M.fromList [(1, -3), (2, 5)])
  ,
    [ LEQ (M.fromList [(2, -1), (1, -6), (2, 7)]) 4
    , LEQ (M.fromList [(1, 1), (2, -4), (3, 3)]) (-2)
    , LEQ (M.fromList [(2, 6), (1, -4), (2, 1)]) 0
    ]
  )

-- This test will fail if the objective function is not simplified
testQuickCheck3 :: (ObjectiveFunction, [PolyConstraint])
testQuickCheck3 =
  ( Min (M.fromList [(2, 0), (2, -4)])
  ,
    [ GEQ (M.fromList [(1, 5), (2, 4)]) (-4)
    , LEQ (M.fromList [(1, -1), (2, -1)]) 2
    , LEQ (M.fromList [(2, 1)]) 2
    , GEQ (M.fromList [(1, -5), (2, -1), (2, 1)]) (-5)
    ]
  )
