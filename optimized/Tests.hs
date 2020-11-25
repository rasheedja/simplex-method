module Tests where

import Util as Util
import Simplex

-- From page 50 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 29, 1 = 3, 2 = 4, 
test1 :: (ObjectiveFunction, [PolyConstraint])
test1 =
  (
    Max [(1, 3), (2, 5)],
    [
      Util.LEQ [(1, 3), (2, 1)] 15,
      Util.LEQ [(1, 1), (2, 1)] 7,
      Util.LEQ [(2, 1)] 4,
      Util.LEQ [(1, -1), (2, 2)] 6
    ]
  )

-- From https://www.eng.uwaterloo.ca/~syde05/phase1.pdf
-- Solution: obj = 3/5, 2 = 14/5, 3 = 17/5
-- requires two phases
test2 :: (ObjectiveFunction, [PolyConstraint])
test2 =
  (
    Max [(1, 1), (2, -1), (3, 1)],
    [
      Util.LEQ [(1, 2), (2, -1), (3, 2)] 4,
      Util.LEQ [(1, 2), (2, -3), (3, 1)] (-5),
      Util.LEQ [(1, -1), (2, 1), (3, -2)] (-1)
    ]
  )

-- From page 49 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = -5, 3 = 2, 4 = 1, objVar was negated so actual val is 5 wa
-- requires two phases
test3 :: (ObjectiveFunction, [PolyConstraint])
test3 =
  (
    Min [(1, 1), (2, 1), (3, 2), (4, 1)],
    [
      Util.EQ [(1, 1), (3, 2), (4, -2)] 2,
      Util.EQ [(2, 1), (3, 1), (4, 4)] 6
    ]
  )

-- Adapted from page 52 of 'Linear and Integer Programming Made Easy'
-- Removed variables which do not appear in the system (these should be artificial variables)
-- Solution: obj = 20, 3 = 6, 4 = 16 wq
test4 :: (ObjectiveFunction, [PolyConstraint])
test4 =
  (
    Max [(3, -2), (4, 2), (5, 1)],
    [
      Util.EQ [(3, -2), (4, 1), (5, 1)] 4,
      Util.EQ [(3, 3), (4, -1), (5, 2)] 2
    ]
  )

-- From page 59 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 150, 1 = 0, 2 = 150
-- requires two phases
test5 :: (ObjectiveFunction, [PolyConstraint])
test5 =
  (
    Max [(1, 2), (2, 1)],
    [
      Util.LEQ [(1, 4), (2, 1)] 150,
      Util.LEQ [(1, 2), (2, -3)] (-40)
    ]
  )

-- From page 59 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 120, 1 = 20, 2 = 0, 3 = 0, objVar was negated so actual val is -120
test6 :: (ObjectiveFunction, [PolyConstraint])
test6 =
  (
    Min [(1, -6), (2, -4), (3, 2)],
    [
      Util.LEQ [(1, 1), (2, 1), (3, 4)] 20,
      Util.LEQ [(2, -5), (3, 5)] 100,
      Util.LEQ [(1, 1), (3, 1), (1, 1)] 400
    ]
  )

-- From page 59 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 250, 1 = 0, 2 = 50, 3 = 0
test7 :: (ObjectiveFunction, [PolyConstraint])
test7 =
  (
    Max [(1, 3), (2, 5), (3, 2)],
    [
      Util.LEQ [(1, 5), (2, 1), (3, 4)] 50,
      Util.LEQ [(1, 1), (2, -1), (3, 1)] 150,
      Util.LEQ [(1, 2), (2, 1), (3, 2)] 100
    ]
  )
  
-- x1l = 1, x2l = 2, y = 3
testPolyPaver :: (ObjectiveFunction, [PolyConstraint])
testPolyPaver =
  (
    Max [(1 , 1), (2, 1), (3, 1)],
    [
        Util.LEQ [(1, dx1l), (2, dx2l), (3, (-1))] ((-yl) + (dx1l * x1l) + (dx2l * x2l)),
        Util.GEQ [(1, dx1r), (2, dx2r), (3, (-1))] ((-yr) + (dx1r * x1r) + (dx2r * x2r)),
        Util.LEQ [(3, 1)] 0
    ]
  )
  where
    x1l = 0.0
    x1r = 3.0
    x2l = 0.0
    x2r = 3.0
    dx1l = (-1)
    dx1r = (-1.1)
    dx2l = (-1.1)
    dx2r = (-1.2)
    yl = 1
    yr = 1.1
    -- system :: (S.ObjectiveFunction, [S.PolyConstraint])
    -- system = 
    --   (S.Max [(3, rational 1)],
    --   [
    --     S.GEQ [(1, rational 1)] x1lR,
    --     S.LEQ [(1, rational 1)] x1rR,
    --     S.LEQ [(2, rational 1)] x2lR,
    --     S.LEQ [(2, rational 1)] x2rR,
    --     S.LEQ [(1, dx1lR), (2, dx2lR), (3, (rational (-1)))] (-ylR + (dx1lR * x1lR) + (dx2lR * x2lR)),
    --     S.GEQ [(1, dx1rR), (2, dx2rR), (3, (rational (-1)))] (-yrR + (dx1rR * x1rR) + (dx2rR * x2rR))
    --   ]
    --   )
