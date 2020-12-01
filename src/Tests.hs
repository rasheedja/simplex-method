module Tests where

import Simplex

-- From page 50 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 29, 1 = 3, 2 = 4, 
test1 :: (ObjectiveFunction, [PolyConstraint])
test1 =
  (
    Max [(1, 3), (2, 5)],
    [
      Simplex.LEQ [(1, 3), (2, 1)] 15,
      Simplex.LEQ [(1, 1), (2, 1)] 7,
      Simplex.LEQ [(2, 1)] 4,
      Simplex.LEQ [(1, -1), (2, 2)] 6
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
      Simplex.LEQ [(1, 2), (2, -1), (3, 2)] 4,
      Simplex.LEQ [(1, 2), (2, -3), (3, 1)] (-5),
      Simplex.LEQ [(1, -1), (2, 1), (3, -2)] (-1)
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
      Simplex.EQ [(1, 1), (3, 2), (4, -2)] 2,
      Simplex.EQ [(2, 1), (3, 1), (4, 4)] 6
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
      Simplex.EQ [(3, -2), (4, 1), (5, 1)] 4,
      Simplex.EQ [(3, 3), (4, -1), (5, 2)] 2
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
      Simplex.LEQ [(1, 4), (2, 1)] 150,
      Simplex.LEQ [(1, 2), (2, -3)] (-40)
    ]
  )

-- From page 59 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 120, 1 = 20, 2 = 0, 3 = 0, objVar was negated so actual val is -120
test6 :: (ObjectiveFunction, [PolyConstraint])
test6 =
  (
    Min [(1, -6), (2, -4), (3, 2)],
    [
      Simplex.LEQ [(1, 1), (2, 1), (3, 4)] 20,
      Simplex.LEQ [(2, -5), (3, 5)] 100,
      Simplex.LEQ [(1, 1), (3, 1), (1, 1)] 400
    ]
  )

-- From page 59 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 250, 1 = 0, 2 = 50, 3 = 0
test7 :: (ObjectiveFunction, [PolyConstraint])
test7 =
  (
    Max [(1, 3), (2, 5), (3, 2)],
    [
      Simplex.LEQ [(1, 5), (2, 1), (3, 4)] 50,
      Simplex.LEQ [(1, 1), (2, -1), (3, 1)] 150,
      Simplex.LEQ [(1, 2), (2, 1), (3, 2)] 100
    ]
  )
  
-- x1l = 1, x2l = 2, y = 3 
testPolyPaver :: (ObjectiveFunction, [PolyConstraint])
testPolyPaver =
  (
    Min [(2 , 1)],
    [
        Simplex.LEQ [(1, dx1l), (2, dx2l), (3, (-1))] ((-yl) + (dx1l * x1l) + (dx2l * x2l)), -- -4, This will need an artificial variable
        Simplex.GEQ [(1, dx1r), (2, dx2r), (3, (-1))] ((-yr) + (dx1r * x1l) + (dx2r * x2l)), -- -5
        -- Simplex.GEQ [(1, 1)] x1l, -- don't need variable >= 0, already assumed
        Simplex.LEQ [(1, 1)] x1r,
        -- Simplex.GEQ [(2, 1)] x2l,
        Simplex.LEQ [(2, 1)] x2r,
        Simplex.EQ [(3, 1)] 0 -- When this is LEQ, it cannot solve phase 1.
                           -- This is because all variables in the system must be >= 0
                           -- To solve this, we can split y (3) into two variables, y1 (4) and y2 (5)
                           -- With y(3) = y1(4) - y2(5)

                           -- We can also simply negate the y, the constraint that -y >= 0 is already assumed
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

-- x1l = 1, x2l = 2, y = 3
testPolyPaver2 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaver2 =
  (
    Max [(1 , 1), (2, 1)],
    [
        Simplex.LEQ [(1, dx1l), (2, dx2l), (3, (-1))] ((-yl) + (dx1l * x1l) + (dx2l * x2l)), -- -4, This will need an artificial variable
        Simplex.GEQ [(1, dx1r), (2, dx2r), (3, (-1))] ((-yr) + (dx1r * x1l) + (dx2r * x2l)), -- -5
        -- Simplex.GEQ [(1, 1)] x1l, -- It doesn't like having rhs be 0 for a GEQ, probably because the simplex tableau should ignore this
        Simplex.LEQ [(1, 1)] x1r,
        -- Simplex.GEQ [(2, 1)] x2l,
        Simplex.LEQ [(2, 1)] x2r,
        Simplex.EQ [(3, 1)] 0
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

testPolyPaver3 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaver3 =
  (
    Max [(2 , 1)],
    [
        Simplex.LEQ [(1, dx1l), (2, dx2l), (3, (-1))] ((-yl) + (dx1l * x1l) + (dx2l * x2l)), -- -4, This will need an artificial variable
        Simplex.GEQ [(1, dx1r), (2, dx2r), (3, (-1))] ((-yr) + (dx1r * x1l) + (dx2r * x2l)), -- -5
        -- Simplex.GEQ [(1, 1)] x1l, -- It doesn't like having rhs be 0 for a GEQ, probably because the simplex tableau should ignore this
        Simplex.LEQ [(1, 1)] x1r,
        -- Simplex.GEQ [(2, 1)] x2l,
        Simplex.LEQ [(2, 1)] x2r,
        Simplex.EQ [(3, 1)] 0
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

-- x1l = 1, x2l = 2, y1 = 3, y2 = 4
testPolyPaverTwoYs :: (ObjectiveFunction, [PolyConstraint])
testPolyPaverTwoYs =
  (
    Max [(3 , 1), (4, -1)],
    [
        Simplex.LEQ [(1, dx1l), (2, dx2l), (3, -1), (4, 1)] ((-yl) + (dx1l * x1l) + (dx2l * x2l)), -- -4, This will need an artificial variable
        Simplex.GEQ [(1, dx1r), (2, dx2r), (3, -1), (4, 1)] ((-yr) + (dx1r * x1r) + (dx2r * x2r)), -- -5
        -- Simplex.GEQ [(1, 1)] x1l, -- It doesn't like having rhs be 0 for a GEQ, probably because the simplex tableau should ignore this
        Simplex.LEQ [(1, 1)] x1r,
        -- Simplex.GEQ [(2, 1)] x2l,
        Simplex.LEQ [(2, 1)] x2r,
        Simplex.LEQ [(3, 1), (4, -1)] 0 -- When this is LEQ, it cannot solve phase 1.
                           -- This is because all variables in the system must be >= 0
                           -- To solve this, we can split y (3) into two variables, y1 (4) and y2 (5)
                           -- With y(3) = y1(4) - y2(5)
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

-- x1l = 1, x2l = 2, y1 = 3, y2 = 4
testPolyPaverTwoYs2 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaverTwoYs2 =
  (
    Max [(3 , 1), (4, -1)],
    [
        Simplex.LEQ [(1, dx1l), (2, dx2l), (3, -1), (4, 1)] ((-yl) + (dx1l * x1l) + (dx2l * x2l)), -- -4, This will need an artificial variable
        Simplex.GEQ [(1, dx1r), (2, dx2r), (3, -1), (4, 1)] ((-yr) + (dx1r * x1r) + (dx2r * x2r)), -- -5
        -- Simplex.GEQ [(1, 1)] x1l, -- It doesn't like having rhs be 0 for a GEQ, probably because the simplex tableau should ignore this
        Simplex.LEQ [(1, 1)] x1r,
        -- Simplex.GEQ [(2, 1)] x2l,
        Simplex.LEQ [(2, 1)] x2r,
        Simplex.LEQ [(3, 1), (4, -1)] 0 -- When this is LEQ, it cannot solve phase 1.
                           -- This is because all variables in the system must be >= 0
                           -- To solve this, we can split y (3) into two variables, y1 (4) and y2 (5)
                           -- With y(3) = y1(4) - y2(5)
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

-- x1l = 1, x2l = 2, y1 = 3, y2 = 4
testPolyPaverTwoYs3 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaverTwoYs3 =
  (
    Max [(3 , 1), (4, -1)],
    [
        Simplex.LEQ [(1, dx1l), (2, dx2l), (3, -1), (4, 1)] ((-yl) + (dx1l * x1l) + (dx2l * x2l)), -- -4, This will need an artificial variable
        Simplex.GEQ [(1, dx1r), (2, dx2r), (3, -1), (4, 1)] ((-yr) + (dx1r * x1r) + (dx2r * x2r)), -- -5
        -- Simplex.GEQ [(1, 1)] x1l, -- It doesn't like having rhs be 0 for a GEQ, probably because the simplex tableau should ignore this
        Simplex.LEQ [(1, 1)] x1r,
        -- Simplex.GEQ [(2, 1)] x2l,
        Simplex.LEQ [(2, 1)] x2r,
        Simplex.LEQ [(3, 1), (4, -1)] 0 -- When this is LEQ, it cannot solve phase 1.
                           -- This is because all variables in the system must be >= 0
                           -- To solve this, we can split y (3) into two variables, y1 (4) and y2 (5)
                           -- With y(3) = y1(4) - y2(5)
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

-- Should be infeasible
testPolyPaverTwoFs :: (ObjectiveFunction, [PolyConstraint])
testPolyPaverTwoFs =
  (
    Min [(2 , 1)],
    [
        Simplex.LEQ [(1, f1dx1l), (2, f1dx2l), (3, (-1))] ((-f1yl) + (f1dx1l * x1l) + (f1dx2l * x2l)), -- -4, This will need an artificial variable
        Simplex.GEQ [(1, f1dx1r), (2, f1dx2r), (3, (-1))] ((-f1yr) + (f1dx1r * x1l) + (f1dx2r * x2l)),        
        Simplex.LEQ [(1, f2dx1l), (2, f2dx2l), (4, (-1))] ((-f2yl) + (f2dx1l * x1l) + (f2dx2l * x2l)),
        Simplex.GEQ [(1, f2dx1r), (2, f2dx2r), (4, (-1))] ((-f2yr) + (f2dx1r * x1l) + (f2dx2r * x2l)), 
        -- Simplex.GEQ [(1, 1)] x1l, -- don't need variable >= 0, already assumed
        Simplex.LEQ [(1, 1)] x1r,
        -- Simplex.GEQ [(2, 1)] x2l,
        Simplex.LEQ [(2, 1)] x2r,
        Simplex.EQ [(3, 1)] 0,
        Simplex.EQ [(4, 1)] 0 -- When this is LEQ, it cannot solve phase 1.
                           -- This is because all variables in the system must be >= 0
                           -- To solve this, we can split y (3) into two variables, y1 (4) and y2 (5)
                           -- With y(3) = y1(4) - y2(5)

                           -- We can also simply negate the y, the constraint that -y >= 0 is already assumed
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

-- Should be feasible
testPolyPaverTwoFs2 :: (ObjectiveFunction, [PolyConstraint])
testPolyPaverTwoFs2 =
  (
    Min [(2 , 1)],
    [
        Simplex.LEQ [(1, f1dx1l), (2, f1dx2l), (3, (-1))] ((-f1yl) + (f1dx1l * x1l) + (f1dx2l * x2l)), -- -4, This will need an artificial variable
        Simplex.GEQ [(1, f1dx1r), (2, f1dx2r), (3, (-1))] ((-f1yr) + (f1dx1r * x1l) + (f1dx2r * x2l)),        
        Simplex.LEQ [(1, f2dx1l), (2, f2dx2l), (4, (-1))] ((-f2yl) + (f2dx1l * x1l) + (f2dx2l * x2l)),
        Simplex.GEQ [(1, f2dx1r), (2, f2dx2r), (4, (-1))] ((-f2yr) + (f2dx1r * x1l) + (f2dx2r * x2l)), 
        -- Simplex.GEQ [(1, 1)] x1l, -- don't need variable >= 0, already assumed
        Simplex.LEQ [(1, 1)] x1r,
        -- Simplex.GEQ [(2, 1)] x2l,
        Simplex.LEQ [(2, 1)] x2r,
        Simplex.EQ [(3, 1)] 0, -- When this is LEQ, it cannot solve phase 1.
        Simplex.EQ [(4, 1)] 0 -- When this is LEQ, it cannot solve phase 1.
                           -- This is because all variables in the system must be >= 0
                           -- To solve this, we can split y (3) into two variables, y1 (4) and y2 (5)
                           -- With y(3) = y1(4) - y2(5)

                           -- We can also simply negate the y, the constraint that -y >= 0 is already assumed
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
