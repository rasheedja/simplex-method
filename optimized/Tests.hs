module Tests where

import Util as Util
import Simplex

-- From page 50 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 29, 1 = 3, 2 = 4, 
test1 :: ([(Integer, Rational)], [PolyConstraint])
test1 =
  (
    [(1, -3), (2, -5)],
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
test2 :: ([(Integer, Rational)], [PolyConstraint])
test2 =
  (
    [(1, 1), (2, -1), (3, 1)],
    [
      Util.LEQ [(1, 2), (2, -1), (3, 2)] 4,
      Util.LEQ [(1, 2), (2, -3), (3, 1)] (-5),
      Util.LEQ [(1, -1), (2, 1), (3, -2)] (-1)
    ]
  )

-- From page 49 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = -5, 3 = 2, 4 = 1, objVar was negated so actual val is 5
test3 :: ([(Integer, Rational)], [PolyConstraint])
test3 =
  (
    [(1, -1), (2, -1), (3, -2), (4, -1)],
    [
      Util.EQ [(1, 1), (3, 2), (4, -2)] 2,
      Util.EQ [(2, 1), (3, 1), (4, 4)] 6
    ]
  )

-- From page 52 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 20, 3 = 6, 4 = 16
test4 :: ([(Integer, Rational)], [PolyConstraint])
test4 =
  (
    [(3, -2), (4, 2), (5, 1)],
    [
      Util.EQ [(1, 1), (3, -2), (4, 1), (5, 1)] 4,
      Util.EQ [(2, 1), (3, 3), (4, -1), (5, 2)] 2
    ]
  )

-- From page 59 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 40/3, 1 = 0, 2 = 40/3
-- requires two phases
test5 :: ([(Integer, Rational)], [PolyConstraint])
test5 =
  (
    [(1, -2), (2, -1)],
    [
      Util.LEQ [(1, 4), (2, 1)] 150,
      Util.LEQ [(1, 2), (2, -3)] (-40)
    ]
  )

-- From page 59 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 120, 1 = 20, 2 = 0, 3 = 0, objVar was negated so actual val is -120
test6 :: ([(Integer, Rational)], [PolyConstraint])
test6 =
  (
    [(1, -6), (2, -4), (3, 2)],
    [
      Util.LEQ [(1, 1), (2, 1), (3, 4)] 20,
      Util.LEQ [(2, -5), (3, 5)] 100,
      Util.LEQ [(1, 1), (3, 1), (1, 1)] 400
    ]
  )

-- From page 59 of 'Linear and Integer Programming Made Easy'
-- Solution: obj = 250, 1 = 0, 2 = 50, 3 = 0
test7 :: ([(Integer, Rational)], [PolyConstraint])
test7 =
  (
    [(1, -3), (2, -5), (3, -2)],
    [
      Util.LEQ [(1, 5), (2, 1), (3, 4)] 50,
      Util.LEQ [(1, 1), (2, -1), (3, 1)] 150,
      Util.LEQ [(1, 2), (2, 1), (3, 2)] 100
    ]
  )
  