module Tests where

import Util

runTests =
  optSimplex test1 0 Max
  optSimplex test2 0 Min
where
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