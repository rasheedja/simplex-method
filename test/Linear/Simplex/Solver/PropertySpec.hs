{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Linear.Simplex.Solver.PropertySpec
-- Description : Solver-level property tests against a brute-force reference
-- Copyright   : (c) Junaid Rasheed, 2020-2023
-- License     : BSD-3
-- Maintainer  : jrasheed178@gmail.com
-- Stability   : experimental
--
-- The other QuickCheck properties in this test suite only exercise the
-- variable-transform helpers. The properties here check the solver's answer on
-- small bounded linear programs instead:
--
--   * the reported optimum is attained at a feasible point,
--   * its objective value equals the brute-force optimum computed from the
--     vertices of the feasible polytope (and feasibility agrees), and
--   * the result for one objective does not depend on the other objectives
--     requested alongside it.
--
-- Every generated variable has a finite domain, so every generated LP is
-- bounded and the ideal answer is always @Optimal@.
module Linear.Simplex.Solver.PropertySpec where

import Prelude hiding (EQ)

import Control.Monad.Logger (runNoLoggingT)
import Data.List (nub, sortOn)
import qualified Data.Map as M
import Data.Ratio ((%))
import qualified Data.Set as Set
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Property
  , choose
  , conjoin
  , counterexample
  , elements
  , frequency
  , ioProperty
  , oneof
  , property
  , sublistOf
  , suchThat
  , vectorOf
  )

import Linear.Simplex.Solver.TwoPhase (computeObjective, twoPhaseSimplex)
import Linear.Simplex.Types
  ( ObjectiveFunction (..)
  , ObjectiveResult (..)
  , OptimisationOutcome (..)
  , PolyConstraint (..)
  , SimplexNum
  , SimplexResult (..)
  , Var
  , VarDomain (..)
  , VarDomainMap (..)
  , VarLitMap
  , boundedRange
  )

-- | A small bounded linear program. Variable @i@ has the bounds at position
-- @i - 1@ of 'lpBounds', so every generated LP is bounded.
data GeneratedLP = GeneratedLP
  { lpNumVars :: Int
  , lpBounds :: [(SimplexNum, SimplexNum)]
  , lpObjectives :: [ObjectiveFunction]
  , lpConstraints :: [PolyConstraint]
  }
  deriving (Show, Eq)

-- | The solver's view of the generated bounds, one bounded domain per
-- variable.
lpDomainMap :: GeneratedLP -> VarDomainMap
lpDomainMap lp =
  VarDomainMap (M.fromList (zip [1 .. lp.lpNumVars] [boundedRange lo hi | (lo, hi) <- lp.lpBounds]))

-- | A case for the independence property: one system and two objectives to
-- compare.
newtype IndependenceCase = IndependenceCase (GeneratedLP, ObjectiveFunction, ObjectiveFunction)
  deriving (Show)

instance Arbitrary GeneratedLP where
  arbitrary = genLP
  shrink = shrinkLP

instance Arbitrary IndependenceCase where
  arbitrary = IndependenceCase <$> genLPWithTwoObjectives
  shrink _ = []

spec :: Spec
spec = describe "solver answers on small bounded linear programs" $ do
  describe "the brute-force reference solver" $ do
    it "finds the known optimum of Max 3x1 + 5x2" $ do
      bruteForceOptimum textbookLP (Max (M.fromList [(1, 3), (2, 5)])) `shouldBe` Just 29

    it "reports no feasible vertex for an infeasible system" $ do
      bruteForceOptimum infeasibleLP (Max M.empty) `shouldBe` Nothing

  describe "feasibility of the answer" $
    modifyMaxSuccess (const 300) $
      it "every Optimal answer is a feasible point inside every domain" $
        property $
          \(lp :: GeneratedLP) -> propFeasibleAnswer lp

  describe "optimality against brute force" $
    modifyMaxSuccess (const 300) $
      it "matches the brute-force optimum and agrees on feasibility" $
        property $
          \(lp :: GeneratedLP) -> propOptimalValue lp

  describe "per-objective independence" $
    modifyMaxSuccess (const 300) $
      it "an objective's result does not depend on the other requested objectives" $
        property $
          \(kase :: IndependenceCase) -> propObjectiveIndependence kase

-- Properties -----------------------------------------------------------------

-- | Every reported optimal point satisfies the constraints and the domains.
propFeasibleAnswer :: GeneratedLP -> Property
propFeasibleAnswer lp = ioProperty $ do
  result <- solveLP lp
  pure $
    counterexample (renderLP lp result) $
      case result of
        SimplexResult Nothing _ -> property True
        SimplexResult (Just _) results -> conjoin (map checkResult results)
  where
    checkResult (ObjectiveResult obj outcome) = case outcome of
      Unbounded ->
        counterexample
          ("a bounded LP reported Unbounded for objective " ++ show obj)
          (property False)
      Optimal varMap ->
        counterexample
          ("objective " ++ show obj ++ " returned the infeasible point " ++ show varMap)
          (property (isFeasiblePoint lp varMap))

-- | The solver's objective value equals the brute-force optimum, and the solver
-- reports a system feasible exactly when the brute force finds a feasible
-- vertex.
propOptimalValue :: GeneratedLP -> Property
propOptimalValue lp = ioProperty $ do
  result <- solveLP lp
  pure $
    counterexample (renderLP lp result) $
      case result of
        SimplexResult Nothing results ->
          conjoin
            [ counterexample
                ("solver says infeasible, but the brute force found the feasible points " ++ show vertices)
                (property (null vertices))
            , counterexample
                ("solver says infeasible, but it still returned objective results " ++ show results)
                (property (null results))
            ]
        SimplexResult (Just _) results ->
          conjoin
            [ counterexample
                "solver says feasible, but the brute force found no feasible vertex"
                (property (not (null vertices)))
            , counterexample
                ("expected one result per objective, got " ++ show results)
                (property (length results == length lp.lpObjectives))
            , conjoin (zipWith checkObjective lp.lpObjectives results)
            ]
  where
    vertices = bruteForceVertices lp

    checkObjective obj (ObjectiveResult _ outcome) = case outcome of
      Unbounded ->
        counterexample
          ("a bounded LP reported Unbounded for objective " ++ show obj)
          (property False)
      Optimal varMap -> case bruteForceOptimumFrom lp vertices obj of
        Nothing ->
          counterexample
            ( "objective "
                ++ show obj
                ++ " reported the optimum "
                ++ show (computeObjective obj varMap)
                ++ " but the brute force found no feasible point"
            )
            (property False)
        Just best ->
          counterexample
            ( "objective "
                ++ show obj
                ++ " reported "
                ++ show (computeObjective obj varMap)
                ++ " at "
                ++ show varMap
                ++ " but the brute-force optimum is "
                ++ show best
                ++ " over the vertices "
                ++ show vertices
            )
            (property (computeObjective obj varMap == best))

-- | The outcome for an objective is the same whether or not another objective
-- is optimized in the same call.
propObjectiveIndependence :: IndependenceCase -> Property
propObjectiveIndependence (IndependenceCase (lp, o1, o2)) = ioProperty $ do
  withBoth <- solveLP lp {lpObjectives = [o1, o2]}
  withOne <- solveLP lp {lpObjectives = [o2]}
  pure $
    counterexample
      ( unlines
          [ "constraints: " ++ show lp.lpConstraints
          , "domains: " ++ show (lpDomainMap lp)
          , "objective 1: " ++ show o1
          , "objective 2: " ++ show o2
          , "solving [o1, o2] gave " ++ show withBoth
          , "solving [o2] gave " ++ show withOne
          ]
      )
      (property (outcomeValueOf o2 withBoth == outcomeValueOf o2 withOne))

-- | The comparison key for one objective's result: whether the system was
-- infeasible, whether the objective was unbounded, or the optimal value.
data OutcomeValue = Infeasible | NoResult | UnboundedValue | OptimalValue SimplexNum
  deriving (Eq, Show)

outcomeValueOf :: ObjectiveFunction -> SimplexResult -> OutcomeValue
outcomeValueOf _ (SimplexResult Nothing _) = Infeasible
outcomeValueOf obj (SimplexResult (Just _) results) =
  case [valueOf r | r@(ObjectiveResult o _) <- results, o == obj] of
    (value : _) -> value
    [] -> NoResult
  where
    valueOf (ObjectiveResult _ Unbounded) = UnboundedValue
    valueOf (ObjectiveResult _ (Optimal varMap)) = OptimalValue (computeObjective obj varMap)

-- | Run the solver with logging switched off.
solveLP :: GeneratedLP -> IO SimplexResult
solveLP lp = runNoLoggingT $ twoPhaseSimplex (lpDomainMap lp) lp.lpObjectives lp.lpConstraints

renderLP :: GeneratedLP -> SimplexResult -> String
renderLP lp result =
  unlines
    [ "domains: " ++ show (lpDomainMap lp)
    , "objectives: " ++ show lp.lpObjectives
    , "constraints: " ++ show lp.lpConstraints
    , "solver result: " ++ show result
    ]

-- Feasibility ----------------------------------------------------------------

-- | Is the point feasible for the constraints and inside every domain?
-- Variables missing from the map count as zero.
isFeasiblePoint :: GeneratedLP -> VarLitMap -> Bool
isFeasiblePoint lp varMap =
  all (holds varMap) lp.lpConstraints && all inDomain (M.toList domainMap)
  where
    VarDomainMap domainMap = lpDomainMap lp
    holds vals (LEQ lhs rhs) = computeObjective (Max lhs) vals <= rhs
    holds vals (GEQ lhs rhs) = computeObjective (Max lhs) vals >= rhs
    holds vals (EQ lhs rhs) = computeObjective (Max lhs) vals == rhs
    inDomain (v, Bounded mLower mUpper) =
      let x = M.findWithDefault 0 v varMap
      in  maybe True (x >=) mLower && maybe True (x <=) mUpper

-- Brute-force reference ------------------------------------------------------

-- | Enumerate the vertices of the feasible polytope: every intersection of as
-- many constraint and domain-bound hyperplanes as there are variables, keeping
-- the points that are feasible. Because every variable is bounded, the feasible
-- region is a polytope, so it is non-empty exactly when it has a vertex and a
-- linear objective attains its extremes at a vertex.
bruteForceVertices :: GeneratedLP -> [VarLitMap]
bruteForceVertices lp = nub (filter (isFeasiblePoint lp) candidates)
  where
    vars = [1 .. lp.lpNumVars]
    candidates =
      [ M.fromList (zip vars values)
      | combo <- subsetsOfSize (lp.lpNumVars) (hyperplanes lp)
      , Just values <-
          [ solveSquare
              [ [M.findWithDefault 0 v coeffs | v <- vars]
              | (coeffs, _) <- combo
              ]
              [rhs | (_, rhs) <- combo]
          ]
      ]

-- | The hyperplanes bounding the feasible region: every constraint, plus the
-- lower and upper bound of every variable.
hyperplanes :: GeneratedLP -> [(VarLitMap, SimplexNum)]
hyperplanes lp = nub (constraintHyperplanes ++ boundHyperplanes)
  where
    constraintHyperplanes = [(c.lhs, c.rhs) | c <- lp.lpConstraints]
    VarDomainMap domainMap = lpDomainMap lp
    boundHyperplanes =
      concat
        [ maybe [] (\l -> [(M.singleton v 1, l)]) mLower
          ++ maybe [] (\u -> [(M.singleton v 1, u)]) mUpper
        | (v, Bounded mLower mUpper) <- M.toList domainMap
        ]

-- | The best objective value over the feasible vertices, or 'Nothing' when the
-- system is infeasible.
bruteForceOptimum :: GeneratedLP -> ObjectiveFunction -> Maybe SimplexNum
bruteForceOptimum lp = bruteForceOptimumFrom lp (bruteForceVertices lp)

bruteForceOptimumFrom :: GeneratedLP -> [VarLitMap] -> ObjectiveFunction -> Maybe SimplexNum
bruteForceOptimumFrom _ vertices obj = case vertices of
  [] -> Nothing
  points -> Just (pick (map (computeObjective obj) points))
  where
    pick = case obj of
      Max _ -> maximum
      Min _ -> minimum

-- Exact linear algebra -------------------------------------------------------

-- | Solve @A x = b@ exactly for a square system. 'Nothing' when the rows are
-- linearly dependent, i.e. the hyperplanes do not meet in a single point.
solveSquare :: [[SimplexNum]] -> [SimplexNum] -> Maybe [SimplexNum]
solveSquare matrix rhs
  | length matrix /= n
      || any ((/= n) . length) matrix
      || length rhs /= n =
      Nothing
  | otherwise =
      fmap
        (backSubstitute n)
        (rowEchelon n (zipWith (++) matrix (map (: []) rhs)))
  where
    n = length matrix

-- | Reduce the augmented matrix to unit-pivot upper triangular form, pivoting
-- on row @i@ for column @i@. 'Nothing' if some column has no usable pivot.
rowEchelon :: Int -> [[SimplexNum]] -> Maybe [[SimplexNum]]
rowEchelon n augmented = go 0 augmented
  where
    go i _ | i >= n = Just []
    go i rows = do
      (pivotRow, otherRows) <- pickPivot i rows
      let normalized = map (/ (pivotRow !! i)) pivotRow
      rest <- go (i + 1) (map (eliminateWith i normalized) otherRows)
      pure (normalized : rest)

    pickPivot _ [] = Nothing
    pickPivot i (row : rows)
      | row !! i /= 0 = Just (row, rows)
      | otherwise =
          fmap (\(pivotRow, others) -> (pivotRow, row : others)) (pickPivot i rows)

    -- Subtract the multiple of the pivot row that clears column @i@.
    eliminateWith i pivotRow row =
      let factor = row !! i
      in  zipWith (\x y -> x - factor * y) row pivotRow

backSubstitute :: Int -> [[SimplexNum]] -> [SimplexNum]
backSubstitute n rows = [solveFor i | i <- [0 .. n - 1]]
  where
    solveFor i =
      last (rows !! i)
        - sum [rows !! i !! j * solveFor j | j <- [i + 1 .. n - 1]]

-- | Every sublist of the given size.
subsetsOfSize :: Int -> [a] -> [[a]]
subsetsOfSize k _ | k < 0 = []
subsetsOfSize 0 _ = [[]]
subsetsOfSize _ [] = []
subsetsOfSize k (x : xs) =
  map (x :) (subsetsOfSize (k - 1) xs) ++ subsetsOfSize k xs

-- Generators -----------------------------------------------------------------

maxGeneratedVars :: Int
maxGeneratedVars = 3

maxGeneratedConstraints :: Int
maxGeneratedConstraints = 4

-- | Small nonzero rationals ordered by magnitude, so that QuickCheck shrinks
-- towards simple coefficients such as 1 and 1/3.
smallRationals :: [SimplexNum]
smallRationals =
  nub (sortOn abs [fromIntegral n % fromIntegral d | n <- [-5 .. 5 :: Int], n /= 0, d <- [1 .. 3 :: Int]])

genCoefficient :: Gen SimplexNum
genCoefficient = elements smallRationals

-- | A domain @boundedRange lo hi@ with @-5 <= lo <= hi <= 5@, sometimes with
-- @lo == hi@.
genDomain :: Gen (SimplexNum, SimplexNum)
genDomain = do
  lo <- choose (-5 :: Int, 5)
  hi <- frequency [(1, pure lo), (5, choose (lo, 5))]
  pure (fromIntegral lo, fromIntegral hi)

-- | A non-empty coefficient map over a random subset of the given variables,
-- with every coefficient nonzero.
genVarLitMap :: [Var] -> Gen VarLitMap
genVarLitMap vars = do
  chosen <- sublistOf vars `suchThat` (not . null)
  M.fromList <$> mapM (\v -> (v,) <$> genCoefficient) chosen

genRhs :: Gen SimplexNum
genRhs = fromIntegral <$> choose (-6 :: Int, 6)

genConstraint :: [Var] -> Gen PolyConstraint
genConstraint vars = do
  lhs <- genVarLitMap vars
  rhs <- genRhs
  oneof [pure (LEQ lhs rhs), pure (GEQ lhs rhs), pure (EQ lhs rhs)]

-- | A random objective: usually a linear function, sometimes the zero objective
-- @Max M.empty@, and about half the time a minimization.
genObjective :: [Var] -> Gen ObjectiveFunction
genObjective vars = do
  isZero <- frequency [(1, pure True), (9, pure False)]
  if isZero
    then pure (Max M.empty)
    else do
      lhs <- genVarLitMap vars
      direction <- elements [True, False]
      pure (if direction then Max lhs else Min lhs)

-- | Variables, domains and constraints, without any objectives.
genSystem :: Gen (Int, [(SimplexNum, SimplexNum)], [PolyConstraint])
genSystem = do
  numVars <- choose (1, maxGeneratedVars)
  let vars = [1 .. numVars]
  bounds <- vectorOf numVars genDomain
  numConstraints <- choose (0, maxGeneratedConstraints)
  constraints <- vectorOf numConstraints (genConstraint vars)
  pure (numVars, bounds, constraints)

genLP :: Gen GeneratedLP
genLP = do
  (numVars, bounds, constraints) <- genSystem
  numObjectives <- choose (1, 3)
  objectives <- vectorOf numObjectives (genObjective [1 .. numVars])
  pure (mkLP numVars bounds objectives constraints)

-- | A system with two objective functions and a non-empty feasible region, for
-- the independence property. Infeasible systems make that comparison trivial,
-- so they are retried.
genLPWithTwoObjectives :: Gen (GeneratedLP, ObjectiveFunction, ObjectiveFunction)
genLPWithTwoObjectives = do
  (numVars, bounds, constraints) <- genSystem
  let vars = [1 .. numVars]
  o1 <- genObjective vars
  o2 <- genObjective vars
  let lp = mkLP numVars bounds [o1, o2] constraints
  if null (bruteForceVertices lp)
    then genLPWithTwoObjectives
    else pure (lp, o1, o2)

-- | Assemble a generated LP and restate the domain of every variable that no
-- objective or constraint mentions yet. Every variable is bounded, so every LP
-- is bounded; keeping every variable mentioned means a variable missing from an
-- answer really does mean zero rather than being ignored by the solver.
mkLP :: Int -> [(SimplexNum, SimplexNum)] -> [ObjectiveFunction] -> [PolyConstraint] -> GeneratedLP
mkLP numVars bounds objectives constraints =
  padConstraints
    GeneratedLP
      { lpNumVars = numVars
      , lpBounds = bounds
      , lpObjectives = objectives
      , lpConstraints = constraints
      }

-- | Add a constraint restating the domain's upper bound for every variable that
-- nothing mentions. Such a constraint is implied by the domain, so it leaves
-- the feasible region unchanged. Idempotent: the added constraints themselves
-- mention the previously missing variables.
padConstraints :: GeneratedLP -> GeneratedLP
padConstraints lp =
  if null padding
    then lp
    else lp {lpConstraints = lp.lpConstraints ++ padding}
  where
    mentioned =
      Set.unions
        ( map (M.keysSet . (.objective)) lp.lpObjectives
            ++ map (M.keysSet . (.lhs)) lp.lpConstraints
        )
    padding =
      [ LEQ (M.singleton v 1) hi
      | (v, (_, hi)) <- zip [1 .. lp.lpNumVars] lp.lpBounds
      , v `Set.notMember` mentioned
      ]

-- | Shrink towards smaller systems: fewer variables, then fewer constraints,
-- then fewer objectives. Restating a variable's domain after dropping a
-- constraint can reproduce the very LP being shrunk, so such candidates are
-- discarded; QuickCheck would otherwise recurse on them forever.
shrinkLP :: GeneratedLP -> [GeneratedLP]
shrinkLP lp =
  filter (\candidate -> candidate /= lp && not (null (lpObjectives candidate))) $
    concat
      [ [restrictToVars n lp | n <- [1 .. lpNumVars lp - 1]]
      , [padConstraints lp {lpConstraints = removeAt i (lpConstraints lp)} | i <- indicesOf (lpConstraints lp)]
      , [ padConstraints lp {lpObjectives = removeAt i (lpObjectives lp)}
        | i <- indicesOf (lpObjectives lp)
        , length (lpObjectives lp) > 1
        ]
      ]

-- | Drop the objectives and constraints that mention variables above @n@, then
-- restate the remaining variables' domains.
restrictToVars :: Int -> GeneratedLP -> GeneratedLP
restrictToVars n lp =
  padConstraints
    lp
      { lpNumVars = n
      , lpBounds = take n (lpBounds lp)
      , lpObjectives = [obj | obj <- lpObjectives lp, all (<= n) (M.keys (obj.objective))]
      , lpConstraints = [c | c <- lpConstraints lp, all (<= n) (M.keys c.lhs)]
      }

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i + 1) xs

indicesOf :: [a] -> [Int]
indicesOf xs = [0 .. length xs - 1]

-- Fixtures -------------------------------------------------------------------

-- | Max 3x1 + 5x2 over the LP from page 50 of 'Linear and Integer Programming
-- Made Easy': the optimum is 29 at (3, 4).
textbookLP :: GeneratedLP
textbookLP =
  GeneratedLP
    { lpNumVars = 2
    , lpBounds = [(0, 10), (0, 10)]
    , lpObjectives = [Max (M.fromList [(1, 3), (2, 5)])]
    , lpConstraints =
        [ LEQ (M.fromList [(1, 3), (2, 1)]) 15
        , LEQ (M.fromList [(1, 1), (2, 1)]) 7
        , LEQ (M.singleton 2 1) 4
        ]
    }

-- | x1 <= 1 and x1 >= 2: no feasible point.
infeasibleLP :: GeneratedLP
infeasibleLP =
  GeneratedLP
    { lpNumVars = 2
    , lpBounds = [(0, 5), (0, 5)]
    , lpObjectives = [Max M.empty]
    , lpConstraints =
        [ LEQ (M.singleton 1 1) 1
        , GEQ (M.singleton 1 1) 2
        ]
    }
