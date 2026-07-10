# simplex-method

`simplex-method` is a Haskell library that implements the two-phase
[simplex method](https://en.wikipedia.org/wiki/Simplex_algorithm) in exact
rational arithmetic.

## Quick Overview

The `Linear.Simplex.Solver.TwoPhase` module contains phase-one feasibility
search, phase-two optimization, and a convenience `twoPhaseSimplex` entrypoint
that runs both phases.

Variables are identified by positive `Int` values, and all numeric values use
`Rational`:

```haskell
type Var = Int
type SimplexNum = Rational
type VarLitMapSum = Map Var SimplexNum
```

`VarLitMapSum` represents a linear expression. For example,
`Map.fromList [(1, 2), (2, -3)]` represents `2x1 - 3x2`.

## Constraints And Objectives

Constraints use `LEQ`, `GEQ`, and `EQ`:

```haskell
data PolyConstraint
  = LEQ {lhs :: VarLitMapSum, rhs :: SimplexNum}
  | GEQ {lhs :: VarLitMapSum, rhs :: SimplexNum}
  | EQ {lhs :: VarLitMapSum, rhs :: SimplexNum}
```

Objectives use `Max` or `Min`:

```haskell
data ObjectiveFunction
  = Max {objective :: VarLitMapSum}
  | Min {objective :: VarLitMapSum}
```

## Variable Domains

`twoPhaseSimplex` accepts a `VarDomainMap` so variables can be non-negative,
unbounded, lower-bounded, upper-bounded, or bounded on both sides:

```haskell
newtype VarDomainMap = VarDomainMap {unVarDomainMap :: Map Var VarDomain}

nonNegative :: VarDomain
unbounded :: VarDomain
lowerBoundOnly :: Rational -> VarDomain
upperBoundOnly :: Rational -> VarDomain
boundedRange :: Rational -> Rational -> VarDomain
```

Variables missing from the `VarDomainMap` are treated as `unbounded`. To keep
the traditional simplex assumption that every variable is non-negative, provide
`nonNegative` for every variable in the problem.

## Solving

The main entrypoint is:

```haskell
twoPhaseSimplex ::
  (MonadIO m, MonadLogger m) =>
  VarDomainMap ->
  [ObjectiveFunction] ->
  [PolyConstraint] ->
  m SimplexResult
```

`twoPhaseSimplex` can optimize multiple objectives over the same constraint set.
Pass an empty objective list to run phase one only.

Results are returned as:

```haskell
data SimplexResult = SimplexResult
  { feasibleSystem :: Maybe FeasibleSystem
  , objectiveResults :: [ObjectiveResult]
  }

data ObjectiveResult = ObjectiveResult
  { objectiveFunction :: ObjectiveFunction
  , outcome :: OptimisationOutcome
  }

data OptimisationOutcome
  = Optimal {varValMap :: VarLitMap}
  | Unbounded
```

For an `Optimal` result, `varValMap` contains values for original decision
variables. Variables with value zero may be absent from the map.

`twoPhaseSimplex` applies domain transformations before solving. Its
`feasibleSystem` field is therefore the feasible system for the transformed
non-negative problem, while each `Optimal.varValMap` is postprocessed back into
the original variable space.

`computeObjective` can be used to evaluate an objective against an optimal
variable map:

```haskell
computeObjective :: ObjectiveFunction -> Map Var Rational -> Rational
```

## Phase One And Phase Two

The lower-level phase functions do not apply `VarDomainMap` transformations.
Use them when the system is already in the non-negative variable space expected
by simplex, or call `twoPhaseSimplex` when solving a problem with variable
domain metadata.

For lower-level usage, phase one is exposed as:

```haskell
findFeasibleSolution ::
  (MonadIO m, MonadLogger m) =>
  [PolyConstraint] ->
  m (Maybe FeasibleSystem)
```

Phase two can optimize a feasible system:

```haskell
optimizeFeasibleSystem ::
  (MonadIO m, MonadLogger m) =>
  ObjectiveFunction ->
  FeasibleSystem ->
  m OptimisationOutcome
```

## Example

```haskell
import Control.Monad.Logger (LogLevel (LevelInfo), filterLogger, runStdoutLoggingT)
import qualified Data.Map as Map
import Linear.Simplex.Solver.TwoPhase (collectAllVars, computeObjective, twoPhaseSimplex)
import Linear.Simplex.Types
  ( ObjectiveFunction (Max)
  , ObjectiveResult (ObjectiveResult)
  , OptimisationOutcome (Optimal)
  , PolyConstraint (LEQ)
  , SimplexResult (SimplexResult)
  , VarDomainMap (VarDomainMap)
  , nonNegative
  )

example :: IO ()
example = do
  let objective = Max (Map.fromList [(1, 3), (2, 5)])
      constraints =
        [ LEQ (Map.fromList [(1, 3), (2, 1)]) 15
        , LEQ (Map.fromList [(1, 1), (2, 1)]) 7
        , LEQ (Map.fromList [(2, 1)]) 4
        , LEQ (Map.fromList [(1, -1), (2, 2)]) 6
        ]
      allVars = collectAllVars [objective] constraints
      domainMap = VarDomainMap $ Map.fromSet (const nonNegative) allVars

  SimplexResult feasibleSystem objectiveResults <-
    runStdoutLoggingT $
      filterLogger (\_ logLevel -> logLevel > LevelInfo) $
      twoPhaseSimplex domainMap [objective] constraints

  case (feasibleSystem, objectiveResults) of
    (Just _, [ObjectiveResult _ (Optimal varMap)]) -> do
      print varMap
      print $ computeObjective objective varMap
    (Just _, [_]) ->
      putStrLn "Objective is unbounded"
    _ ->
      putStrLn "System is infeasible"
```

Ignoring `Rational` formatting details, this prints an optimal assignment
equivalent to:

```haskell
Map.fromList [(1, 3), (2, 4)]
29
```

## Pretty Printing

`Linear.Simplex.Prettify` provides helpers for human-readable output:

```haskell
prettyShowVarLitMapSum :: VarLitMapSum -> String
prettyShowPolyConstraint :: PolyConstraint -> String
prettyShowObjectiveFunction :: ObjectiveFunction -> String
```

## Issues

Please share any bugs you find at
[github.com/rasheedja/simplex-method/issues](https://github.com/rasheedja/simplex-method/issues).
