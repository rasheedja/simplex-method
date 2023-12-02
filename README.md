# simplex-method

`simplex-method` is a Haskell library that implements the two-phase [simplex method](https://en.wikipedia.org/wiki/Simplex_algorithm) in exact rational arithmetic.

## Quick Overview

The `Linear.Simplex.Solver.TwoPhase` module contain both phases of the two-phase simplex method.

### Phase One

Phase one is implemented by `findFeasibleSolution`:

```haskell
findFeasibleSolution :: (MonadIO m, MonadLogger m) => [PolyConstraint] -> m (Maybe FeasibleSystem)
```

`findFeasibleSolution` takes a list of `PolyConstraint`s.
The `PolyConstraint` type, as well as other custom types required by this library, are defined in the `Linear.Simplex.Types` module.
`PolyConstraint` is defined as:

```haskell
data PolyConstraint
  = LEQ {lhs :: VarLitMapSum, rhs :: SimplexNum}
  | GEQ {lhs :: VarLitMapSum, rhs :: SimplexNum}
  | EQ {lhs :: VarLitMapSum, rhs :: SimplexNum}
  deriving (Show, Read, Eq, Generic)
```

`SimplexNum` is an alias for `Rational`, and `VarLitMapSum` is an alias for `VarLitMap`, which is an alias for `Map Var SimplexNum`.
`Var` is an alias of `Int`.

A `VarLitMapSum` is read as `Integer` variables mapped to their `Rational` coefficients, with an implicit `+` between each entry.
For example: `Map.fromList [(1, 2), (2, (-3)), (1, 3)]` is equivalent to `(2x1 + (-3x2) + 3x1)`.

And a `PolyConstraint` is an inequality/equality where the LHS is a `VarLitMapSum` and the RHS is a `Rational`.
For example: `LEQ (Map.fromList [(1, 2), (2, (-3)), (1, 3)] 60)` is equivalent to `(2x1 + (-3x2) + 3x1) <= 60`.

Passing a `[PolyConstraint]` to `findFeasibleSolution` will return a `FeasibleSystem` if a feasible solution exists:

```haskell
data FeasibleSystem = FeasibleSystem
  { dict :: Dict
  , slackVars :: [Var]
  , artificialVars :: [Var]
  , objectiveVar :: Var
  }
  deriving (Show, Read, Eq, Generic)
```

```haskell
type Dict = M.Map Var DictValue

data DictValue = DictValue
  { varMapSum :: VarLitMapSum
  , constant :: SimplexNum
  }
  deriving (Show, Read, Eq, Generic)
```

`Dict` can be thought of as a set of equations, where the key represents a basic variable on the LHS of the equation
that is equal to the RHS represented as a `DictValue` value.

### Phase Two

`optimizeFeasibleSystem` performs phase two of the simplex method, and has the type:

```haskell

optimizeFeasibleSystem :: (MonadIO m, MonadLogger m) => ObjectiveFunction -> FeasibleSystem -> m (Maybe Result)

data ObjectiveFunction = Max {objective :: VarLitMapSum} | Min {objective :: VarLitMapSum}

data Result = Result
  { objectiveVar :: Var
  , varValMap :: VarLitMap
  }
  deriving (Show, Read, Eq, Generic)
```

We give `optimizeFeasibleSystem` an `ObjectiveFunction` along with a `FeasibleSystem`.

### Two-Phase Simplex

`twoPhaseSimplex` performs both phases of the simplex method.
It has the type:

```haskell
twoPhaseSimplex :: (MonadIO m, MonadLogger m) => ObjectiveFunction -> [PolyConstraint] -> m (Maybe Result)
```

### Extracting Results

The result of the objective function is present in the returned `Result` type of both `twoPhaseSimplex` and `optimizeFeasibleSystem`, but this can be difficult to grok in systems with many variables, so the following function will extract the value of the objective function for you.

```haskell
dictionaryFormToTableau :: Dict -> Tableau
```

There are similar functions for `DictionaryForm` as well as other custom types in the module `Linear.Simplex.Util`.

## Example

```haskell
exampleFunction :: (ObjectiveFunction, [PolyConstraint])
exampleFunction =
  (
    Max {objective = Map.fromList [(1, 3), (2, 5)]},      -- 3x1 + 5x2
    [
      LEQ {lhs = Map.fromList [(1, 3), (2, 1)], rhs = 15}, -- 3x1 + x2 <= 15 
      LEQ {lhs = Map.fromList [(1, 1), (2, 1)], rhs = 7},  -- x1 + x2 <= 7
      LEQ {lhs = Map.fromList [(2, 1)], rhs = 4},          -- x2 <= 4
      LEQ {lhs = Map.fromList [(1, -1), (2, 2)], rhs = 6}  -- -x1 + 2x2 <= 6
    ]
  )

twoPhaseSimplex (fst exampleFunction) (snd exampleFunction)
```

The result of the call above is:

```haskell
Just 
  (Result
    { objectiveVar = 7 -- Integer representing objective function
    , varValMap = M.fromList  
      [ (7, 29) -- Value for variable 7, so max(3x1 + 5x2) = 29.
      , (1, 3) -- Value for variable 1, so x1 = 3 
      , (2, 4) -- Value for variable 2, so x2 = 4
      ]
    }
  )
```

There are many more examples in test/TestFunctions.hs.
You may use `prettyShowVarConstMap`, `prettyShowPolyConstraint`, and `prettyShowObjectiveFunction` to convert these tests into a more human-readable format.

## Issues

Please share any bugs you find [here](https://github.com/rasheedja/simplex-haskell/issues).
