# linear-simplex-haskell

`linear-simplex-haskell` is a library that implements the two-phase [simplex method](https://en.wikipedia.org/wiki/Simplex_algorithm).

## Quick Overview

The `Linear.Simplex` module contain both phases of the simplex method.

### Phase One

Phase one is implemented by `findFeasibleSolution`:

```haskell
findFeasibleSolution :: [PolyConstraint] -> Maybe (DictionaryForm, [Integer], [Integer], Integer)
```

`findFeasibleSolution` takes a list of `PolyConstraint`s.
The `PolyConstraint` type, as well as other custom types required by this library, are defined in the `Linear.Type` module.
`PolyConstraint` is defined as:

```haskell
data PolyConstraint =
  LEQ VarConstMap Rational      | 
  GEQ VarConstMap Rational      | 
  EQ  VarConstMap Rational       deriving (Show, Eq);
```

And `VarConstMap` is defined as:

```haskell
type VarConstMap = [(Integer, Rational)]
```

A `VarConstMap` is treated as a list of `Integer` variables mapped to their `Rational` coefficients, with an implicit `+` between each element in the list.
For example: `[(1, 2), (2, (-3)), (1, 3)]` is equivalent to `(2x1 + (-3x2) + 3x1)`.

And a `PolyConstraint` is an inequality/equality where the LHS is a `VarConstMap` and the RHS is a `Rational`.
For example: `LEQ [(1, 2), (2, (-3)), (1, 3)] 60` is equivalent to `(2x1 + (-3x2) + 3x1) <= 60`.

Passing a `[PolyConstraint]` to `findFeasibleSolution` will return a feasible solution if it exists as well as a list of slack variables, artificial variables, and a variable that can be safely used to represent the objective for phase two.
`Nothing` is returned if the given `[PolyConstraint]` is infeasible.
The feasible system is returned as the type `DictionaryForm`:

```haskell
type DictionaryForm = [(Integer, VarConstMap)]
```

`DictionaryForm` can be thought of as a list of equations, where the `Integer` represents a basic variable on the LHS that is equal to the RHS represented as a `VarConstMap`. In this `VarConstMap`, the `Integer` -1 is used internally to represent a `Rational` number.

### Phase Two

`optimizeFeasibleSystem` performs phase two of the simplex method, and has the type:

```haskell
data ObjectiveFunction = Max VarConstMap | Min VarConstMap deriving (Show, Eq)

optimizeFeasibleSystem :: ObjectiveFunction -> DictionaryForm -> [Integer] -> [Integer] -> Integer -> Maybe (Integer, [(Integer, Rational)])
```

We first pass an `ObjectiveFunction`.
Then we give a feasible system in `DictionaryForm`, a list of slack variables, a list of artificial variables, and a variable to represent the objective.
`optimizeFeasibleSystem` Maximizes/Minimizes the linear equation represented as a `VarConstMap` in the given `ObjectiveFunction`.
The first item of the returned pair is the `Integer` variable representing the objective.
Ths second item is a map of `Integer` variables with their optimized values.
If a variable is not present in this map, the variable is equal to 0.

### Two-Phase Simplex
`twoPhaseSimplex` performs both phases of the simplex method.
It has the type:
```haskell
twoPhaseSimplex :: ObjectiveFunction -> [PolyConstraint] -> Maybe (Integer, [(Integer, Rational)])
```
The return type is the same as that of `optimizeFeasibleSystem`

### Extracting Results
The result of the objective function is present in the return type of both `twoPhaseSimplex` and `optimizeFeasibleSystem`, but this can be difficult to grok in systems with many variables, so the following function will extract the value of the objective function for you.

```haskell
extractObjectiveValue :: Maybe (Integer, [(Integer, Rational)]) -> Maybe Rational
```

There are similar functions for `DictionaryForm` as well as other custom types in the module `Linear.Util`.

## Usage notes

You must only use positive `Integer` variables in a `VarConstMap`.
This implementation assumes that the user only provides positive `Integer` variables; the `Integer` -1, for example, is sometimes used to represent a `Rational` number. 

## Example

```haskell
exampleFunction :: (ObjectiveFunction, [PolyConstraint])
exampleFunction =
  (
    Max [(1, 3), (2, 5)],      -- 3x1 + 5x2
    [
      LEQ [(1, 3), (2, 1)] 15, -- 3x1 + x2 <= 15 
      LEQ [(1, 1), (2, 1)] 7,  -- x1 + x2 <= 7
      LEQ [(2, 1)] 4,          -- x2 <= 4
      LEQ [(1, -1), (2, 2)] 6  -- -x1 + 2x2 <= 6
    ]
  )

twoPhaseSimplex (fst exampleFunction) (snd exampleFunction)
```

The result of the call above is:
```haskell
Just
  (7, -- Integer representing objective function
  [
    (7,29 % 1), -- Value for variable 7, so max(3x1 + 5x2) = 29.
    (1,3 % 1),  -- Value for variable 1, so x1 = 3 
    (2,4 % 1)   -- Value for variable 2, so x2 = 4
  ]
  )
```

There are many more examples in test/TestFunctions.hs.
You may use `prettyShowVarConstMap`, `prettyShowPolyConstraint`, and `prettyShowObjectiveFunction` to convert these tests into a more human-readable format.

## Issues

Please share any bugs you find [here](https://github.com/rasheedja/simplex-haskell/issues).
