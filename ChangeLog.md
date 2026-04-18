# Changelog for simplex-haskell

## Unreleased changes

- `twoPhaseSimplex` now takes a `VarDomainMap` as its first argument
  - Specify each variable's domain using smart constructors: `nonNegative`, `unbounded`, `lowerBoundOnly`, `upperBoundOnly`, or `boundedRange`
  - Variables not in the `VarDomainMap` are assumed to be `unbounded`
  - To keep the same behaviour as before (all vars non-negative), use `nonNegative` for all variables
- `twoPhaseSimplex` now takes a list of `ObjectiveFunction`s instead of a single one
  - Optimise multiple variables against one constraint set in a single call
  - Pass an empty list to run Phase 1 only (feasibility check)
- `twoPhaseSimplex` now returns `SimplexResult` instead of `Maybe Result`
  - `SimplexResult` contains `Maybe FeasibleSystem` and `[ObjectiveResult]`
  - Each `ObjectiveResult` pairs an `ObjectiveFunction` with its `OptimisationOutcome`
  - `OptimisationOutcome` is either `Optimal { varValMap }` or `Unbounded`
- `optimizeFeasibleSystem` now returns `OptimisationOutcome` instead of `Maybe Result`
- Restructured `VarDomain` type to support upper bounds
  - Replaced `NonNegative`, `LowerBound SimplexNum`, and `Unbounded` constructors with a single `Bounded { lowerBound :: Maybe SimplexNum, upperBound :: Maybe SimplexNum }` record
  - Added smart constructors: `unbounded`, `nonNegative`, `lowerBoundOnly`, `upperBoundOnly`, and `boundedRange`
- Added `VarDomainMap` newtype for mapping variables to their domains
- Added `VarTransform` type with `AddLowerBound`, `AddUpperBound`, `Shift`, and `Split` constructors
- Removed `Result`, `SimplexMeta`, `SystemWithSlackVarRow`, `Equation` types
- Removed `extractObjectiveValue` and `foldDictValue` utility functions
- Renamed `prettyShowVarConstMap` to `prettyShowVarLitMapSum`
- Widened dependency version bounds (supports GHC 9.2–9.12)
- Removed `package.yaml`; `simplex-method.cabal` is now maintained directly
- Use HSpec for tests
- Add Nix flake
- Add Makefile with cabal and stack support
- Add CI for cabal (GHC 9.2–9.12), stack (LTS 22.44), and Nix
- Explicit import lists on all modules
- Bump Stackage LTS to 22.44

## [v0.2.0.0](https://github.com/rasheedja/LPPaver/tree/v0.2.0.0)

- Setup CI
- Use fourmolu formatter
- Add better types
- Use lens
- Use RecordDot syntax
- Add logging
- Improve Docs
- More Tests
- Bump Stackage LTS
- Rename Linear.Simplex.Simplex -> Linear.Simplex.TwoPhase.Simplex

## [v0.1.0.0](https://github.com/rasheedja/LPPaver/tree/v0.1.0.0)

- Initial release
