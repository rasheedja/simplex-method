# Changelog for simplex-haskell

## Unreleased changes

- **BREAKING CHANGE**: Restructured `VarDomain` type to support upper bounds
  - Replaced `NonNegative`, `LowerBound SimplexNum`, and `Unbounded` constructors with 
    a single `Bounded { lowerBound :: Maybe SimplexNum, upperBound :: Maybe SimplexNum }` record
  - Added smart constructors for convenience: `unbounded`, `nonNegative`, `lowerBoundOnly`, 
    `upperBoundOnly`, and `boundedRange`
  - `Bounded Nothing Nothing` is equivalent to `Unbounded`
  - `Bounded (Just 0) Nothing` is equivalent to `NonNegative`
  - Upper bounds are now supported and automatically added as LEQ constraints
- Added `AddUpperBound` constructor to `VarTransform` for upper bound constraint generation
- Updated `getTransform` to return a list of transforms (can now generate both lower and upper bound transforms)
- Use Hspec for tests
- Add nix flake
- twoPhaseSimplex now takes a VarDomainMap (as the first param)
  - You can specify each Var's domain using smart constructors: `nonNegative`, `unbounded`, 
    `lowerBoundOnly`, `upperBoundOnly`, or `boundedRange`
  - If a VarDomain for a Var is undefined, it's assumed to be `unbounded`
  - If you want to keep the same behaviour as before (all vars non-negative), use `nonNegative` for all Vars

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
