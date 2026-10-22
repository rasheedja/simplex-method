# Changelog for simplex-method

## Unreleased changes

- Warm-start each objective from the previous objective's final dictionary: `optimizeAllObjectives` is now a left fold that threads the `FeasibleSystem`, fed by a new internal `optimizeFeasibleSystemWithDict` that returns the final dictionary with the objective row removed (the input system, unchanged, on `Unbounded`), so a `Min x_i`/`Max x_i` pair starts phase two from a basis near the next optimum instead of from the phase-one dictionary. The public `optimizeFeasibleSystem` and `OptimisationOutcome` are unchanged, and no objective's optimal value can change: every feasible dictionary describes the same feasible region, so only the pivot count and the choice among tied optimal bases differ. On the new spec's 2-variable LP the `Max x1` phase takes one pivot after `Min x1` instead of two, and the per-objective-independence property still passes 300 cases. ([#42](https://github.com/rasheedja/simplex-method/pull/42))

- De-duplicate the two-phase spec with a shared `SpecSupport` helper module: `solveNonNeg`/`solveWith` run a solve under `runNoLoggingT`, and `expectOptimal`/`expectOptimalValues`/`expectUnbounded`/`expectInfeasible`/`expectOutcomes` assert the result shape and report the actual result on failure, so 129 solve sites and 131 assertions no longer repeat the all-variables-non-negative domain map, the `runStdoutLoggingT` filter and the hand-written fall-throughs, and no test writes solver warnings to stdout. All 268 test cases, their descriptions and every expected variable map and objective value are unchanged; the spec shrinks from 3747 to 2785 lines and still builds warning-free under `-Wall`. ([#37](https://github.com/rasheedja/simplex-method/pull/37))

- Turn on warnings: the library and test suite now build with `-Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints`, and GHC reports no warnings on the Stack GHC (warnings stay non-fatal, so a newer compiler that adds a check cannot break CI). Fixing them removes the unused `Data.List`, `Data.Ratio`, `GHC.Real` and `fromJust`/`mapMaybe`/`nonNegative` imports, renames the phase-one `where` bindings that were shadowed by their own parameters, and replaces the non-exhaustive pattern bindings in the tests with total bindings. `dictionaryFormToTableau`, which no library code called, moves out of `Linear.Simplex.Util` into the test suite. No behaviour change. ([#36](https://github.com/rasheedja/simplex-method/pull/36))

- Transform the constraints once per solve instead of once per objective: `preprocess` takes the already-collected variable set, `applyTransform` is split into `applyTransformToObjective` and `applyTransformToConstraints`, and the zero-objective case no longer fabricates a `Max M.empty` objective to reuse the combined fold, so solving `n` objectives over one constraint set no longer rebuilds the transformed constraints `n + 1` times. `getTransform` matches on the lower bound once instead of twice and drops the dead `lowerTransforms`/`varOffset` bindings. No behaviour change: the transforms are still folded in the same order, so the constraint list and fresh-variable ids are unchanged. ([#35](https://github.com/rasheedja/simplex-method/pull/35))

- Build the phase-two objective row in one place: the `null artificialVars` branch in `optimizeFeasibleSystem` and the `normalObjective`/`adjustedObjective` split are gone, the objective is sign-normalised once for the direction of optimisation and every variable that is basic after phase one is back-substituted into it, so the plain row is the degenerate case rather than a separate construction; `displayResults` regains the objective sign with the same single `isMax` check and drops slack and artificial rows with one `M.withoutKeys`. No behaviour change. ([#34](https://github.com/rasheedja/simplex-method/pull/34))

- Simplify phase-one variable construction: `systemInStandardForm` and `systemWithArtificialVars` now thread the next fresh id through `mapAccumL` instead of hand-written recursion, the artificial-variable ladder is replaced by the single rule it implemented (no basic var, or `r /= 0` with a basic coefficient of the opposite sign), and the `error "1"` branch and the non-EQ catch-all report what went wrong. Slack and artificial ids are still allocated in constraint order, so pivot choices and answers are unchanged. ([#33](https://github.com/rasheedja/simplex-method/pull/33))
- Keep the objective as an ordinary row of the `Dict` while pivoting: `simplexPivot` takes the objective `Var` instead of a separate `PivotObjective` record and returns the `Dict` with that row still in it, so a pivot no longer inserts the row, looks it back up, deletes it and rebuilds the record. Removes the `PivotObjective` type, `insertPivotObjectiveToDict` and `prettyShowPivotObjective`. ([#32](https://github.com/rasheedja/simplex-method/pull/32))
- Remove the clock from logging: `logMsg` no longer timestamps messages, so `MonadIO` is not required by `findFeasibleSolution`, `optimizeFeasibleSystem`, `twoPhaseSimplex` or `simplexPivot` any more (only a breaking change for callers that pattern-matched the constraint), and `twoPhaseSimplex` now runs in a pure monad under `runNoLoggingT`; the new `runTimestampedStdoutLoggingT` restores the old timestamped output at the log sink. Traces follow a pivot counter instead of wall-clock time, with one `LevelInfo` line per pivot and pretty-printed dictionaries moved to `LevelDebug`. ([#31](https://github.com/rasheedja/simplex-method/pull/31))
- Shift every nonzero lower bound onto a fresh variable through `Shift` instead of adding a `GEQ` row, so a bounded variable's upper bound stays a `LEQ` row whose slack is feasible at the origin: positive lower bounds no longer create artificial variables or run the phase-one pivot loop at all. Removes the `AddLowerBound` constructor from `VarTransform`. ([#30](https://github.com/rasheedja/simplex-method/pull/30))
- Add solver-level property tests on small bounded linear programs: every optimal answer is checked to be feasible inside its domains, its objective value is checked against a brute-force vertex enumeration, and each objective's result is checked not to depend on the other objectives requested alongside it. ([#29](https://github.com/rasheedja/simplex-method/pull/29))
- Reserve every variable that appears in an objective when allocating slack and artificial variable ids in phase one, so a variable that appears only in an objective no longer shares an id with a slack variable and silently returns a wrong optimum. ([#27](https://github.com/rasheedja/simplex-method/pull/27))
- Remove an unreachable objective-row lookup branch in `findFeasibleSolution` and correct its inverted phase-one objective RHS error message. ([#26](https://github.com/rasheedja/simplex-method/pull/26))
- Decide constant constraints, whose left-hand side has only zero coefficients, before phase one: a contradiction returns infeasible instead of crashing on an empty variable list, and tautologies are dropped. ([#28](https://github.com/rasheedja/simplex-method/pull/28))
- Prevent degenerate simplex pivot cycles by using Bland's least-index entering and leaving rules in both phases. Tied optimal solutions may select different variable values while preserving the objective optimum.
- Remove zero-valued artificial basic variables before phase two, preserving the original constraints when a degenerate phase-one optimum leaves artificials in the basis.
- Support zero objectives after the artificial-variable phase and replace custom coefficient-map merging with standard Data.Map operations. ([#18](https://github.com/rasheedja/simplex-method/pull/18))
- Correct fractional coefficients in pretty-printed expressions and remove trailing plus separators. ([#19](https://github.com/rasheedja/simplex-method/pull/19))
- Share shift and split coefficient-map transformations between objectives and constraints, and simplify access to their fields. ([#23](https://github.com/rasheedja/simplex-method/pull/23))
  - Avoid ambiguous constraint record updates using the existing field lenses, group the substitution helpers together, and test them directly.
- Use standard Data.Map selection and deletion operations for solver variable filtering. ([#20](https://github.com/rasheedja/simplex-method/pull/20))
- Remove unused pivot bindings and give the active row-update helper a descriptive name. ([#21](https://github.com/rasheedja/simplex-method/pull/21))
- Extract optimal variable values directly from dictionary constants without an intermediate tableau conversion. ([#22](https://github.com/rasheedja/simplex-method/pull/22))
- Remove QuickCheck from the library dependencies while retaining it for property tests. ([#24](https://github.com/rasheedja/simplex-method/pull/24))
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

## [v0.2.0.0](https://github.com/rasheedja/simplex-method/tree/v0.2.0.0)

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

## [v0.1.0.0](https://github.com/rasheedja/simplex-method/tree/v0.1.0.0)

- Initial release
