# Contributing to finrb

finrb welcomes focused bug fixes, documentation improvements, tests, and
financial calculations that fit the project's scope. Before starting a large
feature or public API change, open an issue so its financial conventions and
design can be agreed upon first.

## Development setup

finrb requires Ruby 3.3 or newer. MRI 3.3, 3.4, and 4.0 are supported; JRuby
and TruffleRuby are tested as informational compatibility targets.

```shell
bundle install
bundle exec rake quality
bundle exec rubocop
```

`rake quality` runs the RSpec suite with coverage, verifies maintained API
examples, and validates the packaged RBS declarations. Additional checks are
available for changes that affect their domains:

```shell
bundle exec rake security:audit
bundle exec rake package:verify
bundle exec rake benchmark:run
bundle exec rake solver:verify
```

The solver verification task requires the optional Python environment
documented in [spec/fixtures/README.md](spec/fixtures/README.md). The Python
packages are development references and are not gem dependencies.

## Making changes

- Preserve unrelated code and behavior. Keep pull requests small enough to
  review their financial and numerical consequences directly.
- Follow the local Ruby style and run RuboCop. Comments should explain a
  non-obvious reason or convention, not restate the code.
- Route public numeric inputs through `Finrb::Validation`. Use `ArgumentError`
  for malformed inputs and namespaced finrb errors for financial or numerical
  domains.
- Preserve `Flt::DecNum` calculations and return types unless the change has a
  documented compatibility reason.
- Update RBS declarations and user documentation when a public contract
  changes.
- Do not add an unreleased changelog section for ordinary development work.
  The maintainer prepares the changelog as part of an actual release.

## Numerical and financial changes

A formula compiling or producing a plausible number is not sufficient
verification. Describe the convention being implemented and test the relevant
invariants, boundaries, and failure modes.

For a solver or precision-sensitive change, include as appropriate:

- known-root or algebraically constructed cases;
- normalized equation residuals;
- zero, negative-rate, long-horizon, and scale-sensitive cases;
- attributable fixtures from a reputable independent implementation under
  matching financial conventions;
- deterministic seeds for generated cases; and
- benchmarks that compare the same scenario across revisions, runtimes, or
  architectures rather than ranking unrelated calculations.

Reference fixtures must record their source, version, conventions, generation
method, and tolerance. Do not replace committed fixtures merely to make a
changed implementation pass without explaining why the reference changed.

## Pull requests

Use the pull request template and include:

- the financial or technical problem;
- the intended contract and compatibility impact;
- the evidence used to verify correctness; and
- any checks skipped because an optional runtime or external reference was
  unavailable.

Commit subjects in this repository commonly use the form
`domain [Category]: Imperative summary`, for example:

```text
cashflows [Fix]: Preserve negative-root selection
```

By contributing, you agree that your work is provided under the repository's
LGPL-3.0-or-later license.
