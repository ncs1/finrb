# Solver oracle fixtures

These fixtures validate finrb against independent implementations. They are test evidence, not runtime data.

## SciPy

`scipy_brentq_irr.json` was generated with `scipy.optimize.brentq` 1.18.1. SciPy solves the periodic NPV equation using binary double precision and a caller-supplied sign-changing bracket.

## QuantLib

`quantlib_yield_rate_xirr.json` was generated with QuantLib 1.43 `CashFlows.yieldRate`. The settings intentionally match finrb's current XIRR semantics:

- Actual/365 Fixed day count;
- annually compounded effective rate;
- settlement-date cashflows excluded from the future leg, with the initial outlay supplied as NPV;
- `1e-13` accuracy and 1000 maximum evaluations.

QuantLib's cached bond-yield fixtures are not copied here. Those values incorporate coupon schedules, accrued interest, clean/dirty prices, market calendars, and bond-specific day-count conventions that finrb XIRR does not currently model. Treating them as plain XIRR fixtures would compare different financial contracts.

## Live randomized comparison

Install the optional dependencies into a Python environment of your choice:

```shell
python3 -m pip install --requirement script/requirements-cross-validation.txt
```

Run the randomized comparison with that environment active:

```shell
bundle exec rake oracle:cross_validate
```

The task uses `python3` by default. Set `PYTHON` to select another interpreter without requiring any particular environment manager:

```shell
PYTHON=/path/to/python bundle exec rake oracle:cross_validate
```

The default run generates 100 periodic and 100 dated cases with seed `20260825`, four workers, and batches of 50. Override these settings through environment variables when needed:

```shell
COUNT=500 SEED=17 WORKERS=8 BATCH_SIZE=25 bundle exec rake oracle:cross_validate
```

The harness generates both periodic and irregularly dated conventional cashflows, selects finrb guesses independently from the constructed root, calls finrb through `script/finrb_solver_adapter.rb`, and compares each result with both external oracles. It sends bounded newline-delimited JSON batches through persistent Ruby worker processes instead of constructing one unbounded stdin payload. Ruby workers and SciPy comparisons run concurrently; QuantLib comparisons stay sequential because its Python binding returned invalid results under concurrent access.

QuantLib receives the constructed root as its guess because its linear auto-bracketing can cross invalid yield domains from poor guesses; the comparison still independently evaluates its NPV, derivative, and safeguarded Newton implementation. The versions used by the maintained fixtures are pinned in `script/requirements-cross-validation.txt`. Neither Python package is a finrb runtime dependency.
