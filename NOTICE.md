# finrb notices and attribution

finrb is licensed under the GNU Lesser General Public License version 3.0 or
later. The complete GPLv3 and LGPLv3 texts are distributed as `COPYING` and
`COPYING.LESSER`.

finrb is derived from the Ruby
[finance](https://github.com/Edward-Intelligence/finance) gem. Financial
calculation utilities were also ported from R's
[FinCal](https://github.com/felixfan/FinCal) package. Their contributors are
acknowledged in the README and repository history.

The bracketed scalar root solver implements the Brent–Dekker algorithm using
the method described by Richard P. Brent in *Algorithms for Minimization
Without Derivatives* (1973). The implementation is local Ruby code and is not
copied from SciPy or QuantLib.

SciPy and QuantLib are optional, independently maintained reference
implementations used by the maintainer verification scripts. They are not
runtime dependencies and their code is not distributed in the finrb gem.
