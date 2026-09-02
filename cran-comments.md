## Submission

This is a patch release fixing the check ERROR seen on the r-devel flavors of
designit 0.5.0, and the CPU-time NOTE seen on the Debian flavors.

### 1. Test failure on r-devel

The tests in `test-save-random-seed.R` asserted that `RNGkind()` returns a
character vector of length 3. In R-devel this gained a fourth component,
`binom.kind`:

    r90299 | maechler | 2026-07-25 18:28:36 +0200 (Sat, 25 Jul 2026) | 1 line
    fix 2 signs in formula of the BTPE algorithm for rbinom(); add RNGkind(binom.kind=*)

The assertions now compare against `length(RNGkind())` rather than a literal
3, so they hold on every R version. No package code changed for this: the
`BatchContainer` trace already stored the result of `RNGkind()` verbatim and
never inspects its length.

### 2. "Examples with CPU time > 2.5 times elapsed time"

designit has no compiled code and no parallel code of its own. The likely cause
is OpenMP threading in the imported data.table, though we could not reproduce
the NOTE directly. Examples, tests and vignettes now call
`data.table::setDTthreads(2)`, following
<https://github.com/Rdatatable/data.table/issues/5658#issuecomment-1741934995>.

The flagged examples themselves are unchanged since 0.5.0.

The release also includes the changes accumulated since 0.5.0; see NEWS.md.

## Test environments

* macOS 26.6.2 (aarch64-apple-darwin), R 4.7.0 Under development (r90448)
* macOS 26.6.2 (aarch64-apple-darwin), R 4.6.1
* macOS 26.6.2 (aarch64-apple-darwin), R 4.1.3 (declared minimum; tests only)
* Debian (aarch64, container), R 4.6.1

## R CMD check results

0 errors | 0 warnings | 0 notes
