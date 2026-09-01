## Submission

This is a patch release fixing the check ERROR seen on the r-devel flavors of
designit 0.5.0.

The tests in `test-save-random-seed.R` asserted that `RNGkind()` returns a
character vector of length 3. In R-devel this gained a fourth component,
`binom.kind`:

    r90299 | maechler | 2026-07-25 18:28:36 +0200 (Sat, 25 Jul 2026) | 1 line
    fix 2 signs in formula of the BTPE algorithm for rbinom(); add RNGkind(binom.kind=*)

The assertions now compare against `length(RNGkind())` rather than a literal
3, so they hold on every R version. No package code changed for this: the
`BatchContainer` trace already stored the result of `RNGkind()` verbatim and
never inspects its length.

The release also includes the changes accumulated since 0.5.0; see NEWS.md.

## Test environments

* macOS 26.6.2 (aarch64-apple-darwin), R 4.7.0 Under development (r90448)
* macOS 26.6.2 (aarch64-apple-darwin), R 4.6.1
* macOS 26.6.2 (aarch64-apple-darwin), R 4.1.3 (declared minimum; tests only)

## R CMD check results

0 errors | 0 warnings | 0 notes
