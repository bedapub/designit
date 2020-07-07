
<!-- README.md is generated from README.Rmd. Please edit that file -->

# designit

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of designit is to …

## skeleton

  - a metric of how balanced?
  - check ^2 and ^3, it should be different (3 better)

### algorithm

  - generate formula
      - .^m m = number of factors
      - check model.matrix
      - check how many to remove, and what order
      - if one order is not complete, then do random draw from that?
      - or use optMonteCarlo \<– this doesn’t work
  - optFederov
  - optBlock
  - rename Block and levels
  - do it again

**should we support more than 2 levels?**

**what to do with numeric
covariates**

``` r
a <- readxl::read_excel('inst/extdata/CEA-PRIT_CEAmodels_for randomization.xlsx')
```

## Installation

TODO

## Example

``` r
library(designit)
# define samples data.frame
samples <- data.frame(a='a', b=letters[1:3], c=c('b', 'b', 'c'))
samples
#>   a b c
#> 1 a a b
#> 2 a b b
#> 3 a c c

bc <- BatchContainer$new(
  dimensions=c('plate'=3, 'row'=2, 'column'=2),
  exclude=data.frame(plate=2, row=1, column=2)
)

bc
#> Batch container with 12 elements and 1 excluded.
#>   Dimensions: plate<size=3>, row<size=2>, column<size=2>

bc$exclude
#> # A tibble: 1 x 3
#>   plate   row column
#>   <int> <int>  <int>
#> 1     2     1      2
bc$n_elements
#> [1] 12
bc$n_excluded
#> [1] 1
bc$n_available
#> [1] 11
bc$elements_df
#> # A tibble: 11 x 3
#>    plate   row column
#>    <int> <int>  <int>
#>  1     1     1      1
#>  2     2     1      1
#>  3     3     1      1
#>  4     1     2      1
#>  5     2     2      1
#>  6     3     2      1
#>  7     1     1      2
#>  8     3     1      2
#>  9     1     2      2
#> 10     2     2      2
#> 11     3     2      2

distributed <- distribute_samples(samples, bc, random_seed=1)
distributed
#>   a b c plate row column
#> 1 a a b     1   2      1
#> 2 a b b     2   2      1
#> 3 a c c     1   1      2
```

``` r
library(tidyverse)
#> ── Attaching packages ──────────────────────────────────────────────────────────── tidyverse 1.3.0 ──
#> ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
#> ✓ tibble  3.0.1     ✓ dplyr   1.0.0
#> ✓ tidyr   1.1.0     ✓ stringr 1.4.0
#> ✓ readr   1.3.1     ✓ forcats 0.5.0
#> ── Conflicts ─────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
x <- expand.grid(b = paste0('b', 1:4),
                 a = LETTERS[1:4]) %>% 
  slice(-3:-4)
```

Example dataset:

Cell: 5 levels (A, B, C, D, E) Trt: 2 levels (N, T) Time: 2 levels (24,
48)

2 batches: Extract: 5 levels (1-5) Prep: 2 levels (1-2)

all combinations, 80 expt
