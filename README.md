
<!-- README.md is generated from README.Rmd. Please edit that file -->

# designit

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of designit is to … **TODO** fill this\!

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
  dimensions = list(
    "plate" = 3,
    "row" = list(values = letters[1:3]),
    "column" = list(values = c(1, 3))
  ),
  exclude = data.frame(plate = 1, row = "a", column = c(1, 3), stringsAsFactors = F)
)

bc
#> Batch container with 18 locations and 2 excluded.
#>   Dimensions: plate<size=3>, row<size=3>, column<size=2>

bc$exclude
#> # A tibble: 2 x 3
#>   plate row   column
#>   <int> <chr>  <int>
#> 1     1 a          1
#> 2     1 a          3
bc$n_locations
#> [1] 18
bc$n_excluded
#> [1] 2
bc$n_available
#> [1] 16
bc$locations_df
#> # A tibble: 16 x 3
#>    plate row   column
#>    <int> <fct>  <int>
#>  1     1 b          1
#>  2     1 b          3
#>  3     1 c          1
#>  4     1 c          3
#>  5     2 a          1
#>  6     2 a          3
#>  7     2 b          1
#>  8     2 b          3
#>  9     2 c          1
#> 10     2 c          3
#> 11     3 a          1
#> 12     3 a          3
#> 13     3 b          1
#> 14     3 b          3
#> 15     3 c          1
#> 16     3 c          3

bc$distribute_samples(samples, random_seed=1)

head(bc$get_samples())
#> # A tibble: 6 x 6
#>   plate row   column a     b     c    
#>   <int> <fct>  <int> <fct> <fct> <fct>
#> 1     1 b          1 <NA>  <NA>  <NA> 
#> 2     1 b          3 <NA>  <NA>  <NA> 
#> 3     1 c          1 <NA>  <NA>  <NA> 
#> 4     1 c          3 a     a     b    
#> 5     2 a          1 a     b     b    
#> 6     2 a          3 <NA>  <NA>  <NA>
bc$get_samples(remove_empty_locations=TRUE)
#> # A tibble: 3 x 6
#>   plate row   column a     b     c    
#>   <int> <fct>  <int> <fct> <fct> <fct>
#> 1     1 c          3 a     a     b    
#> 2     2 a          1 a     b     b    
#> 3     2 b          3 a     c     c

# You can reassign samples starting from the current assignment.
bc$distribute_samples(random_seed=2)
head(bc$get_samples())
#> # A tibble: 6 x 6
#>   plate row   column a     b     c    
#>   <int> <fct>  <int> <fct> <fct> <fct>
#> 1     1 b          1 <NA>  <NA>  <NA> 
#> 2     1 b          3 <NA>  <NA>  <NA> 
#> 3     1 c          1 <NA>  <NA>  <NA> 
#> 4     1 c          3 <NA>  <NA>  <NA> 
#> 5     2 a          1 <NA>  <NA>  <NA> 
#> 6     2 a          3 a     a     b

# Results should be reproducible if the seed is set.
bc$distribute_samples(random_seed=1)
bc$get_samples(remove_empty_locations=TRUE)
#> # A tibble: 3 x 6
#>   plate row   column a     b     c    
#>   <int> <fct>  <int> <fct> <fct> <fct>
#> 1     1 c          3 a     a     b    
#> 2     2 a          1 a     b     b    
#> 3     2 b          3 a     c     c
```

``` r
library(tidyverse)
#> ── Attaching packages ───────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──
#> ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
#> ✓ tibble  3.0.3     ✓ dplyr   1.0.0
#> ✓ tidyr   1.1.0     ✓ stringr 1.4.0
#> ✓ readr   1.3.1     ✓ forcats 0.5.0
#> ── Conflicts ──────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
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
