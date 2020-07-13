
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

**what to do with numeric covariates**

``` r
a <- readxl::read_excel('inst/extdata/CEA-PRIT_CEAmodels_for randomization.xlsx')
```

## Installation

TODO

## Example

``` r
library(tidyverse)
#> -- Attaching packages ----------------------------------------- tidyverse 1.3.0 --
#> v ggplot2 3.3.0     v purrr   0.3.4
#> v tibble  3.0.1     v dplyr   0.8.5
#> v tidyr   1.0.2     v stringr 1.4.0
#> v readr   1.3.1     v forcats 0.5.0
#> -- Conflicts -------------------------------------------- tidyverse_conflicts() --
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
