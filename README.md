
<!-- README.md is generated from README.Rmd. Please edit that file -->

# designit

<!-- badges: start -->

[![Pipeline
Status](https://code.roche.com/PMDA/packages/designit/badges/master/pipeline.svg)](https://code.roche.com/PMDA/packages/designit/-/commits/master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Documentation](https://img.shields.io/badge/docs-pkgdown-blue.svg)](http://docs.roche.com/#/designit/latest)
<!-- badges: end -->

<figure>
<img src="logo.png" width="200" alt="logo" />
<figcaption aria-hidden="true">logo</figcaption>
</figure>

The goal of designit is to generate optimal sample allocations for
experimental designs.

## Installation

You can install the development version from Roche GitHub with:

``` r
# install.packages("devtools")
devtools::install_gitlab(
  "PMDA/packages/designit",
  host = "code.roche.com",
  auth_token = "rNVzMHKpdnnD_TJ3nMGw"
)
```

## Usage

The main class used is `BatchContainer`, which holds the dimensions for
sample allocation. After creating such a container, a list of samples
can be allocated in it using a given assignment function.

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
#> Batch container with 16 locations.
#>   Dimensions: plate, row, column

bc$n_locations
#> [1] 16
bc$get_locations()
#> # A tibble: 16 × 3
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

set.seed(1)
assign_random(bc, samples)

head(bc$get_samples())
#> # A tibble: 6 × 6
#>   plate row   column a     b     c    
#>   <int> <fct>  <int> <chr> <chr> <chr>
#> 1     1 b          1 <NA>  <NA>  <NA> 
#> 2     1 b          3 <NA>  <NA>  <NA> 
#> 3     1 c          1 <NA>  <NA>  <NA> 
#> 4     1 c          3 a     a     b    
#> 5     2 a          1 a     b     b    
#> 6     2 a          3 <NA>  <NA>  <NA>
bc$get_samples(remove_empty_locations=TRUE)
#> # A tibble: 3 × 6
#>   plate row   column a     b     c    
#>   <int> <fct>  <int> <chr> <chr> <chr>
#> 1     1 c          3 a     a     b    
#> 2     2 a          1 a     b     b    
#> 3     2 b          3 a     c     c

# You can reassign samples starting from the current assignment.
set.seed(2)
assign_random(bc)
head(bc$get_samples())
#> # A tibble: 6 × 6
#>   plate row   column a     b     c    
#>   <int> <fct>  <int> <chr> <chr> <chr>
#> 1     1 b          1 <NA>  <NA>  <NA> 
#> 2     1 b          3 <NA>  <NA>  <NA> 
#> 3     1 c          1 <NA>  <NA>  <NA> 
#> 4     1 c          3 <NA>  <NA>  <NA> 
#> 5     2 a          1 <NA>  <NA>  <NA> 
#> 6     2 a          3 a     a     b

# Results should be reproducible if the seed is set.
set.seed(1)
assign_random(bc)
bc$get_samples(remove_empty_locations=TRUE)
#> # A tibble: 3 × 6
#>   plate row   column a     b     c    
#>   <int> <fct>  <int> <chr> <chr> <chr>
#> 1     1 c          3 a     a     b    
#> 2     2 a          1 a     b     b    
#> 3     2 b          3 a     c     c
```

## Examples

See vignettes `vignette("basic_examples")`.
