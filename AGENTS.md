# AGENTS.md

## Code Style

### General R Style

- Use `snake_case` for function and variable names
- Use roxygen2 for documentation with markdown enabled (`Roxygen: list(markdown = TRUE)`)
- Prefer tidyverse-style code (pipes, dplyr verbs)
- Use `assertthat::assert_that()` for input validation with informative error messages
- Use `stringr::str_c()` and `stringr::str_glue()` for string operations

### Package Dependencies

- Core dependencies: `dplyr`, `purrr`, `ggplot2`, `tibble`, `tidyr`, `R6`, `data.table`
- Use `rlang` for non-standard evaluation and function programming utilities
- Use `data.table` for performance-critical operations

### R6 Classes

The main class is `BatchContainer` (R6). When working with R6:
- Document methods using roxygen2 `@description` tags
- Use `private` for internal state
- Use `active` bindings for computed properties
- Validate inputs in setters

### Function Documentation

```r
#' Brief description of the function.
#'
#' Longer description if needed.
#'
#' @param param_name Description of parameter.
#' @return Description of return value.
#' @export
#'
#' @examples
#' # Example code here
```

### Testing

- Tests are in `tests/testthat/`
- Use `testthat` for unit tests
- Test file naming: `test-<feature>.R`
- Use `expect_true()`, `expect_equal()`, `expect_error()`, etc.

### Vignettes

- Vignettes are in `vignettes/` as `.Rmd` files
- Use knitr for rendering
- Some vignettes have cached versions in `vignettes/cached/`

## Key Concepts

- **BatchContainer**: Holds dimensions for sample allocation and assignment
- **Scoring functions**: Evaluate sample assignments (lower is better)
- **Shuffle functions**: Propose sample permutations during optimization
- **OSAT score**: Chi-square-based score for even sample distribution

## Common Patterns

### Creating a scoring function generator

```r
my_score_generator <- function(param1, param2) {
  force(param1)
  force(param2)
  
  cached_value <- NULL
  
  function(bc) {
    # Compute and return score
  }
}
```

### Input validation

```r
assertthat::assert_that(
  is.data.frame(df),
  msg = "df should be a data.frame"
)
```

### Silencing R CMD check notes for data.table

```r
# At the start of functions using data.table syntax:
. <- .N <- `:=` <- NULL
```
