# designit 0.5.0

* remove magrittr, glue, checkmate, forcats, ineq, and tidyselect dependencies
* minimum R version 4.1.0 (for the native pipe and lambda functions)
* remove several unexported functions
* BatchContainer trace saves the random seed as well as RNG kind
* preparing for CRAN resumbission

# designit 0.4.1

* mainly github action changes

# designit 0.4.0

* multiplate wrapper
* fixes in the OptimizationTrace
* cleanup

# designit 0.3.0

* `BatchContainer` stores locations table (dimensions & excluded)
* `BatchContainer$new()` accepts locations table
* `BatchContainer` with samples can be created using
  `batch_container_from_table`
* `bc$n_available` was removed (use `bc$n_locations` instead)
* minor improvements to `print(bc)`

# designit 0.2.4

* fix dplyr filter issue (once again)
* migrate from BEDA to PMDA

# designit 0.2.3

* fix dplyr filter issue

# designit 0.2.2

* fix plate scoring example vignette caching

# designit 0.2.1

* plate scoring

# designit 0.2.0

* new optimizer `optimize_design()`
* various API improvements

# designit 0.1.0

* `BatchContainer` major API update
* rename fields for consistency
* hide cachind (`$samples_dt`) behind `$get_samples()`
* `$sample_attr` for sample attributes
* unified `$move_samples()` for moving samples

# designr 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
