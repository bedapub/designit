library(testthat)
library(designit)

# limit threads to avoid CPU time issues on CRAN
data.table::setDTthreads(2)

test_check("designit")
