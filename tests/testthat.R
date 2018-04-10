library(testthat)
library(callr)

Sys.unsetenv("R_TESTS")
test_check("callr", reporter = "summary")
