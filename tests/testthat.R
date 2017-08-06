library(testthat)
library(callr)

if (Sys.getenv("NOT_CRAN") != "" || .Platform$OS.type != "windows") {
  Sys.unsetenv("R_TESTS")
  test_check("callr")
}
