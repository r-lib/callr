library(testthat)
library(callr)

if (Sys.getenv("NOT_CRAN", "") == "true") test_check("callr")
