library(callr)

if (Sys.getenv("NOT_CRAN") == "true") {
  if (callr:::is_false_check_env_var("_R_CHECK_FORCE_SUGGESTS_")) {
    if (requireNamespace("testthat", quietly = TRUE)) {
      library(testthat)
      test_check("callr")
    }
  } else {
    library(testthat)
    test_check("callr")
  }
}
