library(testthat)
library(callr)

Sys.unsetenv("R_TESTS")

if (ps::ps_is_supported()) {
  reporter <- ps::CleanupReporter(testthat::SummaryReporter)$new()
  results <- test_check("callr", reporter = reporter)
  failed <- sum(vapply(results, FUN.VALUE = integer(1), function(r) {
    sum(vapply(
      r$results,
      FUN.VALUE = logical(1),
      inherits, "expectation_error"))
  }))
  if (failed) stop("Test failures", call. = FALSE)

} else {
  ## ps does not support this platform

  sysname <- tolower(Sys.info()[["sysname"]])
  if (sysname != "sunos" || Sys.getenv("NOT_CRAN") == "true") {
    test_check("callr", reporter = "summary")
  }
}
