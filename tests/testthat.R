library(testthat)
library(callr)

Sys.unsetenv("R_TESTS")

if (ps::ps_is_supported()) {
  reporter <- ps::CleanupReporter(testthat::SummaryReporter)$new()
} else {
  ## ps does not support this platform
  reporter <- "summary"
}

test_check("callr", reporter = reporter)

print(1:10000)
