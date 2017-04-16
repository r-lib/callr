
context("timeout")

test_that("r with timeout", {
  e <- tryCatch(
    r(function() Sys.sleep(5), timeout = 0.01),
    error = function(e) e
  )

  expect_true("callr_timeout_error" %in% class(e))
})
