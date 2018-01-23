
context("timeout")

test_that("r with timeout", {
  tic <- Sys.time()
  e <- tryCatch(
    r(function() Sys.sleep(5), timeout = 1),
    error = function(e) e
  )
  tac <- Sys.time()

  expect_true("callr_timeout_error" %in% class(e))
  expect_true(tac - tic < as.difftime(4, units = "secs"))
})
