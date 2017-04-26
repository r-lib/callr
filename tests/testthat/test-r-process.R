
context("r_process")

test_that("create and run r_process", {
  options <- r_process_options(func = function() 1 + 1)
  x <- r_process$new(options)
  x$wait()
  expect_equal(x$get_result(), 2)
})
