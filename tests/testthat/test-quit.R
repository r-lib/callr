
context("calling quit()")

test_that("quit() in the function", {
  x <- r(function() quit())
  expect_null(x)

  expect_error(r(function() quit(status = 2)), "non-zero status")
})

test_that("quit() in bg process", {
  p <- r_bg(function() quit())
  p$wait()
  expect_null(p$get_result())

  p <- r_bg(function() quit(status =  2))
  p$wait()
  expect_error(p$get_result(), "non-zero status")
})
