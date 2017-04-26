
context("r_bg")

test_that("r_bg runs", {
  x <- r_bg_safe(function() 1 + 1)
  x$wait()
  expect_identical(x$get_result(), 2)
})

test_that("r_bg takes arguments", {
  x <- r_bg_safe(function(x) x + 10, args = list(32))
  x$wait()
  expect_identical(x$get_result(), 42)
})

test_that("r_bg can be killed", {
  x <- r_bg_safe(function(x) Sys.sleep(2))
  x$kill()
  expect_false(x$is_alive())
  expect_error(x$get_result(), "child process crashed or was killed")
})

test_that("r_bg can get the error back", {
  x <- r_bg_safe(function(x) 1 + "A")
  x$wait()
  expect_error(x$get_result(), "non-numeric argument to binary operator")
})
