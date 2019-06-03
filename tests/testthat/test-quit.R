
context("calling quit()")

test_that("quit() in the function", {
  x <- r(function() quit())
  expect_null(x)

  expect_error(
    r(function() quit(status = 2)), "non-zero status",
    class = "callr_status_error")
  gc()
})

test_that("quit() in bg process", {
  p1 <- r_bg(function() quit())
  on.exit(p1$kill())
  p1$wait()
  expect_null(p1$get_result())

  close(p1$get_output_connection())
  close(p1$get_error_connection())

  p2 <- r_bg(function() quit(status = 2))
  on.exit(p2$kill(), add = TRUE)
  p2$wait()
  expect_error(
    p2$get_result(), "non-zero status", class = "callr_status_error")

  close(p2$get_output_connection())
  close(p2$get_error_connection())
})
