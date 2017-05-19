
context("rcmd_bg")

test_that("rcmd_bg runs", {
  x <- rcmd_bg("config", "CC")
  x$wait()
  expect_match(x$read_output_lines(), ".")
  expect_equal(x$get_exit_status(), 0)
})

test_that("r_cmd can be killed", {
  cat("Sys.sleep(5)", file = tmp <- tempfile())
  on.exit(try_silently(unlink(tmp)), add = TRUE)

  x <- rcmd_bg("BATCH", tmp)
  expect_true(x$is_alive())

  x$kill()
  expect_false(x$is_alive())
})

test_that("r_cmd errors", {
  x <- rcmd_bg("config", "axaxaxaxax")
  x$wait()
  expect_equal(x$get_exit_status(), 1)
})
