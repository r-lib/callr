
context("rcmd_process")

test_that("create and run rcmd_process", {
  opts <- rcmd_process_options(cmd = "config", cmdargs = "CC")
  x <- rcmd_process$new(opts)
  x$wait()
  expect_equal(x$get_exit_status(), 0)
  out <- x$read_output_lines()
  expect_match(out, ".")
})
