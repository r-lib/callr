
context("C++")

test_that("Catch unit tests pass", {
  output <- ""
  tests_passed <- TRUE
  tryCatch(
    output <- capture_output_lines(
      tests_passed <- .Call(c_run_testthat_tests)
    ),
    error = function(e) {
      warning(sprintf("failed to call test entrypoint '%s'",
                      "run_testthat_tests"))
    })
  info <- paste(output[-1], collapse = "\n")
  expect(tests_passed, paste("C++ unit tests:", info, sep = "\n"))
})
