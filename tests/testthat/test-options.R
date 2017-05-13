
context("options")

test_that("error for unknown options", {

  expect_error(
    r_process_options(func = function() {}, foo = "bar"),
    "Unknown option"
  )

  expect_error(
    r_process_options(func = function() {}, foo = "bar", bar = "foo"),
    "Unknown options"
  )
})
