
context("rcmd")

test_that("rcmd works", {
  expect_equal(rcmd("config", "CC")$status, 0)
  expect_match(rcmd("config", "CC")$stdout, ".")
})

test_that("rcmd show works", {
  expect_output(rcmd("config", "CC", show = TRUE), ".")
})
