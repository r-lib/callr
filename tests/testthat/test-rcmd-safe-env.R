
context("rcmd_safe_env")

test_that("NOT_CRAN is kept if set", {
  out <- withr::with_envvar(c(NOT_CRAN = "false"), rcmd_safe_env())
  expect_false("NOT_CRAN" %in% names(out))
})
