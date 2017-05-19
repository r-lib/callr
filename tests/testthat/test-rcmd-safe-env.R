
context("rcmd_safe_env")

test_that("NOT_CRAN is kept if set", {
  out <- withr::with_envvar(c(NOT_CRAN = "false"), rcmd_safe_env())
  expect_false("NOT_CRAN" %in% names(out))

  out2 <- withr::with_envvar(c(NOT_CRAN = NA), rcmd_safe_env())
  expect_equal(out2[["NOT_CRAN"]], "true")
})
