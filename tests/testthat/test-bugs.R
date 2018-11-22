
context("various bug fixes")

test_that("repos is a list, #82", {

  skip_if_offline()

  expect_silent(
    withr::with_options(
      list(repos = list(CRAN = "https://cloud.r-project.org")),
      callr::r(function() utils::available.packages()))
  )

  gc()
})
