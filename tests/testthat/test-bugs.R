test_that("repos is a list, #82", {
  skip_if_not_installed("withr")
  expect_true(withr::with_options(
    list(repos = list(CRAN = "https://cloud.r-project.org")),
    callr::r(function() inherits(getOption("repos"), "list"))
  ))
})
