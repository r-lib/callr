
context("presets")

test_that("r", {

  withr::with_options(
    list(repos = "foobar"),
    expect_equal(
      r_copycat(function() getOption("repos"),
        user_profile = FALSE,
        system_profile = FALSE
      ),
      "foobar"
    )
  )
})

## Need to supply libpath for covr...
test_that("r_vanilla", {
  expect_equal(
    r_vanilla(function() getOption("repos"), libpath = .libPaths()),
    "@CRAN@"
  )
})

test_that("r_safe", {

  expect_equal(
    r_safe(function() Sys.getenv("R_TESTS")),
    ""
  )
})
