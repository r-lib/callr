
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
  gc()
})

## Need to supply libpath for covr...
test_that("r_vanilla", {
  expect_equal(
    r_vanilla(function() getOption("repos"), libpath = .libPaths()),
    c(CRAN = "@CRAN@")
  )
  gc()
})

test_that("r_safe", {

  expect_equal(
    r_safe(function() Sys.getenv("R_TESTS")),
    ""
  )
  gc()
})

## https://github.com/r-lib/callr/issues/66
test_that("names of getOption('repos') are preserved", {
  repos <- withr::with_options(
    list(repos = c(foo = "bar")),
    callr::r(function() getOption("repos"))
  )
  expect_false(is.null(names(repos)))
  expect_identical("foo", names(repos)[[1]])
  gc()
})
