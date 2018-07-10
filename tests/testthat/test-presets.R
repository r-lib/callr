
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
    c(CRAN = "@CRAN@")
  )
})

test_that("r_safe", {

  expect_equal(
    r_safe(function() Sys.getenv("R_TESTS")),
    ""
  )
})

test_that("R_LIBS_SITE is set properly", {
  skip_on_cran()

  lib <- normalizePath(tempdir())
  env <- rcmd_safe_env()
  env["R_LIBS_SITE"] <- lib

  res <- r(env = env, function() {
    Sys.unsetenv("R_LIBS_SITE")
    callr::r(function() {
      .libPaths(tempdir())
      .libPaths()
    })
  })

  expect_true(lib %in% res)
})

## https://github.com/r-lib/callr/issues/66
test_that("names of getOption('repos') are preserved", {
  repos <- withr::with_options(
    list(repos = c(foo = "bar")),
    callr::r(function() getOption("repos"))
  )
  expect_false(is.null(names(repos)))
  expect_identical("foo", names(repos)[[1]])
})
