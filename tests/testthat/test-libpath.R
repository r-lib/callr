
context("library path setup")

test_that(".Library", {
  expect_equal(
    .Library,
    r(function() .Library)
  )
  gc()
})

test_that(".Library.site", {
  expect_equal(
    .Library.site,
    r(function() .Library.site)
  )
  gc()
})

test_that(".libPaths()", {
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE))

  lp <- withr::with_libpaths(
    tmp, action = "prefix",
    r(function() {
      .libPaths()
    })
  )

  expect_true(normalizePath(tmp) %in% normalizePath(lp))
  gc()
})

test_that("if .Renviron overrides R_PROFILE", {

  ## But we still need to use the proper lib path, as set in the fake
  ## profile
  
  cat("Sys.setenv(FOO='nope')\n", file = tmp_prof <- tempfile())
  cat("R_PROFILE=\"", tmp_prof, "\"\n", file = tmp_env <- tempfile(), sep = "")
  cat("R_PROFILE_USER=\"", tmp_prof, "\"\n", file = tmp_env, sep = "", append = TRUE)

  cat("FOO=bar\n", file = tmp_env, sep = "", append = TRUE)

  dir.create(tmp_lib <- tempfile())
  
  on.exit(unlink(c(tmp_prof, tmp_env, tmp_lib), recursive = TRUE))

  lp <- withr::with_envvar(
    c(R_ENVIRON = tmp_env),
    withr::with_libpaths(
      tmp_lib, action = "prefix",
      r(function() list(.libPaths(), Sys.getenv("FOO"), Sys.getenv("R_PROFILE")))
    )
  )

  expect_true(normalizePath(tmp_lib) %in% normalizePath(lp[[1]]))
  expect_equal(lp[[2]], "bar")
  gc()
})
