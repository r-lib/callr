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
  skip_if_not_installed("withr")
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE))

  lp <- withr::with_libpaths(
    tmp,
    action = "prefix",
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
  skip_in_covr()
  skip_if_not_installed("withr")

  cat("Sys.setenv(FOO='nope')\n", file = tmp_prof <- tempfile())
  cat("R_PROFILE=\"", tmp_prof, "\"\n", file = tmp_env <- tempfile(), sep = "")
  cat(
    "R_PROFILE_USER=\"",
    tmp_prof,
    "\"\n",
    file = tmp_env,
    sep = "",
    append = TRUE
  )

  cat("FOO=bar\n", file = tmp_env, sep = "", append = TRUE)

  dir.create(tmp_lib <- tempfile())

  on.exit(unlink(c(tmp_prof, tmp_env, tmp_lib), recursive = TRUE))

  lp <- withr::with_envvar(
    c(R_ENVIRON = tmp_env),
    withr::with_libpaths(
      tmp_lib,
      action = "prefix",
      r(
        function() list(.libPaths(), Sys.getenv("FOO"), Sys.getenv("R_PROFILE"))
      )
    )
  )

  expect_true(normalizePath(tmp_lib) %in% normalizePath(lp[[1]]))
  expect_equal(lp[[2]], "bar")
  gc()
})

test_that("libpath in system(), empty .Renviron", {
  # We remove the library with covr from the lib path, so this
  # cannot work in a subprocess.
  skip_in_covr()
  skip_if_not_installed("withr")

  dir.create(tmpdrop <- tempfile("drop"))
  dir.create(tmpkeep <- tempfile("keep"))
  on.exit(unlink(c(tmpdrop, tmpkeep), recursive = TRUE), add = TRUE)

  tmpenv <- withr::local_tempfile()
  cat("", file = tmpenv)
  withr::local_envvar(c(R_ENVIRON_USER = tmpenv))

  withr::local_libpaths(tmpdrop, action = "prefix")

  test_paths(tmpdrop, tmpkeep)

  ## To close FDs
  gc()
})

test_that("libpath in system, R_LIBS in .Renviron", {
  # We remove the library with covr from the lib path, so this
  # cannot work in a subprocess.
  skip_in_covr()
  skip_if_not_installed("withr")

  dir.create(tmpdrop <- tempfile("drop"))
  dir.create(tmpkeep <- tempfile("keep"))
  on.exit(unlink(c(tmpdrop, tmpkeep), recursive = TRUE), add = TRUE)

  tmpenv <- withr::local_tempfile()
  cat("R_LIBS=\"", tmpdrop, "\"\n", sep = "", file = tmpenv)
  withr::local_envvar(c(R_ENVIRON_USER = tmpenv))

  withr::local_libpaths(tmpdrop, action = "prefix")

  # Since we add tmpdrop in .Renviron, it will only be dropped by --vanilla
  test_paths(tmpdrop, tmpkeep, system_drop = NULL)

  ## To close FDs
  gc()
})

test_that("libpath in system, R_LIBS", {
  # We remove the library with covr from the lib path, so this
  # cannot work in a subprocess.
  skip_in_covr()
  skip_if_not_installed("withr")

  dir.create(tmpdrop <- tempfile("drop"))
  dir.create(tmpkeep <- tempfile("keep"))
  on.exit(unlink(c(tmpdrop, tmpkeep), recursive = TRUE), add = TRUE)

  tmpenv <- withr::local_tempfile()
  cat("", file = tmpenv)
  withr::local_envvar(c(R_ENVIRON_USER = tmpenv, R_LIBS = tmpdrop))

  withr::local_libpaths(tmpdrop, action = "prefix")

  # R_LIBS is used by both R and R --vanilla, so it will not be dropped
  test_paths(tmpdrop, tmpkeep, system_drop = NULL, system_vanilla_drop = NULL)

  ## To close FDs
  gc()
})

test_that("libpath in system, R_LIBS and .Renviron", {
  # We remove the library with covr from the lib path, so this
  # cannot work in a subprocess.
  skip_in_covr()
  skip_if_not_installed("withr")

  dir.create(tmpdrop <- tempfile("drop"))
  dir.create(tmpkeep <- tempfile("keep"))
  on.exit(unlink(c(tmpdrop, tmpkeep), recursive = TRUE), add = TRUE)

  tmpenv <- withr::local_tempfile()
  cat("R_LIBS=\"", tmpdrop, "\"\n", sep = "", file = tmpenv)
  withr::local_envvar(c(R_ENVIRON_USER = tmpenv, R_LIBS = tmpdrop))

  withr::local_libpaths(tmpdrop, action = "prefix")

  # R_LIBS is used by both R and R --vanilla, so it will not be dropped
  test_paths(tmpdrop, tmpkeep, system_drop = NULL, system_vanilla_drop = NULL)

  ## To close FDs
  gc()
})

test_that("libpath in system, if subprocess changes R_LIBS", {
  dir.create(tmpkeep <- tempfile("keep"))
  on.exit(unlink(tmpkeep, recursive = TRUE), add = TRUE)

  rbin <- setup_r_binary_and_args(list())$bin
  rbin <- shQuote(rbin)

  f <- function(rbin, new) {
    Sys.setenv(R_LIBS = new)
    Sys.setenv(R_ENVIRON_USER = "no_such_file")
    system(
      paste(
        rbin,
        "--no-site-file --no-init-file --no-save --no-restore --slave",
        "-e \".libPaths()\""
      ),
      intern = TRUE
    )
  }

  out <- callr::r(f, list(rbin = rbin, new = tmpkeep))
  expect_true(any(grepl(basename(normalizePath(tmpkeep)), out)))

  ## Close FDs
  gc()
})

test_that("libpath in system, if subprocess changes R_LIBS #2", {
  if (.Platform$OS.type != "unix") skip("Unix only")

  dir.create(tmpkeep <- tempfile("keep"))
  on.exit(unlink(tmpkeep, recursive = TRUE), add = TRUE)

  rbin <- setup_r_binary_and_args(list())$bin

  f <- function(rbin, new) {
    Sys.setenv(R_LIBS = new)
    Sys.setenv(R_ENVIRON_USER = "no_such_file")
    env <- paste0("R_LIBS=", shQuote(new))
    tmp <- tempfile()
    on.exit(unlink(tmp), add = TRUE)
    system2(
      rbin,
      c(
        "--no-site-file",
        "--no-init-file",
        "--no-save",
        "--no-restore",
        "--slave",
        "-e",
        "'.libPaths()'"
      ),
      env = env,
      stdout = tmp
    )
    readLines(tmp)
  }

  out <- callr::r(f, list(rbin = rbin, new = tmpkeep))
  expect_true(any(grepl(basename(normalizePath(tmpkeep)), out)))

  ## Close FDs
  gc()
})

test_that("setting profile/environ variables in 'env'", {
  # See https://github.com/r-lib/callr/issues/193

  skip_in_covr()

  profile <- tempfile()
  on.exit(unlink(profile), add = TRUE)
  cat("foo <- '11'\n", file = profile)

  envfile <- tempfile()
  on.exit(unlink(envfile), add = TRUE)
  cat("MY_ENV2=MILES2\n", file = envfile)

  ret <- callr::r(
    function() {
      c(
        Sys.getenv("MY_ENV"),
        Sys.getenv("R_PROFILE_USER"),
        exists("foo"),
        Sys.getenv("MY_ENV2")
      )
    },
    user_profile = TRUE,
    env = c(
      "R_PROFILE_USER" = profile,
      "R_ENVIRON_USER" = envfile,
      "MY_ENV" = "MILES"
    )
  )

  expect_equal(ret, c("MILES", profile, "TRUE", "MILES2"))
})
