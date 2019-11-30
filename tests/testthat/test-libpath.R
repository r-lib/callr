
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

test_that("libpath in system(), empty .Renviron", {

  dir.create(tmpdrop <- tempfile("drop"))
  dir.create(tmpkeep <- tempfile("keep"))
  on.exit(unlink(c(tmpdrop, tmpkeep), recursive = TRUE), add  = TRUE)

  withr::local_tempfile("tmpenv")
  cat("", file = tmpenv)
  withr::local_envvar(c(R_ENVIRON_USER = tmpenv))

  withr::local_libpaths(tmpdrop, action = "prefix")

  test_paths(tmpdrop, tmpkeep)

  ## To close FDs
  gc()
})

test_that("libpath in system, R_LIBS in .Renviron", {

  dir.create(tmpdrop <- tempfile("drop"))
  dir.create(tmpkeep <- tempfile("keep"))
  on.exit(unlink(c(tmpdrop, tmpkeep), recursive = TRUE), add  = TRUE)

  withr::local_tempfile("tmpenv")
  cat("R_LIBS=\"", tmpdrop, "\"\n", sep = "", file = tmpenv)
  withr::local_envvar(c(R_ENVIRON_USER = tmpenv))

  withr::local_libpaths(tmpdrop, action = "prefix")

  # Since we add tmpdrop in .Renviron, it will only be dropped by --vanilla
  test_paths(tmpdrop, tmpkeep, system_drop = NULL)

  ## To close FDs
  gc()
})

test_that("libpath in system, R_LIBS", {
  dir.create(tmpdrop <- tempfile("drop"))
  dir.create(tmpkeep <- tempfile("keep"))
  on.exit(unlink(c(tmpdrop, tmpkeep), recursive = TRUE), add  = TRUE)

  withr::local_tempfile("tmpenv")
  cat("", file = tmpenv)
  withr::local_envvar(c(R_ENVIRON_USER = tmpenv, R_LIBS=tmpdrop))

  withr::local_libpaths(tmpdrop, action = "prefix")

  # R_LIBS is used by both R and R --vanilla, so it will not be dropped
  test_paths(tmpdrop, tmpkeep, system_drop = NULL, system_vanilla_drop = NULL)

  ## To close FDs
  gc()
})

test_that("libpath in system, R_LIBS and .Renviron", {
  dir.create(tmpdrop <- tempfile("drop"))
  dir.create(tmpkeep <- tempfile("keep"))
  on.exit(unlink(c(tmpdrop, tmpkeep), recursive = TRUE), add  = TRUE)

  withr::local_tempfile("tmpenv")
  cat("R_LIBS=\"", tmpdrop, "\"\n", sep = "", file = tmpenv)
  withr::local_envvar(c(R_ENVIRON_USER = tmpenv, R_LIBS=tmpdrop))

  withr::local_libpaths(tmpdrop, action = "prefix")

  # R_LIBS is used by both R and R --vanilla, so it will not be dropped
  test_paths(tmpdrop, tmpkeep, system_drop = NULL, system_vanilla_drop = NULL)

  ## To close FDs
  gc()
})

test_that("libpath in system, if subprocess changes R_LIBS", {
  dir.create(tmpkeep <- tempfile("keep"))
  on.exit(unlink(tmpkeep, recursive = TRUE), add  = TRUE)

  rbin <- setup_r_binary_and_args(list())$bin
  rbin <- shQuote(rbin)

  f <- function(rbin, new) {
    Sys.setenv(R_LIBS = new)
    Sys.setenv(R_ENVIRON_USER = "no_such_file")
    system(paste(
      rbin, "--no-site-file --no-init-file --no-save --no-restore --slave",
      "-e \".libPaths()\""), intern = TRUE)
  }

  out <- callr::r(f, list(rbin = rbin, new = tmpkeep))
  expect_true(any(grepl(basename(normalizePath(tmpkeep)), out)))

  ## Close FDs
  gc()
})

test_that("libpath in system, if subprocess changes R_LIBS #2", {

  if (.Platform$OS.type != "unix") skip("Unix only")

  dir.create(tmpkeep <- tempfile("keep"))
  on.exit(unlink(tmpkeep, recursive = TRUE), add  = TRUE)

  rbin <- setup_r_binary_and_args(list())$bin

  f <- function(rbin, new) {
    Sys.setenv(R_LIBS = new)
    Sys.setenv(R_ENVIRON_USER = "no_such_file")
    env <- paste0("R_LIBS=", shQuote(new))
    tmp <- tempfile()
    on.exit(unlink(tmp), add = TRUE)
    system2(
      rbin,
      c("--no-site-file", "--no-init-file", "--no-save",
        "--no-restore", "--slave", "-e", "'.libPaths()'"),
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

test_that("libpath in system, in R CMD INSTALL", {
  skip_on_cran()

  csomag <- test_path("fixtures","csomag")
  tmplib <- tempfile()
  dir.create(tmplib)
  on.exit(unlink(tmplib, recursive = TRUE), add = TRUE)
  dump <- tempfile(fileext = ".rds")
  on.exit(unlink(dump), add = TRUE)

  out <- callr::r(function(pkg, lib, savefile) {
    Sys.setenv(CALLR_DUMP_HERE = savefile)
    ## We need to do this, otherwise install.packages() only keeps the
    ## first library
    Sys.unsetenv("_R_CHECK_INSTALL_DEPENDS_")
    install.packages(pkg, lib = lib, repos = NULL, type = "source")
  }, list(csomag, tmplib, dump))

  data <- readRDS(dump)
  ## We test the basename, in case a normalizePath makes dirnames differ
  expect_true(basename(tmplib) %in% basename(data$libpaths))

  ## Close FDs
  gc()
})
