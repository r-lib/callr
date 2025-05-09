test_that("basic r", {
  expect_equal(r(function() 1 + 1), 2)
  expect_equal(r(function(x) 1 + x, list(5)), 6)
  gc()
})

test_that("the same R version is called", {
  expect_equal(r(function() R.version), R.version)
  gc()
})

test_that("standard output", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  r(function() cat("hello\n"), stdout = tmp)
  expect_equal(readLines(tmp), "hello")
  gc()
})

test_that("standard error", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  r(function() message("hello"), stderr = tmp)
  expect_equal(readLines(tmp), "hello")
  gc()
})

test_that("standard output and standard error", {
  tmp_out <- tempfile("stdout-")
  tmp_err <- tempfile("stderr-")
  on.exit(unlink(c(tmp_out, tmp_err)), add = TRUE)
  expect_silent(
    r(
      function() {
        cat("hello world!\n")
        message("hello again!")
      },
      stdout = tmp_out,
      stderr = tmp_err
    )
  )
  expect_equal(readLines(tmp_out), "hello world!")
  expect_equal(readLines(tmp_err), "hello again!")
  gc()
})

test_that("cmdargs argument", {
  o1 <- tempfile()
  o2 <- tempfile()
  on.exit(unlink(c(o1, o2)), add = TRUE)

  r(function() ls(), stdout = o1)
  r(function() ls(), stdout = o2, cmdargs = character())

  expect_true(length(readLines(o2)) > length(readLines(o1)))
  gc()
})

test_that("env", {
  expect_equal(
    r(function() Sys.getenv("CALLR_FOOBAR"), env = c(CALLR_FOOBAR = "indeed")),
    "indeed"
  )
  gc()
})

test_that("stdout and stderr in the same file", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  r(
    function() {
      cat("hello1\n")
      Sys.sleep(0.1)
      message("hello2")
      Sys.sleep(0.1)
      cat("hello3\n")
    },
    stdout = tmp,
    stderr = tmp
  )

  expect_equal(readLines(tmp), paste0("hello", 1:3))
  gc()
})

test_that("profiles are used as requested", {
  skip_if_not_installed("withr")
  do <- function(system, user) {
    tmp1 <- tempfile()
    tmp2 <- tempfile()
    on.exit(unlink(c(tmp1, tmp2)), add = TRUE)
    cat("Sys.setenv(FOO = 'bar')\n", file = tmp1)
    cat("Sys.setenv(NAH = 'doh')\n", file = tmp2)
    withr::with_envvar(list(R_PROFILE = tmp1, R_PROFILE_USER = tmp2), {
      res <- r(
        function() c(Sys.getenv("FOO", ""), Sys.getenv("NAH", "")),
        system_profile = system,
        user_profile = user
      )
    })
  }

  ## None
  res <- do(FALSE, FALSE)
  expect_equal(res, c("", ""))

  ## System
  res <- do(TRUE, FALSE)
  expect_equal(res, c("bar", ""))

  ## User
  res <- do(FALSE, TRUE)
  expect_equal(res, c("", "doh"))

  ## Both
  res <- do(TRUE, TRUE)
  expect_equal(res, c("bar", "doh"))
  gc()
})

test_that(".Renviron is used, but lib path is set over it", {
  skip_if_not_installed("withr")
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  withr::with_dir(tmp, {
    ## Create .Renviron file
    dir.create("not")
    dir.create("yes")
    cat(
      "R_LIBS=\"",
      file.path(getwd(), "not"),
      "\"\n",
      sep = "",
      file = ".Renviron"
    )
    cat("FOO=bar\n", file = ".Renviron", append = TRUE)

    ## R CMD check sets R_ENVIRON and R_ENVIRON_USER to empty,
    ## so we need to recover that.
    withr::with_envvar(c(R_ENVIRON_USER = ".Renviron"), {
      res <- r(
        function() list(.libPaths(), Sys.getenv("FOO")),
        libpath = c(file.path(getwd(), "yes"), .libPaths())
      )
    })
  })

  expect_equal(basename(res[[1]][1]), "yes")
  expect_equal(res[[2]], "bar")
  gc()
})

test_that("errors are printed on stderr", {
  ## See https://github.com/r-lib/callr/issues/80
  f <- function() {
    print("send to stdout")
    message("send to stderr")
    stop("send to stderr 2")
  }

  expect_snapshot(error = TRUE, {
    r(f, stdout = out <- tempfile(), stderr = err <- tempfile())
  })
  on.exit(unlink(c(out, err), recursive = TRUE), add = TRUE)

  expect_false(any(grepl("send to stderr", readLines(out))))
  expect_true(any(grepl("send to stderr 2", readLines(err))))
  gc()
})

test_that("stdout and stderr are interleaved correctly", {
  f <- function() {
    cat("stdout", file = stdout())
    cat("stderr", file = stderr())
    cat("stdout2\n", file = stdout())
  }

  on.exit(unlink(out, recursive = TRUE), add = TRUE)

  r(f, stdout = out <- tempfile(), stderr = "2>&1")
  expect_equal(readLines(out), "stdoutstderrstdout2")
  unlink(out)

  r(f, stdout = out, stderr = out)
  expect_equal(readLines(out), "stdoutstderrstdout2")
  gc()
})

test_that("callr messages do not cause problems", {
  do <- function() {
    cnd <- structure(
      list(message = "foobar"),
      class = c("callr_message", "condition")
    )
    signalCondition(cnd)
    signalCondition(cnd)
    signalCondition(cnd)
    cat("stdout\n")
    message("stderr")
    "hi"
  }

  out <- tempfile()
  err <- tempfile()
  on.exit(unlink(c(out, err)), add = TRUE)
  ret <- callr::r(do, stdout = out, stderr = err, poll_connection = FALSE)

  expect_equal(ret, "hi")
  expect_equal(readLines(out), "stdout")
  expect_equal(readLines(err), "stderr")
})

test_that("cleans up temp files", {
  skip_on_cran()

  rsc <- function() {
    library(callr)
    old <- dir(tempdir(), pattern = "^callr-")

    result <- callr::r(function() 1 + 1)

    unloadNamespace("callr")

    new <- setdiff(dir(tempdir(), "^callr-"), old)

    list(result = result, new = new)
  }

  out <- r(rsc)
  expect_identical(out$result, 2)
  expect_identical(out$new, character())
})

test_that("local .Rprofile is loaded", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp)
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(tmp)
  cat("aa <- 123\n", file = ".Rprofile")
  out <- callr::r(function() aa)
  expect_equal(out, 123)
})

test_that("local .Rprofile is not loaded from actual wd", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp)
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(tmp)

  dir.create(wd2 <- tempfile())
  on.exit(unlink(wd2, recursive = TRUE), add = TRUE)
  cat("aa <- 123\n", file = ".Rprofile")
  out <- callr::r(function() ls(.GlobalEnv), wd = wd2)
  expect_equal(out, character())
})

test_that("local .Rprofile is not loaded recursively", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp)
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(tmp)

  expr <- quote({
    rprofile <- Sys.getenv("R_PROFILE_USER", "~/.Rprofile")
    if (file.exists(rprofile)) source(rprofile)
    rm(rprofile)
    aa <- 123
  })
  cat(deparse(expr), file = ".Rprofile", sep = "\n")
  out <- callr::r(function() aa)
  expect_equal(out, 123)
})

test_that("symbolic arguments are protected", {
  expect_equal(
    callr::r(function(x) x, list(x = quote(foobar))),
    quote(foobar)
  )
})
