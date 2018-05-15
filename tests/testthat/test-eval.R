
context("r")

test_that("basic r", {
  expect_equal(r(function() 1 + 1), 2)
  expect_equal(r(function(x) 1 + x, list(5)), 6)
})

test_that("the same R version is called", {
  expect_equal(r(function() R.version), R.version)
})

test_that("standard output", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  r(function() cat("hello\n"), stdout = tmp)
  expect_equal(readLines(tmp), "hello")
})

test_that("standard error", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  r(function() message("hello"), stderr = tmp)
  expect_equal(readLines(tmp), "hello")
})

test_that("standard output and standard error", {
  tmp_out <- tempfile("stdout-")
  tmp_err <- tempfile("stderr-")
  on.exit(unlink(c(tmp_out, tmp_err)), add = TRUE)
  expect_silent(
    r(function() { cat("hello world!\n"); message("hello again!") },
      stdout = tmp_out, stderr = tmp_err)
  )
  expect_equal(readLines(tmp_out), "hello world!")
  expect_equal(readLines(tmp_err), "hello again!")
})

test_that("cmdargs argument", {
  o1 <- tempfile()
  o2 <- tempfile()
  on.exit(unlink(c(o1, o2)), add = TRUE)

  r(function() ls(), stdout = o1)
  r(function() ls(), stdout = o2, cmdargs = character())

  expect_true(length(readLines(o2)) > length(readLines(o1)))
})

test_that("env", {

  expect_equal(
    r(function() Sys.getenv("CALLR_FOOBAR"),
      env = c(CALLR_FOOBAR = "indeed")
      ),
    "indeed"
  )
})

test_that("stdout and stderr in the same file", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)


  r(function() {
      cat("hello1\n")
      Sys.sleep(0.1)
      message("hello2")
      Sys.sleep(0.1)
      cat("hello3\n")
    },
    stdout = tmp, stderr = tmp
  )

  expect_equal(readLines(tmp), paste0("hello", 1:3))
})

test_that("profiles are used as requested", {
  do <- function(system, user) {
    tmp1 <- tempfile()
    tmp2 <- tempfile()
    on.exit(unlink(c(tmp1, tmp2)), add = TRUE)
    cat("Sys.setenv(FOO = 'bar')\n", file = tmp1)
    cat("Sys.setenv(NAH = 'doh')\n", file = tmp2)
    withr::with_envvar(list(R_PROFILE = tmp1, R_PROFILE_USER = tmp2),  {
      res <- r(
        function() c(Sys.getenv("FOO", ""), Sys.getenv("NAH", "")),
        system_profile = system, user_profile = user)
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
})

test_that(".Renviron is used, but lib path is set over it", {
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  withr::with_dir(tmp, {
    ## Create .Renviron file
    dir.create("not")
    dir.create("yes")
    cat("R_LIBS=\"", file.path(getwd(), "not"), "\"\n",
        sep = "", file = ".Renviron")
    cat("FOO=bar\n", file = ".Renviron", append = TRUE)

    ## R CMD check sets R_ENVIRON and R_ENVIRON_USER to empty,
    ## so we need to recover that.
    withr::with_envvar(c(R_ENVIRON_USER=".Renviron"), {
      res <- r(
        function() list(.libPaths(), Sys.getenv("FOO")),
        libpath = c(file.path(getwd(), "yes"), .libPaths())
      )
    })
  })

  expect_equal(basename(res[[1]][1]), "yes")
  expect_equal(res[[2]], "bar")
})
