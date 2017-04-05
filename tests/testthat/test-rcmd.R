
context("rcmd")

test_that("rcmd works", {
  expect_equal(rcmd("config", "CC")$status, 0)
  expect_match(rcmd("config", "CC")$stdout, ".")
})

test_that("rcmd show works", {
  expect_output(rcmd("config", "CC", show = TRUE), ".")
})

test_that("rcmd echo works", {
  expect_output(rcmd("config", "CC", echo = TRUE), "config CC")
})

test_that("rcmd on windows", {

  wbin <- NULL
  wargs <- NULL

  with_mock(
    `callr::os_platform` = function() "windows",
    `callr::run_r` = function(bin, args, ...) {
      wbin <<- bin; wargs <<- args
    },
    rcmd("config", "CC")
  )

  expect_match(wbin, "Rcmd.exe")
  expect_equal(wargs, c("config", "CC"))
})

test_that("rcmd_safe", {
  expect_equal(rcmd_safe("config", "CC")$status, 0)
})

test_that("wd argument", {
  tmp <- tempfile(fileext = ".R")
  tmpout <- paste0(tmp, "out")
  cat("print(getwd())", file = tmp)

  mywd <- getwd()

  rcmd("BATCH", c(tmp, tmpout), wd = tempdir())

  expect_equal(mywd, getwd())
  expect_match(
    paste(readLines(tmpout), collapse = "\n"),
    basename(tempdir())
  )
})

test_that("fail_on_status", {
  rand <- basename(tempfile())
  expect_error(rcmd("BATCH", rand, fail_on_status = TRUE))
  expect_silent(out <- rcmd("BATCH", rand, fail_on_status = FALSE))
  expect_true(out$status != 0)
})
