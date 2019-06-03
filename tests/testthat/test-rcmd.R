
context("rcmd")

test_that("rcmd works", {
  expect_equal(rcmd("config", "CC")$status, 0)
  expect_match(rcmd("config", "CC")$stdout, ".")
  gc()
})

test_that("rcmd show works", {
  expect_output(rcmd("config", "CC", show = TRUE), ".")
  gc()
})

test_that("rcmd echo works", {
  expect_output(rcmd("config", "CC", echo = TRUE), "config\\s+CC")
  gc()
})

test_that("rcmd_safe", {
  expect_equal(rcmd_safe("config", "CC")$status, 0)
  gc()
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
  gc()
})

test_that("fail_on_status", {
  rand <- tempfile()
  expect_error(
    withr::with_dir(
      tempdir(),
      rcmd("BATCH", rand, fail_on_status = TRUE)),
    "System command error",
    class = "system_command_status_error"
  )
  expect_silent(
    out <- withr::with_dir(
      tempdir(),
      rcmd("BATCH", rand, fail_on_status = FALSE))
  )
  expect_true(out$status != 0)
  gc()
})

test_that("command is included in result", {
  res <- rcmd_safe("config", "CC")
  expect_false(is.null(res$command))
  expect_true(is.character(res$command))
  gc()
})

test_that("stderr -> stdout", {
  lib <- test_temp_dir()
  pkg <- test_temp_dir()
  file.copy("fixtures/D1", file.path(pkg, "DESCRIPTION"))
  out <- rcmd("INSTALL", c("-l", lib, pkg))
  expect_match(out$stdout, "No man pages found")
  expect_match(out$stderr, "installing help indices")

  out2 <- rcmd("INSTALL", c("-l", lib, pkg), stderr = "2>&1")
  expect_equal(out2$status, 0L)
  expect_match(
    out2$stdout,
    "installing.*No man pages found.*testing if installed package")
  expect_equal(out2$stderr, "")

  out3 <- test_temp_file(create = FALSE)
  rcmd("INSTALL", c("-l", lib, pkg), stdout = out3, stderr = out3)
  expect_match(
    readChar(out3, nchars = file.info(out3)$size),
    "installing.*No man pages found.*testing if installed package")
  gc()
})
