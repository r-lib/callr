
context("rcmd")

test_that("rcmd works", {
  expect_equal(rcmd("config", "CC")$status, 0)
  expect_match(rcmd("config", "CC")$stdout, ".")
})

test_that("rcmd show works", {
  expect_output(rcmd("config", "CC", show = TRUE), ".")
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
