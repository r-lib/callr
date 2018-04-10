
context("SIGCHLD handler interference")

test_that("is_alive()", {

  skip_other_platforms("unix")
  skip_on_cran()

  library(parallel)

  px <- process$new("sleep", "1")
  on.exit(try(px$kill(), silent = TRUE), add = TRUE)

  p <- mcparallel(Sys.sleep(1))
  q <- mcparallel(Sys.sleep(1))
  res <- mccollect(list(p, q))
  expect_false(px$is_alive())
  expect_identical(px$get_exit_status(), NA_integer_)
})

test_that("finalizer", {

  skip_other_platforms("unix")
  skip_on_cran()

  library(parallel)

  px <- process$new("sleep", "1")
  on.exit(try(px$kill(), silent = TRUE), add = TRUE)

  p <- mcparallel(Sys.sleep(1))
  q <- mcparallel(Sys.sleep(1))
  res <- mccollect(list(p, q))
  expect_error({ rm(px); gc() }, NA)
})

test_that("get_exit_status", {

  skip_other_platforms("unix")
  skip_on_cran()

  library(parallel)

  px <- process$new("sleep", "1")
  on.exit(try(px$kill(), silent = TRUE), add = TRUE)

  p <- mcparallel(Sys.sleep(1))
  q <- mcparallel(Sys.sleep(1))
  res <- mccollect(list(p, q))
  expect_identical(px$get_exit_status(), NA_integer_)
})

test_that("signal", {

  skip_other_platforms("unix")
  skip_on_cran()

  library(parallel)

  px <- process$new("sleep", "1")
  on.exit(try(px$kill(), silent = TRUE), add = TRUE)

  p <- mcparallel(Sys.sleep(1))
  q <- mcparallel(Sys.sleep(1))
  res <- mccollect(list(p, q))
  expect_false(px$signal(2))            # SIGINT
  expect_true(print(px$get_exit_status()) %in% c(0L, NA_integer_))
})

test_that("kill", {

  skip_other_platforms("unix")
  skip_on_cran()

  library(parallel)

  px <- process$new("sleep", "1")
  on.exit(try(px$kill(), silent = TRUE), add = TRUE)

  p <- mcparallel(Sys.sleep(1))
  q <- mcparallel(Sys.sleep(1))
  res <- mccollect(list(p, q))
  expect_false(px$kill())
  expect_identical(px$get_exit_status(), NA_integer_)
})

test_that("SIGCHLD handler", {

  skip_other_platforms("unix")
  skip_on_cran()

  library(parallel)

  px <- process$new("sleep", "1")
  on.exit(try(px$kill(), silent = TRUE), add = TRUE)

  p <- mcparallel(Sys.sleep(1))
  q <- mcparallel(Sys.sleep(1))
  res <- mccollect(list(p, q))

  expect_error({
    px2 <- process$new("true")
    on.exit(try(px2$kill(), silent = TRUE), add = TRUE)
    px2$wait(1)
  }, NA)

  expect_identical(px$get_exit_status(), NA_integer_)
})
