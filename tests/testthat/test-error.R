
context("errors")

test_that("error is propagated", {
  expect_error(
    r(function() 1 + "A", error = "error"),
    "non-numeric argument to binary operator"
  )
  gc()
})

test_that("error object is passed", {
  err <- NULL
  tryCatch(
    r(function() 1 + "A", error = "error"),
    error = function(e) err <<- e
  )
  expect_true(inherits(err, "rlib_error"))
  gc()
})

test_that("error stack is passed", {
  err <- NULL
  tryCatch(
    r(
      function() {
        f <- function() g()
        g <- function() 1 + "A"
        f()
      },
      error = "stack"
    ),
    error = function(e) err <<- e
  )

  expect_true("call" %in% names(err))
  expect_true(inherits(err, "error"))
  expect_true(inherits(err, "callr_error"))
  expect_equal(length(err$stack), 3)
  gc()
})

test_that("error behavior can be set using option", {
  err <- NULL
  withr::with_options(c(callr.error = "error"), {
    tryCatch(
      r(function() 1 + "A"),
      error = function(e) err <<- e
    )
  })
  expect_true(inherits(err, "rlib_error"))

  err <- NULL
  withr::with_options(c(callr.error = "stack"), {
    tryCatch(
      r(
        function() {
          f <- function() g()
          g <- function() 1 + "A"
          f()
        }
      ),
      error = function(e) err <<- e
    )
  })

  expect_true("call" %in% names(err))
  expect_true(inherits(err, "error"))
  expect_true(inherits(err, "callr_error"))
  expect_equal(length(err$stack), 3)
  gc()
})

test_that("parent errors", {
  withr::local_options(list("callr.error" = "error"))
  err <- tryCatch(
    r(function() 1 + "A"),
    error = function(e) e)

  expect_s3_class(err, "rlib_error")
  expect_s3_class(err$parent, "simpleError")
  expect_match(
    conditionMessage(err),
    "callr subprocess failed: non-numeric argument")
  expect_match(
    conditionMessage(err$parent),
    "non-numeric argument")
})

test_that("parent errors, another level", {
  withr::local_options(list("callr.error" = "error"))
  err <- tryCatch(
    callr::r(function() {
      withr::local_options(list("callr.error" = "error"))
      callr::r(function() 1 + "A")
    }),
    error = function(e) e)

  expect_s3_class(err, "rlib_error")
  expect_s3_class(err$parent, "rlib_error")
  expect_s3_class(err$parent$parent, "simpleError")

  expect_match(
    conditionMessage(err),
    "callr subprocess failed: callr subprocess failed: non-numeric")
  expect_match(
    conditionMessage(err$parent),
    "callr subprocess failed: non-numeric argument")
  expect_match(
    conditionMessage(err$parent$parent),
    "non-numeric argument")
})

test_that("error traces are merged", {
  skip_on_cran()

  sf <- tempfile(fileext = ".R")
  op <- sub("\\.R$", ".rds", sf)
  so <- paste0(sf, "out")
  se <- paste0(sf, "err")
  on.exit(unlink(c(sf, op, so, se), recursive = TRUE), add = TRUE)

  expr <- substitute({
    h <- function() callr::r(function() 1 + "a")
    options(rlib_error_handler = function(c) {
      saveRDS(c, file = `__op__`)
      # quit after the first, because the other one is caught here as well
      q()
    })
    h()
  }, list("__op__" = op))

  cat(deparse(expr), file = sf, sep = "\n")

  callr::rscript(sf, stdout = so, stderr = se)

  cond <- readRDS(op)

  expect_s3_class(cond, "rlib_error")
  expect_s3_class(cond$parent, "error")
  expect_s3_class(cond$trace, "rlib_trace")

  expect_equal(length(cond$trace$nframe), 2)
  expect_true(cond$trace$nframe[1] < cond$trace$nframe[2])
  expect_match(cond$trace$messages[[1]], "subprocess failed: non-numeric")
  expect_match(cond$trace$messages[[2]], "non-numeric argument")
})

test_that("errors in r_bg() are merged", {
  skip_on_cran()

  withr::local_options(list("callr.error" = "error"))

  p <- r_bg(function() 1 + "A")
  on.exit(p$kill(), add = TRUE)
  p$wait(2000)

  err <- tryCatch(
    p$get_result(),
    error = function(e) e)

  expect_s3_class(err, "rlib_error")
  expect_s3_class(err$parent, "simpleError")
  expect_match(
    conditionMessage(err),
    "callr subprocess failed: non-numeric argument")
  expect_match(
    conditionMessage(err$parent),
    "non-numeric argument")
})

test_that("errors in r_process are merged", {
  skip_on_cran()
  withr::local_options(list("callr.error" = "error"))

  opts <- r_process_options(func = function() 1 + "A")
  p <- r_process$new(opts)
  on.exit(p$kill(), add = TRUE)
  p$wait(2000)

  err <- tryCatch(
    p$get_result(),
    error = function(e) e)

  expect_s3_class(err, "rlib_error")
  expect_s3_class(err$parent, "simpleError")
  expect_match(
    conditionMessage(err),
    "callr subprocess failed: non-numeric argument")
  expect_match(
    conditionMessage(err$parent),
    "non-numeric argument")
})

test_that("errors in r_session$run() are merged", {
  rs <- r_session$new()
  on.exit(rs$kill(), add = TRUE)

  err1 <- tryCatch(
    rs$run(function() 1 + "A"),
    error = function(e) e)
  err2 <- tryCatch(
    rs$run(function() 1 + "A"),
    error = function(e) e)
  err <- list(err1, err2)

  for (i in seq_along(err)) {
    expect_s3_class(err[[i]], "rlib_error")
    expect_s3_class(err[[i]]$parent, "simpleError")
    expect_match(
      conditionMessage(err[[i]]),
      "callr subprocess failed: non-numeric argument")
    expect_match(
      conditionMessage(err[[i]]$parent),
      "non-numeric argument")
  }
})

test_that("errors in r_session$call() are merged", {
  rs <- r_session$new()
  on.exit(rs$kill(), add = TRUE)

  rs$call(function() 1 + "A")
  rs$poll_process(2000)
  err1 <- rs$read()$error

  rs$call(function() 1 + "A")
  rs$poll_process(2000)
  err2 <- rs$read()$error

  err <- list(err1, err2)

  for (i in seq_along(err)) {
    expect_s3_class(err[[i]], "rlib_error")
    expect_s3_class(err[[i]]$parent, "simpleError")
    expect_match(
      conditionMessage(err[[i]]),
      "callr subprocess failed: non-numeric argument")
    expect_match(
      conditionMessage(err[[i]]$parent),
      "non-numeric argument")
  }
})
