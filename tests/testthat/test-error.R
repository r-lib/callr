
context("errors")

test_that("error is propagated", {
  expect_error(
    r(function() 1 + "A", error = "error"),
    "non-numeric argument to binary operator"
  )
})

test_that("error object is passed", {
  err <- NULL
  tryCatch(
    r(function() 1 + "A", error = "error"),
    error = function(e) err <<- e
  )
  expect_true("call" %in% names(err))
  expect_true(inherits(err, "error"))
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
})

test_that("error behavior can be set using option", {
  err <- NULL
  withr::with_options(c(callr.error = "error"), {
    tryCatch(
      r(function() 1 + "A"),
      error = function(e) err <<- e
    )
  })
  expect_true("call" %in% names(err))
  expect_true(inherits(err, "error"))

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
})
