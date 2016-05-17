
context("errors")

test_that("error is propagated", {
  expect_error(
    r(function() 1 + "A"),
    "non-numeric argument to binary operator"
  )
})

test_that("error object is passed", {
  err <- NULL
  tryCatch(
    r(function() 1 + "A"),
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
  expect_true(inherits(err, "callrError"))
  expect_equal(length(err$stack), 3)
})

test_that("debugger is called", {

  called <- FALSE

  with_mock(
    `utils::debugger` = function(...) called <<- TRUE,
    r(function() { 1 + "A" }, error = "debugger")
  )

  expect_true(called)
})
