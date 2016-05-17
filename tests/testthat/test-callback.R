
context("callbacks")

test_that("show option works", {

  expect_output(
    r(function() print("hello"), show = TRUE),
    "hello"
  )
})

test_that("callbacks work", {
  out <- NULL
  r(function() cat("hello\n"), callback = function(x) out <<- x)
  expect_equal(out, "hello")
})

test_that("show and callbacks at the same time", {
  out <- NULL

  expect_output(
    r(
      function() cat("hello\n"),
      show = TRUE,
      callback = function(x) out <<- x
    ),
    "hello"
  )

  expect_equal(out, "hello")
})
