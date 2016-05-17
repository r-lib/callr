
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
