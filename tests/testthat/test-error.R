
test_that("error is propagated, .Last.error is set", {
  expect_r_process_snapshot(
    callr::r(function() 1 + "A", error = "error"),
    .Last.error,
    transform = redact_srcref
  )
})

test_that("error is propagated, printed if non-interactive mode", {
  expect_r_process_snapshot(
    callr::r(function() 1 + "A", error = "error"),
    interactive = FALSE,
    transform = redact_srcref
  )
})

test_that("error stack is passed, .Last.error is set", {
  expect_r_process_snapshot(
    callr::r(
      function() {
        f <- function() g()
        g <- function() 1 + "A"
        f()
      },
      error = "stack"
    ),
    .Last.error,
    transform = redact_srcref
  )
})

test_that("error behavior can be set using option", {
  skip_if_not_installed("withr")
  withr::local_options(callr.error = "error")
  expect_snapshot(
    error = TRUE,
    r(function() 1 + "A")
  )

  withr::local_options(callr.error = "stack")
  expect_snapshot(
    error = TRUE,
    r(
      function() {
        f <- function() g()
        g <- function() 1 + "A"
        f()
      }
    )
  )
})

test_that("parent errors", {
  skip_if_not_installed("withr")
  withr::local_options(list("callr.error" = "error"))
  expect_snapshot({
    err <- tryCatch(
      r(function() 1 + "A"),
      error = function(e) e
    )
    err$parent
  })
})

test_that("parent errors, another level", {
  skip_if_not_installed("withr")
  withr::local_options(list("callr.error" = "error"))
  expect_snapshot({
    err <- tryCatch(
      callr::r(function() {
        withr::local_options(list("callr.error" = "error"))
        callr::r(function() 1 + "A")
      }),
      error = function(e) e
    )
    err$parent
    err$parent$parent
  })
})

test_that("error traces are printed recursively", {
  expect_r_process_snapshot(
    callr::r(function() callr::r(function() 1 + "a")),
    interactive = FALSE,
    transform = redact_srcref
  )
})

test_that("errors in r_bg() are merged", {
  skip_if_not_installed("withr")
  withr::local_options(list("callr.error" = "error"))

  p <- r_bg(function() 1 + "A")
  on.exit(p$kill(), add = TRUE)
  p$wait(2000)

  expect_snapshot(
    error = TRUE,
    p$get_result()
  )
})

test_that("errors in r_process are merged", {
  skip_if_not_installed("withr")
  withr::local_options(list("callr.error" = "error"))

  opts <- r_process_options(func = function() 1 + "A")
  p <- r_process$new(opts)
  on.exit(p$kill(), add = TRUE)
  p$wait(2000)

  expect_snapshot(
    error = TRUE,
    p$get_result()
  )
})

test_that("errors in r_session$run() are merged", {
  rs <- r_session$new()
  on.exit(rs$kill(), add = TRUE)

  expect_snapshot(
    error = TRUE,
    rs$run(function() 1 + "A")
  )

  expect_snapshot(
    error = TRUE,
    rs$run(function() 1 + "A")
  )
})

test_that("errors in r_session$call() are merged", {
  rs <- r_session$new()
  on.exit(rs$kill(), add = TRUE)

  rs$call(function() 1 + "A")
  rs$poll_process(2000)
  expect_snapshot(rs$read()$error)

  rs$call(function() 1 + "A")
  rs$poll_process(2000)
  expect_snapshot(rs$read()$error)
})

test_that("child error is not modified", {
  expect_snapshot({
    err <- tryCatch(callr::r(function() stop("foobar")), error = function(e) e)
    err
    class(err)
    class(err$parent)
  })
})

test_that("new_callr_error, timeout", {
  expect_r_process_snapshot(
    callr::r(function() Sys.sleep(3), timeout = 1/5),
    transform = redact_srcref
  )
  expect_snapshot(
    error = TRUE,
    callr::r(function() Sys.sleep(3), timeout = 1/5)
  )
})

test_that("interrupting an R session", {
  # Not a great test, because it is timing dependent, especially bad
  # on Windows, where it takes a bit longer to start running the command.
  skip_on_cran()

  rs <- r_session$new()
  on.exit(rs$close(), add = TRUE)
  rs$call(function() Sys.sleep(3))
  # wait a bit so it starts running
  Sys.sleep(0.2)
  rs$interrupt()
  rs$poll_io(3000)

  expect_snapshot(
    rs$read(),
    transform = redact_callr_rs_result
  )
})

test_that("format.call_status_error", {
  err <- tryCatch(
    callr::r(function() 1 + ""),
    error = function(e) e
  )
  expect_snapshot(format(err))
  expect_snapshot(print(err))

  err <- tryCatch(
    callr::r(function() 1 + "", error = "stack"),
    error = function(e) e
  )
  expect_snapshot(format(err))
  expect_snapshot(print(err))
})

test_that("format.call_status_error 2", {
  skip_if_not_installed("withr")
  expect_r_process_snapshot(
    withr::local_options(rlib_error_always_trace = TRUE),
    err <- tryCatch(
      callr::r(function() 1 + ""),
      error = function(e) e
    ),
    writeLines(format(err, trace = TRUE)),
    interactive = FALSE,
    transform = redact_srcref
  )
})

test_that("stdout/stderr is printed on error", {
  expect_r_process_snapshot(
    callr::r(function() {
      warning("I have a bad feeling about this")
      stop("told ya")
    }),
    .Last.error,
    .Last.error$stderr,
    interactive = TRUE,
    transform = function(x) fix_eol(redact_srcref(x))
  )
})

test_that("stdout/stderr is printed on error 2", {
  expect_r_process_snapshot(
    callr::r(function() {
      writeLines("Just some output")
      stop("told ya")
    }),
    .Last.error,
    .Last.error$stdout,
    interactive = TRUE,
    transform = function(x) fix_eol(redact_srcref(x))
  )
})

test_that("stdout/stderr is printed on error 3", {
  expect_r_process_snapshot(
    callr::r(function() {
      writeLines("Just some output")
      warning("I have a bad feeling about this")
      stop("told ya")
    }),
    interactive = FALSE,
    transform = redact_srcref
  )
})

test_that("error is printed to file", {
  tmp <- tempfile("callr-test")
  on.exit(unlink(tmp), add = TRUE)
  err <- tryCatch(
    callr::r(function() stop("ouch"), stderr = tmp),
    error = function(e) e
  )
  expect_snapshot(
    err$stderr,
    transform = function(x) fix_eol(redact_srcref(x))
  )
  expect_snapshot(readLines(tmp))
})
