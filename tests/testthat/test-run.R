
context("run")

test_that("run can run", {

  px <- get_tool("px")
  expect_error({
    run(px, c("sleep", "0"))
  }, NA)
})

test_that("timeout works", {

  px <- get_tool("px")
  tic <- Sys.time()
  x <- run(px, c("sleep", "5"), timeout = 0.00001, error_on_status = FALSE)
  toc <- Sys.time()

  expect_true(toc - tic < as.difftime(3, units = "secs"))
  expect_true(x$timeout)
})

test_that("timeout throws right error", {

  px <- get_tool("px")
  e <- tryCatch(
    run(px, c("sleep", "5"), timeout = 0.00001, error_on_status = TRUE),
    error = function(e) e
  )

  expect_true("system_command_timeout_error" %in% class(e))
})

test_that("callbacks work", {

  px <- get_tool("px")
  ## This typically freezes on Unix, if there is a malloc/free race
  ## condition in the SIGCHLD handler.
  for (i in 1:30) {
    out <- NULL
    run(
      px, rbind("outln", 1:20),
      stdout_line_callback = function(x, ...) out <<- c(out, x)
    )
    expect_equal(out, as.character(1:20))
  }

  for (i in 1:30) {
    out <- NULL
    run(
      px, rbind("errln", 1:20),
      stderr_line_callback = function(x, ...) out <<- c(out, x),
      error_on_status = FALSE
    )
    expect_equal(out, as.character(1:20))
  }
})
