
context("r_session")

test_that("regular use", {
  rs <- r_session$new()
  on.exit(rs$kill())

  expect_equal(rs$get_state(), "idle")

  ## Start a command
  rs$call(function() 42)

  ## Get result
  res <- read_next(rs)
  expect_equal(res$result, 42)
  expect_null(res$error)
  expect_equal(rs$get_state(), "idle")

  ## Run another command, with arguments
  rs$call(function(x, y)  x + y, list(x = 42, y = 42))

  ## Get result
  res <- read_next(rs)
  expect_equal(res$result, 84)
  expect_equal(rs$get_state(), "idle")

  ## Close
  rs$close()
  expect_equal(rs$get_state(), "finished")
  expect_false(rs$is_alive())
})

test_that("run", {
  rs <- r_session$new()
  on.exit(rs$kill())

  expect_equal(rs$run_with_output(function() 42)$result, 42)
  expect_equal(rs$run_with_output(function() 42)$result, 42)
  expect_equal(
    rs$run_with_output(function(x, y) { x + y },
                       list(x = 42, y = 42))$result,
    84)

  ## Finish
  rs$close()
  expect_equal(rs$get_state(), "finished")
  expect_false(rs$is_alive())
})


test_that("get stdout/stderr from file", {
  rs <- r_session$new()
  on.exit(rs$kill())

  for (i in 1:10) {
    res <- rs$run_with_output(function() {
      cat("foo\n"); message("bar"); 42 })
    expect_equal(
      res[c("result", "stdout", "stderr")],
      list(result = 42, stdout = "foo\n", stderr = "bar\n"))

    res <- rs$run_with_output(function() {
      cat("bar\n"); message("foo"); 43 })
    expect_equal(
      res[c("result", "stdout", "stderr")],
      list(result = 43, stdout = "bar\n", stderr = "foo\n"))
  }
})

test_that("stdout/stderr from pipe", {
  opt <- r_session_options(stdout = "|", stderr = "|")
  rs <- r_session$new(opt)
  on.exit(rs$kill())

  res <- rs$run_with_output(function() {
    cat("foo\n"); message("bar"); 42 })
  expect_equal(
    res[c("result", "stdout", "stderr")],
    list(result = 42, stdout = NULL, stderr = NULL))

  res <- rs$run_with_output(function() {
    cat("bar\n"); message("foo"); 43 })
  expect_equal(
    res[c("result", "stdout", "stderr")],
    list(result = 43, stdout = NULL, stderr = NULL))

  rs$close()
  expect_equal(rs$read_all_output_lines(), c("foo", "bar"))
  expect_equal(rs$read_all_error_lines(), c("bar", "foo"))
})

test_that("interrupt", {
  rs <- r_session$new()
  on.exit(rs$kill())

  rs$call(function() Sys.sleep(5))
  Sys.sleep(0.5)
  rs$interrupt()
  rs$poll_process(1000)
  res <- rs$read()
  expect_s3_class(res$error, "interrupt")
})

test_that("messages", {
  f <- function() {
    x <- structure(
      list(code = 301, message = "ahoj"),
      class = c("callr_message", "condition"))
    signalCondition(x)
    22
  }

  msg <- NULL
  cb <- function(x) msg <<- x

  rs <- r_session$new()
  on.exit(rs$kill())

  expect_silent(res <- rs$run_with_output(f, message_callback = cb))
  expect_equal(res$result, 22)
  expect_equal(msg, list(code = 301, message = "ahoj"))
})

test_that("messages with R objects", {
  obj <- list(a = 1, b = 2)
  f <- function(obj) {
    msg <- paste0(
      "base64::",
      base64enc::base64encode(serialize(obj, NULL, ascii = TRUE)))
    x <- structure(
      list(code = 301, message = msg),
      class = c("callr_message", "condition"))
    signalCondition(x)
    22
  }

  msg <- NULL
  cb <- function(x) msg <<- x

  rs <- r_session$new()
  on.exit(rs$kill())

  expect_silent(res <- rs$run_with_output(f, args = list(obj),
                                          message_callback = cb))
  expect_equal(res$result, 22)
  expect_equal(msg, list(code = 301, message = obj))

  rs$call(f, args = list(obj))
  rs$poll_process(2000)
  expect_equal(rs$read(), list(code = 301, message = obj))
  rs$poll_process(2000)
  expect_equal(rs$read()$result, 22)
})

test_that("run throws", {
  rs <- r_session$new()
  on.exit(rs$kill())
  expect_error(rs$run(function() stop("foobar")), "foobar")
})

test_that("exit", {
  rs <- r_session$new()
  on.exit(rs$kill(), add = TRUE)
  res <- rs$run(function() q())

  deadline <- Sys.time() + 3
  while (rs$is_alive() && Sys.time() < deadline) Sys.sleep(0.05)
  expect_true(Sys.time() < deadline)

  expect_null(res)
  expect_false(rs$is_alive())
  expect_equal(rs$get_state(), "finished")

  rs <- r_session$new()
  on.exit(rs$kill(), add = TRUE)
  res <- rs$run_with_output(function() { cat("o\n"); message("e"); q() })
  expect_null(res$result)
  expect_equal(res$stdout, "o\n")
  expect_equal(res$stderr, "e\n")
  expect_false(rs$is_alive())
  expect_equal(rs$get_state(), "finished")
})

test_that("closing the process channel", {
  rs <- r_session$new()
  on.exit(rs$kill(), add = TRUE)

  err <- tryCatch(
    rs$run(function() close(processx::conn_create_fd(3))),
    error = function(e) e)
  expect_true(
    grepl("crashed with exit code", conditionMessage(err)) ||
    grepl("R session closed the process connection", conditionMessage(err)))
  expect_false(rs$is_alive())
  expect_equal(rs$get_state(), "finished")

  rs <- r_session$new()
  on.exit(rs$kill(), add = TRUE)
  res <- rs$run_with_output(function() {
    cat("o\n"); message("e");
    close(processx::conn_create_fd(3))
  })
  expect_null(res$result)
  expect_s3_class(res$error, "error")
  expect_equal(res$stdout, "o\n")
  expect_equal(res$stderr, "e\n")
  expect_false(rs$is_alive())
  expect_equal(rs$get_state(), "finished")
})

test_that("crash", {
  rs <- r_session$new()
  on.exit(rs$kill(), add = TRUE)
  err <- tryCatch(
    rs$run(function() get("crash", asNamespace("callr"))()),
    error = function(e) e)
  expect_true(
    grepl("crashed with exit code", conditionMessage(err)) ||
    grepl("R session closed the process connection", conditionMessage(err)))
  expect_false(rs$is_alive())
  expect_equal(rs$get_state(), "finished")

  rs <- r_session$new()
  on.exit(rs$kill(), add = TRUE)
  res <- rs$run_with_output(function() {
    cat("o\n"); message("e");
    get("crash", asNamespace("callr"))()
  })
  expect_null(res$result)
  expect_s3_class(res$error, "error")
  expect_equal(res$stdout, "o\n")
  expect_equal(substr(res$stderr, 1, 2), "e\n")
  expect_false(rs$is_alive())
  expect_equal(rs$get_state(), "finished")
})
