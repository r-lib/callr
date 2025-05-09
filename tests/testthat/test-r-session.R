test_that("regular use", {
  rs <- r_session$new()
  on.exit(rs$kill())

  expect_equal(rs$get_state(), "idle")

  ## Clean session
  expect_identical(rs$run(function() ls(.GlobalEnv)), character())

  ## Start a command
  rs$call(function() 42)

  ## Get result
  res <- read_next(rs)
  expect_equal(res$result, 42)
  expect_null(res$error)
  expect_equal(rs$get_state(), "idle")

  ## Run another command, with arguments
  rs$call(function(x, y) x + y, list(x = 42, y = 42))

  ## Get result
  res <- read_next(rs)
  expect_equal(res$result, 84)
  expect_equal(rs$get_state(), "idle")

  ## While session is idle the current call running time is NA
  rs$call(function() 42)
  expect_equal(
    is.na(rs$get_running_time()),
    c(total = FALSE, current = FALSE)
  )
  res <- read_next(rs)
  expect_equal(
    is.na(rs$get_running_time()),
    c(total = FALSE, current = TRUE)
  )
  expect_s3_class(rs$get_running_time(), "difftime")

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
    rs$run_with_output(
      function(x, y) {
        x + y
      },
      list(x = 42, y = 42)
    )$result,
    84
  )

  ## Finish
  rs$close()
  expect_equal(rs$get_state(), "finished")
  expect_false(rs$is_alive())
})


test_that("get stdout/stderr from file", {
  rs <- r_session$new()
  on.exit(rs$kill())

  eol <- function(x) {
    paste0(x, if (.Platform$OS.type == "windows") "\r\n" else "\n")
  }

  for (i in 1:10) {
    res <- rs$run_with_output(function() {
      cat("foo\n")
      message("bar")
      42
    })
    expect_equal(
      res[c("result", "stdout", "stderr")],
      list(result = 42, stdout = eol("foo"), stderr = eol("bar"))
    )

    res <- rs$run_with_output(function() {
      cat("bar\n")
      message("foo")
      43
    })
    expect_equal(
      res[c("result", "stdout", "stderr")],
      list(result = 43, stdout = eol("bar"), stderr = eol("foo"))
    )
  }
  rs$close()
})

test_that("stdout/stderr from pipe", {
  skip_on_cran()
  opt <- r_session_options(stdout = "|", stderr = "|")
  rs <- r_session$new(opt)
  on.exit(rs$kill())

  ## Sometimes a NULL slips through....  this is bug to be fixed
  rs$read_output_lines()

  res <- rs$run_with_output(function() {
    cat("foo\n")
    message("bar")
    42
  })
  expect_equal(
    res[c("result", "stdout", "stderr")],
    list(result = 42, stdout = NULL, stderr = NULL)
  )

  res <- rs$run_with_output(function() {
    cat("bar\n")
    message("foo")
    43
  })
  expect_equal(
    res[c("result", "stdout", "stderr")],
    list(result = 43, stdout = NULL, stderr = NULL)
  )

  processx::poll(list(rs$get_output_connection()), 1000)
  expect_equal(rs$read_output_lines(n = 1), "foo")
  processx::poll(list(rs$get_output_connection()), 1000)
  expect_equal(rs$read_output_lines(n = 1), "bar")
  processx::poll(list(rs$get_error_connection()), 1000)
  expect_equal(rs$read_error_lines(n = 1), "bar")
  processx::poll(list(rs$get_error_connection()), 1000)
  expect_equal(rs$read_error_lines(n = 1), "foo")
  rs$close()
})

test_that("interrupt", {
  rs <- r_session$new()
  on.exit(rs$kill())

  rs$call(function() Sys.sleep(5))
  Sys.sleep(0.5)
  rs$interrupt()
  rs$poll_process(1000)
  res <- rs$read()
  expect_s3_class(res$error, "callr_timeout_error")
  rs$close()
})

test_that("messages", {
  f <- function() {
    x <- structure(
      list(code = 301, message = "ahoj"),
      class = c("callr_message", "condition")
    )
    signalCondition(x)
    22
  }

  msg <- NULL
  cb <- function(x) msg <<- x

  rs <- r_session$new()
  on.exit(rs$kill())

  withCallingHandlers(
    expect_silent(res <- rs$run_with_output(f)),
    callr_message = function(e) msg <<- e
  )
  expect_equal(res$result, 22)
  expect_equal(
    msg,
    structure(
      list(code = 301, message = "ahoj", muffle = "callr_r_session_muffle"),
      class = c("callr_message", "condition")
    )
  )
  rs$close()
})

test_that("messages with R objects", {
  obj <- list(a = 1, b = 2)
  f <- function(obj) {
    x <- structure(
      c(list(code = 301), obj),
      class = c("foobar_class", "callr_message", "condition")
    )
    signalCondition(x)
    22
  }

  msg <- NULL
  cb <- function(x) msg <<- x

  rs <- r_session$new()
  on.exit(rs$kill())

  withCallingHandlers(
    expect_silent(res <- rs$run_with_output(f, args = list(obj))),
    foobar_class = function(e) msg <<- e
  )
  expect_equal(res$result, 22)
  exp <- exp2 <- structure(
    list(code = 301, a = 1, b = 2),
    class = c("foobar_class", "callr_message", "condition")
  )
  exp2$muffle <- "callr_r_session_muffle"
  expect_equal(msg, exp2)
  rs$call(f, args = list(obj))
  rs$poll_process(2000)
  expect_equal(
    rs$read(),
    structure(
      list(code = 301, message = exp),
      class = "callr_session_result"
    )
  )
  rs$poll_process(2000)
  expect_equal(rs$read()$result, 22)
  rs$close()
})

test_that("run throws", {
  rs <- r_session$new()
  on.exit(rs$kill())
  expect_snapshot(error = TRUE, rs$run(function() stop("foobar")))
  rs$close()
})

test_that("exit", {
  rs <- r_session$new()
  on.exit(rs$kill(), add = TRUE)
  err <- tryCatch(
    res <- rs$run(function() q()),
    error = function(x) x
  )

  deadline <- Sys.time() + 3
  while (rs$is_alive() && Sys.time() < deadline) Sys.sleep(0.05)
  expect_true(Sys.time() < deadline)

  expect_false(rs$is_alive())
  expect_equal(rs$get_state(), "finished")
  rs$close()
})

test_that("crash", {
  skip_on_cran()
  rs <- r_session$new()
  on.exit(rs$kill(), add = TRUE)
  err <- tryCatch(
    rs$run(function() get("crash", asNamespace("callr"))()),
    error = function(e) e
  )
  expect_true(
    grepl("crashed with exit code", conditionMessage(err)) ||
      grepl("R session closed the process connection", conditionMessage(err)) ||
      grepl(
        "Invalid (uninitialized or closed?) connection object",
        conditionMessage(err),
        fixed = TRUE
      )
  )
  expect_false(rs$is_alive())
  expect_equal(rs$get_state(), "finished")
  rs$close()

  rs <- r_session$new()
  on.exit(rs$kill(), add = TRUE)
  res <- rs$run_with_output(function() {
    cat("o\n")
    message("e")
    get("crash", asNamespace("callr"))()
  })
  expect_null(res$result)
  expect_s3_class(res$error, "error")

  ## This is a race, and sometimes we don't get the stdout/stderr
  ## on Windows
  if (os_platform() != "windows") expect_equal(res$stdout, "o\n")
  if (os_platform() != "windows") expect_equal(substr(res$stderr, 1, 2), "e\n")
  expect_false(rs$is_alive())
  expect_equal(rs$get_state(), "finished")
  rs$close()
})

test_that("custom load hook", {
  opts <- r_session_options(load_hook = quote(options(foobar = "baz")))
  rs <- r_session$new(opts)
  on.exit(rs$kill(), add = TRUE)

  res <- rs$run_with_output(function() getOption("foobar"))
  expect_null(res$error)
  expect_identical(res$result, "baz")
  expect_equal(res$stdout, "")
  expect_equal(res$stderr, "")
  rs$close()
})

test_that("traceback", {
  skip_if_not_installed("withr")
  withr::local_options(callr.traceback = TRUE)
  rs <- r_session$new()
  on.exit(rs$kill(), add = TRUE)

  do <- function() {
    f <- function() g()
    g <- function() stop("oops")
    f()
  }

  expect_snapshot(error = TRUE, rs$run(do))
  expect_output(tb <- rs$traceback(), "1: \"?f()\"?")
  expect_match(c(tb[[3]]), "f()", fixed = TRUE)
})

test_that("error in the load hook", {
  opts <- r_session_options(load_hook = quote(stop("oops")))
  expect_snapshot(error = TRUE, local({
    rs <- r_session$new(opts)
    on.exit(rs$kill(), add = TRUE)
  }))

  rs2 <- r_session$new(opts, wait = FALSE)
  on.exit(rs2$kill(), add = TRUE)

  processx::poll(list(rs2$get_poll_connection()), 3000)
  msg <- rs2$read()
  expect_true(msg$code %in% c(501L, 502L))
  expect_match(msg$stderr, "oops")
})

test_that("fds are not leaked", {
  skip_if_not_installed("ps")
  rs <- r_session$new()
  on.exit(rs$kill(), add = TRUE)

  old <- rs$run(function() ps::ps_num_fds())
  for (i in 1:10) {
    rs$run(function() 1 + 1)
  }
  new <- rs$run(function() ps::ps_num_fds())

  # Allow 2 new fds, just in case
  expect_true(new - old <= 2)
})
