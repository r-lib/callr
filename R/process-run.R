#' Run external command, and wait until finishes
#'
#' `run` provides an interface similar to [base::system()] and
#' [base::system2()], but based on the [process] class. This allows some
#' extra features, see below.
#'
#' `run` supports
#' * Specifying a timeout for the command. If the specified time has
#'   passed, and the process is still running, it will be killed
#'   (with all its child processes).
#' * Calling a callback function for each line or each chunk of the
#'   standard output and/or error. A chunk may contain multiple lines, and
#'   can be as short as a single character.
#'
#' @section Callbacks:
#'
#' Some notes about the callback functions. The first argument of a
#' callback function is a character scalar (length 1 character), a single
#' output or error line. The second argument is always the [process]
#' object. You can manipulate this object, for example you can call
#' `$kill()` on it to terminate it, as a response to a message on the
#' standard output or error.
#'
#' @param command Character scalar, the command to run. It will be
#'   escaped via [base::shQuote].
#' @param args Character vector, arguments to the command. They will be
#'   escaped via [base::shQuote].
#' @param error_on_status Whether to throw an error if the command returns
#'   with a non-zero status, or it is interrupted. The error clases are
#'   `system_command_status_error` and `system_command_timeout_error`,
#'   respectively, and both errors have class `system_command_error` as
#'   well.
#' @param echo_cmd Whether to print the command to run to the screen.
#' @param echo Whether to print the standard output and error
#'   to the screen. Note that the order of the standard output and error
#'   lines are not necessarily correct, as standard output is typically
#'   buffered.
#' @param spinner Whether to show a reassusing spinner while the process
#'   is running.
#' @param timeout Timeout for the process, in seconds, or as a `difftime`
#'   object. If it is not finished before this, it will be killed.
#' @param stdout_line_callback `NULL`, or a function to call for every
#'   line of the standard output. See `stdout_callback` and also more
#'   below.
#' @param stdout_callback `NULL`, or a function to call for every chunk
#'   of the standard output. A chunk can be as small as a single character.
#'   At most one of `stdout_line_callback` and `stdout_callback` can be
#'   non-`NULL`.
#' @param stderr_line_callback `NULL`, or a function to call for every
#'   line of the standard error. See `stderr_callback` and also more
#'   below.
#' @param stderr_callback `NULL`, or a function to call for every chunk
#'   of the standard error. A chunk can be as small as a single character.
#'   At most one of `stderr_line_callback` and `stderr_callback` can be
#'   non-`NULL`.
#' @param windows_verbatim_args Whether to omit the escaping of the
#'   command and the arguments on windows. Ignored on other platforms.
#' @param windows_hide_window Whether to hide the window of the
#'   application on windows. Ignored on other platforms.
#' @param encoding The encoding to assume for `stdout` and
#'   `stderr`. By default the encoding of the current locale is
#'   used. Note that `callr` always reencodes the output of
#'   both streams in UTF-8 currently.
#' @return A list with components:
#'   * status The exit status of the process. If this is `NA`, then the
#'     process was killed and had no exit status.
#'   * stdout The standard output of the command, in a character scalar.
#'   * stderr The standard error of the command, in a character scalar.
#'   * timeout Whether the process was killed because of a timeout.
#'
#' @export
#' @examples
#' ## Different examples for Unix and Windows
#' \dontrun{
#' if (.Platform$OS.type == "unix") {
#'   run("ls")
#'   system.time(run("sleep", "10", timeout = 1,
#'     error_on_status = FALSE))
#'   system.time(
#'     run(
#'       "sh", c("-c", "for i in 1 2 3 4 5; do echo $i; sleep 1; done"),
#'       timeout = 2, error_on_status = FALSE
#'     )
#'   )
#' } else {
#'   run("ping", c("-n", "1", "127.0.0.1"))
#'   run("ping", c("-n", "6", "127.0.0.1"), timeout = 1,
#'     error_on_status = FALSE)
#' }
#' }

run <- function(
  command = NULL, args = character(),
  error_on_status = TRUE, echo_cmd = FALSE, echo = FALSE, spinner = FALSE,
  timeout = Inf, stdout_line_callback = NULL, stdout_callback = NULL,
  stderr_line_callback = NULL, stderr_callback = NULL,
  windows_verbatim_args = FALSE, windows_hide_window = FALSE,
  encoding = "") {

  assert_that(is_flag(error_on_status))
  assert_that(is_time_interval(timeout))
  assert_that(is_flag(spinner))
  assert_that(is.null(stdout_line_callback) ||
              is.function(stdout_line_callback))
  assert_that(is.null(stderr_line_callback) ||
              is.function(stderr_line_callback))
  assert_that(is.null(stdout_callback) || is.function(stdout_callback))
  assert_that(is.null(stderr_callback) || is.function(stderr_callback))
  ## The rest is checked by process$new()
  "!DEBUG run() Checked arguments"

  if (!interactive()) spinner <- FALSE

  ## Run the process
  pr <- process$new(
    command, args, echo_cmd = echo_cmd,
    windows_verbatim_args = windows_verbatim_args,
    windows_hide_window = windows_hide_window,
    stdout = "|", stderr = "|", encoding = encoding
  )
  "#!DEBUG run() Started the process: `pr$get_pid()`"

  ## If echo, then we need to create our own callbacks.
  ## These are merged to user callbacks if there are any.
  if (echo) {
    stdout_callback <- echo_callback(stdout_callback, "stdout")
    stderr_callback <- echo_callback(stderr_callback, "stderr")
  }

  ## Make the process interruptible, and kill it on interrupt
  runcall <- sys.call()
  res <- tryCatch(
    run_manage(pr, timeout, spinner, stdout_line_callback,
               stdout_callback, stderr_line_callback,
               stderr_callback),
    interrupt = function(e) {
      tryCatch(pr$kill(), error = function(e) NULL)
      "!DEBUG run() process `pr$get_pid()` killed on interrupt"
      stop(make_condition(
        list(interrupt = TRUE),
        runcall
      ))
    }
  )

  if (error_on_status && (is.na(res$status) || res$status != 0)) {
    "!DEBUG run() error on status `res$status` for process `pr$get_pid()`"
    stop(make_condition(res, call = sys.call()))
  }

  res
}

#' @importFrom crayon red

echo_callback <- function(user_callback, type) {
  force(user_callback)
  force(type)
  function(x, ...) {
    if (type == "stderr") x <- red(x)
    cat(x, sep = "")
    if (!is.null(user_callback)) user_callback(x, ...)
  }
}

run_manage <- function(proc, timeout, spinner, stdout_line_callback,
                       stdout_callback, stderr_line_callback,
                       stderr_callback) {

  timeout <- as.difftime(timeout, units = "secs")
  start_time <- proc$get_start_time()

  stdout <- ""
  stderr <- ""

  pushback_out <- ""
  pushback_err <- ""

  do_output <- function() {

    newout <- proc$read_output(2000)
    if (length(newout) && nzchar(newout)) {
      if (!is.null(stdout_callback)) stdout_callback(newout, proc)
      stdout <<- paste0(stdout, newout)
      if (!is.null(stdout_line_callback)) {
        newout <- paste0(pushback_out, newout)
        pushback_out <<- ""
        lines <- strsplit(newout, "\r?\n")[[1]]
        if (last_char(newout) != "\n") {
          pushback_out <<- tail(lines, 1)
          lines <- head(lines, -1)
        }
        lapply(lines, function(x) stdout_line_callback(x, proc))
      }
    }

    newerr <- proc$read_error(2000)
    if (length(newerr) && nzchar(newerr)) {
      stderr <<- paste0(stderr, newerr)
      if (!is.null(stderr_callback)) stderr_callback(newerr, proc)
      if (!is.null(stderr_line_callback)) {
        newerr <- paste0(pushback_err, newerr)
        pushback_err <<- ""
        lines <- strsplit(newerr, "\r?\n")[[1]]
        if (last_char(newerr) != "\n") {
          pushback_err <<- tail(lines, 1)
          lines <- head(lines, -1)
        }
        lapply(lines, function(x) stderr_line_callback(x, proc))
      }
    }
  }

  spin <- (function() {
    state <- 1L
    phases <- c("-", "\\", "|", "-")
    function() {
      cat("\r", phases[state], "\r", sep = "")
      state <<- (state + 1) %% length(phases) + 1L
    }
  })()

  timeout_happened <- FALSE

  while (proc$is_alive()) {
    ## Timeout? Maybe finished by now...
    if (!is.null(timeout) && Sys.time() - start_time > timeout) {
      if (proc$kill()) timeout_happened <- TRUE
      "!DEBUG Timeout killed run() process `proc$get_pid()`"
      break
    }

    ## Otherwise just poll for 200ms, or less if a timeout is sooner.
    ## We cannot poll until the end, even if there is not spinner,
    ## because RStudio does not send a SIGINT to the R process,
    ## so interruption does not work.
    if (!is.null(timeout) && timeout < Inf) {
      remains <- timeout - (Sys.time() - start_time)
      remains <- max(0, as.integer(as.numeric(remains) * 1000))
      if (spinner) remains <- min(remains, 200)
    } else {
      remains <- 200
    }
    "!DEBUG run is polling for `remains` ms, process `proc$get_pid()`"
    polled <- proc$poll_io(remains)

    ## If output/error, then collect it
    if (any(polled == "ready")) do_output()

    if (spinner) spin()
  }

  ## Needed to get the exit status
  "!DEBUG run() waiting to get exit status, process `proc$get_pid()`"
  proc$wait()

  ## We might still have output
  "!DEBUG run() reading leftover output / error, process `proc$get_pid()`"
  while (proc$is_incomplete_output() || proc$is_incomplete_error()) {
    proc$poll_io(-1)
    do_output()
  }

  if (spinner) cat("\r \r")

  list(
    status = proc$get_exit_status(),
    stdout = stdout,
    stderr = stderr,
    timeout = timeout_happened
  )
}

make_condition <- function(result, call) {

  if (isTRUE(result$interrupt)) {
    structure(
      list(
        message = "System command interrupted",
        stderr = NULL,
        call = call
      ),
      class = c("system_command_interrupt", "condition")
    )

  } else if (isTRUE(result$timeout)) {
    structure(
      list(
        message = "System command timeout",
        stderr = result$stderr,
        call = call
      ),
      class = c("system_command_timeout_error", "system_command_error",
        "error", "condition")
    )

  } else {
    structure(
      list(
        message = "System command error",
        stderr = result$stderr,
        call = call
      ),
      class = c("system_command_status_error", "system_command_error",
        "error", "condition")
    )
  }
}
