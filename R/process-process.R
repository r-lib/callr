
#' External process
#'
#' Managing external processes from R is not trivial, and this
#' class aims to help with this deficiency. It is essentially a small
#' wrapper around the `system` base R function, to return the process
#' id of the started process, and set its standard output and error
#' streams. The process id is then used to manage the process.
#'
#' @section Usage:
#' ```
#' p <- process$new(command = NULL, args,
#'                  stdout = NULL, stderr = NULL, cleanup = TRUE,
#'                  echo_cmd = FALSE, supervise = FALSE,
#'                  windows_verbatim_args = FALSE,
#'                  windows_hide_window = FALSE,
#'                  encoding = "", post_process = NULL)
#'
#' p$is_alive()
#' p$signal(signal)
#' p$kill(grace = 0.1)
#' p$wait(timeout = -1)
#' p$get_pid()
#' p$get_exit_status()
#' p$restart()
#' p$get_start_time()
#'
#' p$read_output(n = -1)
#' p$read_error(n = -1)
#' p$read_output_lines(n = -1)
#' p$read_error_lines(n = -1)
#' p$get_output_connection()
#' p$get_error_connection()
#' p$is_incomplete_output()
#' p$is_incomplete_error()
#' p$read_all_output()
#' p$read_all_error()
#' p$read_all_output_lines()
#' p$read_all_error_lines()
#'
#' p$poll_io(timeout)
#'
#' p$get_result()
#'
#' print(p)
#' ```
#'
#' @section Arguments:
#' * `p`: `process` object.
#' * `command`: Character scalar, the command to run.
#'     Note that this argument is not passed to a shell, so no
#'     tilde-expansion or variable substitution is performed on it.
#'     It should not be quoted with [base::shQuote()]. See
#'     [base::normalizePath()] for tilde-expansion.
#' * `args`: Character vector, arguments to the command. They will be
#'     used as is, without a shell. They don't need to be escaped.
#' * `stdout`: What to do with the standard output. Possible values:
#'     `NULL`: discard it; a string, redirect it to this file;
#'     `"|"`: create a connection for it.
#' * `stderr`: What to do with the standard error. Possible values:
#'     `NULL`: discard it; a string, redirect it to this file;
#'     `"|"`: create a connection for it.
#' * `cleanup`: Whether to kill the process (and its children)
#'     if the `process` object is garbage collected.
#' * `echo_cmd`: Whether to print the command to the screen before
#'     running it.
#' * `supervise`: Whether to register the process with a supervisor.
#'     If `TRUE`, the supervisor will ensure that the process is
#'     killed when the R process exits.
#' * `windows_verbatim_args`: Whether to omit quoting the arguments
#'     on Windows. It is ignored on other platforms.
#' * `windows_hide_window`: Whether to hide the application's window
#'     on Windows. It is ignored on other platforms.
#' * `signal`: An integer scalar, the id of the signal to send to
#'     the process. See [tools::pskill()] for the list of signals.
#' * `grace`: Currently not used.
#' * `timeout`: Timeout in milliseconds, for the wait or the I/O
#'     polling.
#' * `n`: Number of characters or lines to read.
#' * `encoding`: The encoding to assume for `stdout` and
#'     `stderr`. By default the encoding of the current locale is
#'     used. Note that `callr` always reencodes the output of
#'     both streams in UTF-8 currently. If you want to read them
#'     without any conversion, on all platforms, specify `"UTF-8"` as
#'     encoding.
#' * `post_process`: An optional function to run when the process has
#'     finished. Currently it only runs if `$get_result()` is called.
#'     It is only run once.
#'
#' @section Details:
#' `$new()` starts a new process in the background, and then returns
#' immediately.
#'
#' `$is_alive()` checks if the process is alive. Returns a logical
#' scalar.
#'
#' `$signal()` sends a signal to the process. On Windows only the
#' `SIGINT`, `SIGTERM` and `SIGKILL` signals are interpreted,
#' and the special 0 signal, The first three all kill the process. The 0
#' signal return `TRUE` if the process is alive, and `FALSE`
#' otherwise. On Unix all signals are supported that the OS supports, and
#' the 0 signal as well.
#'
#' `$kill()` kills the process. It also kills all of its child
#' processes, except if they have created a new process group (on Unix),
#' or job object (on Windows). It returns `TRUE` if the process
#' was killed, and `FALSE` if it was no killed (because it was
#' already finished/dead when `callr` tried to kill it).
#'
#' `$wait()` waits until the process finishes, or a timeout happens.
#' Note that if the process never finishes, and the timeout is infinite
#' (the default), then R will never regain control. It returns
#' the process itself, invisibly. In some rare cases, `$wait()` might take
#' a bit longer than specified to time out. This happens on Unix, when
#' another package overwrites the processx SIGCHLD signal handler, after the
#' processx process has started. One such package is parallel, if used
#' with fork clusters, e.g. through `parallel::mcparallel()`.
#'
#' `$get_pid()` returns the process id of the process.
#'
#' `$get_exit_status` returns the exit code of the process if it has
#' finished and `NULL` otherwise. On Unix, in some rare cases, the exit
#' status might be `NA`. This happens if another package (or R itself)
#' overwrites the processx SIGCHLD handler, after the processx process
#' has started. In these cases processx cannot determine the real exit
#' status of the process. One such package is parallel, if used with
#' fork clusters, e.g. through the `parallel::mcparallel()` function.
#'
#' `$restart()` restarts a process. It returns the process itself.
#'
#' `$get_start_time()` returns the time when the process was
#' started.
#'
#' `$is_supervised()` returns whether the process is being tracked by
#' supervisor process.
#'
#' `$supervise()` if passed `TRUE`, tells the supervisor to start
#' tracking the process. If `FALSE`, tells the supervisor to stop
#' tracking the process. Note that even if the supervisor is disabled for a
#' process, if it was started with `cleanup=TRUE`, the process will
#' still be killed when the object is garbage collected.
#'
#' `$read_output()` reads from the standard output connection of the
#' process. If the standard output connection was not requested, then
#' then it returns an error. It uses a non-blocking text connection. This
#' will work only if `stdout="|"` was used. Otherwise, it will throw an
#' error.
#'
#' `$read_error()` is similar to `$read_output`, but it reads
#' from the standard error stream.
#'
#' `$read_output_lines()` reads lines from standard output connection
#' of the process. If the standard output connection was not requested, then
#' then it returns an error. It uses a non-blocking text connection. This
#' will work only if `stdout="|"` was used. Otherwise, it will throw an
#' error.
#'
#' `$read_error_lines()` is similar to `$read_output_lines`, but
#' it reads from the standard error stream.
#'
#' `$has_output_connection()` returns `TRUE` if there is a connection
#' object for standard output; in other words, if `stdout="|"`. It returns
#' `FALSE` otherwise.
#'
#' `$has_error_connection()` returns `TRUE` if there is a connection
#' object for standard error; in other words, if `stderr="|"`. It returns
#' `FALSE` otherwise.
#'
#' `$get_output_connection()` returns a connection object, to the
#' standard output stream of the process.
#'
#' `$get_error_conneciton()` returns a connection object, to the
#' standard error stream of the process.
#'
#' `$is_incomplete_output()` return `FALSE` if the other end of
#' the standard output connection was closed (most probably because the
#' process exited). It return `TRUE` otherwise.
#'
#' `$is_incomplete_error()` return `FALSE` if the other end of
#' the standard error connection was closed (most probably because the
#' process exited). It return `TRUE` otherwise.
#'
#' `$read_all_output()` waits for all standard output from the process.
#' It does not return until the process has finished.
#' Note that this process involves waiting for the process to finish,
#' polling for I/O and potentically several `readLines()` calls.
#' It returns a character scalar. This will return content only if
#' `stdout="|"` was used. Otherwise, it will throw an error.
#'
#' `$read_all_error()` waits for all standard error from the process.
#' It does not return until the process has finished.
#' Note that this process involves waiting for the process to finish,
#' polling for I/O and potentically several `readLines()` calls.
#' It returns a character scalar. This will return content only if
#' `stderr="|"` was used. Otherwise, it will throw an error.
#'
#' `$read_all_output_lines()` waits for all standard output lines
#' from a process. It does not return until the process has finished.
#' Note that this process involves waiting for the process to finish,
#' polling for I/O and potentically several `readLines()` calls.
#' It returns a character vector. This will return content only if
#' `stdout="|"` was used. Otherwise, it will throw an error.
#'
#' `$read_all_error_lines()` waits for all standard error lines from
#' a process. It does not return until the process has finished.
#' Note that this process involves waiting for the process to finish,
#' polling for I/O and potentically several `readLines()` calls.
#' It returns a character vector. This will return content only if
#' `stderr="|"` was used. Otherwise, it will throw an error.
#'
#' `$get_output_file()` if the `stdout` argument was a filename,
#' this returns the absolute path to the file. If `stdout` was `"|"` or
#' `NULL`, this simply returns that value.
#'
#' `$get_error_file()` if the `stderr` argument was a filename,
#' this returns the absolute path to the file. If `stderr` was `"|"` or
#' `NULL`, this simply returns that value.
#'
#' `$poll_io()` polls the process's connections for I/O. See more in
#' the _Polling_ section, and see also the [poll()] function
#' to poll on multiple processes.
#'
#' `$get_result()` returns the result of the post processesing function.
#' It can only be called once the process has finished. If the process has
#' no post-processing function, then `NULL` is returned.
#'
#' `print(p)` or `p$print()` shows some information about the
#' process on the screen, whether it is running and it's process id, etc.
#'
#' @section Polling:
#' The `poll_io()` function polls the standard output and standard
#' error connections of a process, with a timeout. If there is output
#' in either of them, or they are closed (e.g. because the process exits)
#' `poll_io()` returns immediately.
#'
#' In addition to polling a single process, the [poll()] function
#' can poll the output of several processes, and returns as soon as any
#' of them has generated output (or exited).
#'
#' @importFrom R6 R6Class
#' @name process
#' @examples
#' # CRAN does not like long-running examples
#' \dontrun{
#' p <- process$new("sleep", "2")
#' p$is_alive()
#' p
#' p$kill()
#' p$is_alive()
#'
#' p$restart()
#' p$is_alive()
#' Sys.sleep(3)
#' p$is_alive()
#' }
#'
NULL

#' @export

process <- R6Class(
  "process",
  cloneable = FALSE,
  public = list(

    initialize = function(command = NULL, args = character(),
      stdout = NULL, stderr = NULL, cleanup = TRUE,
      echo_cmd = FALSE, supervise = FALSE, windows_verbatim_args = FALSE,
      windows_hide_window = FALSE, encoding = "", post_process = NULL)
      process_initialize(self, private, command, args,
                         stdout, stderr, cleanup, echo_cmd, supervise,
                         windows_verbatim_args, windows_hide_window,
                         encoding, post_process),

    kill = function(grace = 0.1)
      process_kill(self, private, grace),

    signal = function(signal)
      process_signal(self, private, signal),

    get_pid = function()
      process_get_pid(self, private),

    is_alive = function()
      process_is_alive(self, private),

    wait = function(timeout = -1)
      process_wait(self, private, timeout),

    get_exit_status = function()
      process_get_exit_status(self, private),

    restart = function()
      process_restart(self, private),

    print = function()
      process_print(self, private),

    get_start_time = function()
      process_get_start_time(self, private),

    is_supervised = function()
      process_is_supervised(self, private),

    supervise = function(status)
      process_supervise(self, private, status),

    ## Output

    read_output = function(n = -1)
      process_read_output(self, private, n),

    read_error = function(n = -1)
      process_read_error(self, private, n),

    read_output_lines = function(n = -1)
      process_read_output_lines(self, private, n),

    read_error_lines = function(n = -1)
      process_read_error_lines(self, private, n),

    is_incomplete_output = function()
      process_is_incompelete_output(self, private),

    is_incomplete_error = function()
      process_is_incompelete_error(self, private),

    has_output_connection = function()
      process_has_output_connection(self, private),

    has_error_connection = function()
      process_has_error_connection(self, private),

    get_output_connection = function()
      process_get_output_connection(self, private),

    get_error_connection = function()
      process_get_error_connection(self, private),

    read_all_output = function()
      process_read_all_output(self, private),

    read_all_error = function()
      process_read_all_error(self, private),

    read_all_output_lines = function()
      process_read_all_output_lines(self, private),

    read_all_error_lines = function()
      process_read_all_error_lines(self, private),

    get_output_file = function()
      process_get_output_file(self, private),

    get_error_file = function()
      process_get_error_file(self, private),

    poll_io = function(timeout)
      process_poll_io(self, private, timeout),

    get_result = function()
      process_get_result(self, private)
  ),

  private = list(

    command = NULL,       # Save 'command' argument here
    args = NULL,          # Save 'args' argument here
    cleanup = NULL,       # cleanup argument
    stdout = NULL,        # stdout argument or stream
    stderr = NULL,        # stderr argument or stream
    pstdout = NULL,       # the original stdout argument
    pstderr = NULL,       # the original stderr argument
    cleanfiles = NULL,    # which temp stdout/stderr file(s) to clean up
    starttime = NULL,     # timestamp of start
    echo_cmd = NULL,      # whether to echo the command
    windows_verbatim_args = NULL,
    windows_hide_window = NULL,

    status = NULL,        # C file handle
    exited = FALSE,       # Whether pid & exitcode was copied over here
    pid = NULL,           # pid, if finished, otherwise in status!
    exitcode = NULL,      # exit code, if finished, otherwise in status!

    supervised = FALSE,   # Whether process is tracked by supervisor

    stdout_pipe = NULL,
    stderr_pipe = NULL,

    encoding = "",

    post_process = NULL,
    post_process_result = NULL,
    post_process_done = FALSE,

    get_short_name = function()
      process_get_short_name(self, private)
  )
)

process_restart <- function(self, private) {

  "!DEBUG process_restart `private$get_short_name()`"

  ## Suicide if still alive
  if (self$is_alive()) self$kill()

  ## This makes sure that the finalizer does not modify `private`.
  ## Otherwise we get a race condition, beacause we are trying to
  ## set `private$exited`, `private$exitcode` and `private$pid` here,
  ## but the finalizer also sets them, and the finalizer runs async.
  ## So we set the tag of the external pointer to NULL here, which signals
  ## the finalizer not to set `private$*`.
  if (!is.null(private$status)) {
    .Call(c_callr__disconnect_process_handle, private$status);
  }

  ## Wipe out state, to be sure
  private$cleanfiles <- NULL
  private$status <- NULL
  private$exited <- FALSE
  private$pid <- NULL
  private$exitcode <- NULL
  private$stdout_pipe <- NULL
  private$stderr_pipe <- NULL

  process_initialize(
    self,
    private,
    private$command,
    private$args,
    private$pstdout,
    private$pstderr,
    private$cleanup,
    private$echo_cmd,
    private$supervised,
    private$windows_verbatim_args,
    private$windows_hide_window,
    private$encoding,
    private$post_process
  )

  invisible(self)
}

## See the C source code for a discussion about the implementation
## of these methods

process_wait <- function(self, private, timeout) {
  "!DEBUG process_wait `private$get_short_name()`"
  if (private$exited) {
    ## Nothing
  } else {
    .Call(c_callr_wait, private$status, as.integer(timeout))
  }
  invisible(self)
}

process_is_alive <- function(self, private) {
  "!DEBUG process_is_alive `private$get_short_name()`"
  if (private$exited) {
    FALSE
  } else {
    .Call(c_callr_is_alive, private$status)
  }
}

process_get_exit_status <- function(self, private) {
  "!DEBUG process_get_exit_status `private$get_short_name()`"
  if (private$exited) {
    private$exitcode
  } else {
    .Call(c_callr_get_exit_status, private$status)
  }
}

process_signal <- function(self, private, signal) {
  "!DEBUG process_signal `private$get_short_name()` `signal`"
  if (private$exited) {
    FALSE
  } else {
    .Call(c_callr_signal, private$status, as.integer(signal))
  }
}

process_kill <- function(self, private, grace) {
  "!DEBUG process_kill '`private$get_short_name()`', pid `self$get_pid()`"
  if (private$exited) {
    FALSE
  } else {
    .Call(c_callr_kill, private$status, as.numeric(grace))
  }
}

process_get_start_time <- function(self, private) {
  private$starttime
}

process_get_pid <- function(self, private) {
  if (private$exited) {
    private$pid
  } else {
    .Call(c_callr_get_pid, private$status)
  }
}

process_is_supervised <- function(self, private) {
  private$supervised
}

process_supervise <- function(self, private, status) {
  if (status && !self$is_supervised()) {
    supervisor_watch_pid(self$get_pid())
    private$supervised <- TRUE

  } else if (!status && self$is_supervised()) {
    supervisor_unwatch_pid(self$get_pid())
    private$supervised <- FALSE
  }
}

process_get_result <- function(self, private) {
  if (self$is_alive()) stop("Process is still alive")
  if (!private$post_process_done && is.function(private$post_process)) {
    private$post_process_result <- private$post_process()
    private$post_process_done <- TRUE
  }
  private$post_process_result
}
