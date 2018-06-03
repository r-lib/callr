
#' External R Session
#'
#' A permanent R session that runs in the background. This is an R6 class
#' that extends the [processx::process] class.
#'
#' The process is started at the creation of the object, and then it can
#' be used to evaluate R function calls, one at a time.
#'
#' @section Usage:
#' ```
#' rs <- r_session$new(options)
#' rs$run(func, args = list())
#' rs$call(func, args = list())
#' rs$get_result()
#' rs$get_result_and_output()
#' rs$get_running_time()
#' rs$get_state()
#' rs$finish()
#' ```
#'
#' @section Arguments:
#' * `options`: A list of options created via [r_session_options()].
#' * `func`: Function object to call in the background R process.
#' *   Please read the notes for the similar argument of [r()]
#' * `args`: Arguments to pass to the function. Must be a list.
#'
#' @section Details:
#' `r_session$new()` creates a new R background process. It returns
#' immediately, i.e. before the process is actually ready to run. You may
#' call `poll_io()` to make sure it is ready.
#'
#' `rs$run()` is similar to [r()], but runs the function in the `rs` R
#' session. Note that if a timeout happens, the session and the background
#' computation is not terminated. You can call `rs$finish()` to terminate
#' the R process. There is currently no way to terminate the computation
#' without terminating the background R process.
#'
#' `rs$call()` starts running a function in the background R session, and
#' returns immediately. To check if the function is done, call the
#' `poll_io()` method. To get the result call the `get_result()`
#' method.
#'
#' `rs$get_result()` returns the result of the last `rs$call()`
#' computation. (Or the result of the last `rs$run()`, if it was
#' interrupted.) If there is no result to return, because the computation
#' has not finished yet, or some other reason, it throws an error.
#'
#' `rs$get_running_time()` returns the elapsed time since the R process
#' has started, and the elapsed time since the current computation has
#' started. The latter is NA if there is no active computation.
#'
#' `rs$get_state()` return the state of the R session. Possible values:
#' * `"starting"`: starting up,
#' * `"idle"`: ready to compute,
#' * `"busy"`: computing right now,
#' * `"ready"`: computation finished, result can be read out,
#' * `"finished"`: the R process has finished.
#' `rs$get_state()` automatically updates the state, i.e. it performs a
#' quick `poll_io()`, if needed.
#'
#' `r$finish()` terminates the current computation and the R process.
#' The session object will be in `"finished"` state after this.
#'
#' @name r_session
#' @examples
#' \dontrun{
#' opt <- r_session_options()
#' rs <- r_ression$new(opt)
#'
#' rs$run(function() 1 + 2)
#'
#' rs$call(function() Sys.sleep(1))
#' rs$get_state()
#' rs$poll_io(-1)
#'
#' rs$get_result()
#' }
NULL


#' @importFrom R6 R6Class
#' @export

r_session <- R6Class(
  "r_session",
  inherit = process,

  public = list(
    initialize = function(options = r_session_options())
      rs_init(self, private, super, options),
    run = function(func, args = list(), timeout = -1)
      rs_run(self, private, func, args, timeout),
    call = function(func, args = list())
      rs_call(self, private, func, args),
    get_result = function()
      rs_get_result(self, private),
    get_result_and_output = function()
      rs_get_result_and_output(self, private),
    get_running_time = function()
      rs_get_running_time(self, private),
    get_state = function()
      rs_get_state(self, private),
    finish = function(grace = 200)
      rs_finish(self, private, grace),
    finalize = function() {
      unlink(private$tmp_output_file)
      unlink(private$tmp_error_file)
      if ("finalize" %in% ls(super)) super$finalize()
    }
  ),

  private = list(
    options = NULL,
    state = NULL,
    started_at = NULL,
    fun_started_at = as.POSIXct(NA),
    pipe = NULL,

    tmp_output_file = NULL,
    tmp_error_file = NULL,

    func_file = NULL,
    res_file = NULL,

    wait_for_call = function(timeout = -1)
      rs__wait_for_call(self, private, timeout),
    update_state = function()
      rs__update_state(self, private),
    report_back = function(code, text = "")
      rs__report_back(self, private, code, text),
    write_for_sure = function(text)
      rs__write_for_sure(self, private, text)
  )
)

#' @importFrom processx conn_create_pipepair

rs_init <- function(self, private, super, options) {

  options$func <- options$func %||% function() { }
  options$args <- list()

  options <- convert_and_check_my_args(options)
  options <- setup_context(options)
  options <- setup_r_binary_and_args(options, script_file = FALSE)

  private$options <- options

  with_envvar(
    options$env,
    super$initialize(options$bin, options$real_cmdargs, stdin = "|",
                     stdout = options$stdout, stderr = options$stderr,
                     poll_connection = TRUE)
  )

  ## Make child report back when ready
  private$report_back(200, "ready to go")

  private$pipe <- self$get_poll_connection()

  private$started_at <- Sys.time()
  private$state <- "starting"

  invisible(self)
}

rs_run <- function(self, private, func, args, timeout) {
  self$call(func, args)
  poll(list(private$pipe), timeout)
  self$get_result_and_output()
}

rs_call <- function(self, private, func, args) {
  private$update_state()
  if (private$state != "idle") stop("R session busy")

  ## Save the function in a file
  private$options$func <- func
  private$options$args <- args
  private$options$func_file <- save_function_to_temp(private$options)
  private$options$result_file <- tempfile()
  private$options$tmp_files <-
    c(private$options$tmp_files, private$options$func_file,
      private$options$result_file)

  ## Maybe we need to redirect stdout / stderr
  re_stdout  <-  if (is.null(private$options$stdout)) {
    private$tmp_output_file <- tempfile()
  }
  re_stderr <- if (is.null(private$options$stderr)) {
    private$tmp_error_file <- tempfile()
  }

  ## Run an expr that loads it, in the child process, with error handlers
  expr <- make_vanilla_script_expr(private$options$func_file,
                                   private$options$result_file,
                                   private$options$error,
                                   re_stdout = re_stdout,
                                   re_stderr = re_stderr)
  cmd <- paste0(deparse(expr), "\n")

  ## Write this to stdin
  private$write_for_sure(cmd)

  ## Report back when done
  report_str <- paste0("DONE", basename(private$options$result_file))
  private$report_back(200, report_str)

  private$state <- "busy"
}

rs__wait_for_call <- function(self, private, timeout) {
  if (private$state %in% c("finished", "ready", "idle")) return()

  pr <- self$poll_io(timeout)
  if (pr[["process"]] == "ready") {
    if (private$state == "starting") {
      private$state <- "idle"
    } else {
      private$state <- "ready"
    }
    invisible(conn_read_lines(private$pipe, 1))
  } else {
    invisible()
  }
}

rs_get_result <- function(self, private) {
  rs_get_result_and_output(self, private)$result
}

rs_get_result_and_output <- function(self, private) {
  if (private$state != "ready") private$update_state()

  get_my_result <- function() {
    out <- if (!is.null(private$tmp_output_file) &&
               file.exists(private$tmp_output_file)) {
      read_all(private$tmp_output_file)
    }
    err <- if (!is.null(private$tmp_error_file) &&
               file.exists(private$tmp_error_file)) {
      read_all(private$tmp_error_file)
    }
    unlink(c(private$tmp_output_file, private$tmp_error_file))
    private$tmp_output_file <- private$tmp_error_file <- NULL
    outp <- list(
      status = 0,
      stdout = out %||% "",
      stderr = err %||% "",
      timeout = FALSE
    )
    private$state <- "idle"
    res <- get_result(outp, private$options)
    unlink(private$options$tmp_files, recursive = TRUE)
    private$options$tmp_files <- NULL

    list(result = res, output = out, error = err)
  }

  switch(
    private$state,
    "finished" = stop("R session already finished"),
    "idle" = stop("R session is idle"),
    "busy" = stop("R session still busy"),
    "starting" = stop("R session still starting"),
    "ready" = get_my_result()
  )
}

rs_get_running_time <- function(self, private) {
  now <- Sys.time()
  c(total = now - private$started_at,
    current = now - private$fun_started_at)
}

rs_get_state <- function(self, private) {
  private$update_state()
  private$state
}

rs_finish <- function(self, private, grace) {
  close(self$get_input_connection())
  self$poll_io(grace)
  self$kill()
  self$wait(1000)
  if (self$is_alive()) stop("Could not kill background R session")
  private$state <- "finished"
  private$fun_started_at <- as.POSIXct(NA)
}

#' @importFrom processx conn_read_lines

rs__update_state <- function(self, private) {
  private$wait_for_call(timeout = 0)
}

rs__report_back <- function(self, private, code, text) {
  cmd <- paste0(deparse(rs__status_expr(code, text, fd = 3)), "\n")
  private$write_for_sure(cmd)
}

rs__write_for_sure <- function(self, private, text) {
  while (1) {
    text <- self$write_input(text)
    if (!length(text)) break;
    Sys.sleep(.1)
  }
}

#' @importFrom processx conn_create_fd conn_write

rs__status_expr <- function(code, text = "", fd = 3) {
  substitute(
    {
      code_ <- code; fd_ <- fd; text_ <- text
      con <- processx::conn_create_fd(fd_, close = FALSE)
      data <- paste0(code_, " ", text_, "\n")
      while (1) {
        data <- processx::conn_write(con, data)
        if (!length(data)) break;
        Sys.sleep(.1)
      }
    },
    list(code = code, fd = fd, text = text)
  )
}

#' Create options for an [r_session] object
#'
#' @param  ... Options to override, named arguments.
#'
#' @export

r_session_options <- function(...) {
  update_options(r_session_options_default(), ...)
}

r_session_options_default <- function() {
  list(
    func = NULL,
    args = NULL,
    libpath = .libPaths(),
    repos = c(getOption("repos"), CRAN = "https://cloud.r-project.org"),
    stdout = NULL,
    stderr = NULL,
    error = getOption("callr.error", "error"),
    cmdargs = c("--no-site-file", "--slave",
      "--no-save", "--no-restore"),
    system_profile = FALSE,
    user_profile = FALSE,
    env = character(),
    supervise = FALSE
  )
}
