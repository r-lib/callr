
#' External R Process
#'
#' An R process that runs in the background. This is an R6 class that
#' extends the [processx::process] class. The process starts in the
#' background, evaluates an R function call, and then quits.
#'
#' @section Usage:
#' ```
#' rp <- r_process$new(options)
#' rp$get_result()
#' ```
#'
#' See [process] for the inherited methods.
#'
#' @section Arguments:
#' * `options` A list of options created via [r_process_options()].
#'
#' @section Details:
#' `r_process$new` creates a new instance. Its `options` argument is best
#' created by the [r_process_options()] function.
#'
#' `rp$get_result()` returns the result, an R object, from a finished
#' background R process. If the process has not finished yet, it throws an
#' error. (You can use `rp$wait()` to wait for the process to finish,
#' optionally with a timeout.)
#'
#' @name r_process
#' @examples
#' \dontrun{
#' ## List all options and their default values:
#' r_process_options()
#'
#' ## Start an R process in the background, wait for it, get result
#' opts <- r_process_options(func = function() 1 + 1)
#' rp <- r_process$new(opts)
#' rp$wait()
#' rp$get_result()
#' }
NULL

#' @export

r_process <- R6::R6Class(
  "r_process",
  inherit = processx::process,
  public = list(
    initialize = function(options)
      rp_init(self, private, super, options),
    get_result = function()
      rp_get_result(self, private)
  ),
  private = list(
    options = NULL
  )
)

rp_init <- function(self, private, super, options) {

  ## This contains the context that we set up in steps
  options <- convert_and_check_my_args(options)

  options <- setup_script_files(options)
  options <- setup_context(options)
  options <- setup_r_binary_and_args(options)

  private$options <- options

  with_envvar(
    options$env,
    super$initialize(options$bin, options$real_cmdargs,
                     stdout = options$stdout, stderr = options$stderr,
                     poll_connection = options$poll_connection,
                     supervise = options$supervise)
  )

  invisible(self)
}

rp_get_result <- function(self, private) {
  if (self$is_alive()) stop("Still alive")

  ## This is artificial...
  out <- list(
    status = self$get_exit_status(),
    stdout = "",
    stderr = "",
    timeout = FALSE
  )

  get_result(out, private$options)
}
