
#' External R CMD Process
#'
#' An `R CMD *` command that runs in the background. This is an R6 class
#' that extends the [process] class.
#'
#' @section Usage:
#' ```
#' rp <- rcmd_process$new(options)
#' ```
#'
#' @section Arguments:
#' * `options` A list of options created via [rcmd_process_options()].
#'
#' @section Details:
#' `rcmd_process$new` creates a new instance. Its `options` argument is
#' best created by the [r_process_options()] function.
#'
#' @name rcmd_process
#' @examples
#' \dontrun{
#' options <- rcmd_process_options(cmd = "config", cmdargs = "CC")
#' rp <- rcmd_process$new(options)
#' rp$wait()
#' rp$read_output_lines()
#' }
NULL

#' @importFrom R6 R6Class
#' @export

rcmd_process <- R6Class(
  "rcmd_process",
  inherit = process,
  public = list(
    initialize = function(options)
      rcmdp_init(self, private, super, options)
  ),
  private = list(
    options = NULL
  )
)

rcmdp_init <- function(self, private, super, options) {

  ## This contains the context that we set up in steps
  options <- convert_and_check_my_args(options)

  options <- setup_context(options)
  options <- setup_rcmd_binary_and_args(options)

  private$options <- options

  oldwd <- getwd()
  setwd(options$wd)
  on.exit(setwd(oldwd), add = TRUE)

  with_envvar(
    options$env,
    super$initialize(options$bin, options$real_cmdargs,
                     stdout = options$stdout, stderr = options$stderr)
  )

  invisible(self)
}
