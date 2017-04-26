
#' External R CMD Process
#'
#' @section Usage:
#' TODO
#'
#' @section Arguments:
#' TODO
#'
#' @section Details:
#' TODO
#'
#' @name rcmd_process
NULL

#' @importFrom R6 R6Class
#' @export

rcmd_process <- R6Class(
  "rcmd_process",
  inherit = processx::process,
  public = list(
    initialize = function(..., .options = NULL)
      rcmdp_init(self, private, super, ..., .options = .options)
  ),
  private = list(
    options = NULL
  )
)

rcmdp_init <- function(self, private, super, ..., .options) {

  options <- list(...)

  if (length(options) && length(.options)) {
    stop(sQuote(".options"), " must be either ", sQuote("NULL"),
         " or must contain all arguments")
  }

  if (length(.options)) options <- .options

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
