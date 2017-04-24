
#' @importFrom R6 R6Class

r_process <- R6Class(
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

  private$options <- options

  super$initialize(options$bin, options$real_cmdargs,
                   stdout = options$stdout, stderr = options$stderr)
  
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
