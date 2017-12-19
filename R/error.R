
#' Create an error object
#'
#' There are two kinds of errors, both have class `callr_error`:
#' 1. the first one is thrown after a timeout: `callr_timeout_error`.
#' 2. the second one is thrown after an R error (in the other session):
#'    `callr_status_error`.
#'
#' @param out The object returned by [run()].
#' @keywords internal

make_error <- function(out) {
  error_class <- c(
    if (out$timeout) "callr_timeout_error" else "callr_status_error",
    "callr_error", "error", "condition"
  )
  error_msg <- if (out$timeout) "callr timed out" else "callr failed:"
  structure(
    list(
      message = paste(error_msg, out$stderr),
      status = out$status,
      stdout = out$stdout,
      stderr = out$stderr
    ),
    class = error_class
  )
}
