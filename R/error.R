
#' Create an error object
#'
#' There are two kinds of errors, both have class `callr_error`:
#' 1. the first one is thrown after a timeout: `callr_timeout_error`.
#' 2. the second one is thrown after an R error (in the other session):
#'    `callr_status_error`.
#'
#' @param out The object returned by [run()].
#' @param msg An extra message to add to the error message.
#' @keywords internal

make_error <- function(out, msg = NULL) {
  error_class <- c(
    if (out$timeout) "callr_timeout_error" else "callr_status_error",
    "callr_error", "error", "condition"
  )
  error_msg <- paste0(
    if (out$timeout) "callr timed out" else "callr failed",
    if (!is.null(msg)) paste0(", ", msg) else if (!out$timeout) ":"
  )
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
