
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
