
try_silently <- function(expr) {
  tryCatch(
    expr,
    error = function(x) "error",
    warning = function(x) "warning",
    message = function(x) "message"
  )
}

r_session_wait_or_kill <- function(x) {
  pr <- poll(list(x$get_poll_connection()), 3000)[[1]]
  if (pr != "ready") {
    x$kill()
    stop("R session not ready...")
  }
}
