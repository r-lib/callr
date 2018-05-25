
try_silently <- function(expr) {
  tryCatch(
    expr,
    error = function(x) "error",
    warning = function(x) "warning",
    message = function(x) "message"
  )
}

r_session_wait_or_kill <- function(x, state = "ready") {
  x$wait(3000)
  if (x$get_state() != state) {
    x$kill()
    stop("R session not ready...")
  }
}
