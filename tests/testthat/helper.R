
try_silently <- function(expr) {
  tryCatch(
    expr,
    error = function(x) "error",
    warning = function(x) "warning",
    message = function(x) "message"
  )
}

read_next <- function(x, timeout = 3000) {
  pr <- x$poll_process(timeout)
  if (any(pr == "ready")) {
    x$read()
  } else {
    stop("R session is not ready, timed out...")
  }
}
