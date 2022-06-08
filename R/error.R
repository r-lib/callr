
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

new_callr_error <- function(out, msg = NULL) {
  error_msg <- paste0(
    if (out$timeout) "callr timed out" else "callr subprocess failed",
    if (!is.null(msg)) paste0(": ", msg) else if (!out$timeout) ":"
  )

  cond <- new_error(paste(error_msg))

  class(cond) <- c(
    if (out$timeout) "callr_timeout_error" else "callr_status_error",
    "callr_error",
    class(cond))

  cond$status <- out$status
  cond$stdout <- out$stdout
  cond$stderr <- out$stderr

  cond
}

callr_remote_error <- function(remerr) {
  if (inherits(remerr[[3]], "interrupt")) {
    err <- new_error("timeout in callr subprocess", call. = FALSE)
    class(err) <- c("callr_timeout_error", "callr_error", class(err))
    remerr[[3]]$message <- "interrupt"
  } else {
    err <- new_error("error in callr subprocess", call. = FALSE)
    class(err) <- c("callr_status_error", "callr_error", class(err))
  }

  err$parent_trace <- remerr[[2]]$trace
  throw(err, parent = remerr[[3]])
}

#' @export

format.callr_status_error <- function(x, trace = FALSE, class = FALSE,
                                      advice = !trace, ...) {
  class(x) <- setdiff(class(x), "callr_status_error")

  lines <- NextMethod(
    object = x,
    trace = FALSE,
    class = class,
    advice = FALSE,
    ...
  )

  lines <- c(
    lines,
    if (advice) err$format$advice(),
    if (trace && !is.null(x$trace)) {
      c(
        "---",
        "Backtrace:",
        err$format$trace(x$trace)
      )
    }
  )

  cond <- x
  while (trace && !is.null(cond$parent_trace)) {
    lines <- c(lines, c("---", "Subprocess backtrace:", format(cond$parent_trace)))
    cond <- cond$parent
  }

  lines
}

#' @export

print.callr_status_error <- function(x, trace = TRUE, class = TRUE,
                                     advice = !trace, ...) {
  writeLines(format(x, trace = trace, class = class, advice = advice, ...))
}
