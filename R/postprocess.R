
post_process_run <- function(out, options) {

  if (!is.null(options$stdout)) cat(out$stdout, file = options$stdout)

  ## If the same file is selected for stdout and stderr,
  ## then we need to append here
  if (!is.null(options$stderr)) {
    nstderr <- normalizePath(options$stderr, mustWork = FALSE)
    append <- ! is.null(options$stdout) &&
      normalizePath(options$stdout) == nstderr
    cat(out$stderr, file = options$stderr, append = append)
  }

  if (options$fail_on_status && out$status != 0) {
    stop(make_error(out))
  }

  out
}
