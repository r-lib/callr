
post_process_run <- function(out, options) {

  if (options$fail_on_status && out$status != 0) {
    stop(make_error(out))
  }

  out
}
