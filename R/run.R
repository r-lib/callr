
run_r <- function(bin, args, libpath, repos, stdout, stderr, show,
                  callback) {

  ## Temporary profile
  profile <- tempfile()
  cat("options(repos=", deparse(repos), ")\n", sep = "", file = profile)
  on.exit(unlink(profile), add = TRUE)

  ## Temporary library path
  lib <- paste(libpath, collapse = .Platform$path.sep)

  real_callback <- if (show) {
    if (is.null(callback)) {
      function(x) cat(x, sep = "", "\n")
    } else {
      function(x) { cat(x, sep = "", "\n"); callback(x) }
    }
  } else {
    callback
  }

  out <- with_envvar(
    c(R_LIBS = lib,
      R_LIBS_USER = lib,
      R_LIBS_SITE = lib,
      R_PROFILE_USER = profile),
    safe_system(bin, args = args, callback = real_callback)
  )

  if (!is.null(stdout)) cat(out$stdout, file = stdout)
  if (!is.null(stderr)) cat(out$stderr, file = stderr)

  out
}
