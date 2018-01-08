
run_r <- function(bin, args, libpath, repos, stdout, stderr, echo, show,
                  callback, system_profile, user_profile, env, wd,
                  fail_on_status) {

  ## Temporary wd
  oldwd <- setwd(wd)
  on.exit(setwd(oldwd), add = TRUE)

  ## Temporary profile
  profile <- make_profile(repos, libpath)
  on.exit(try(unlink(profile), silent = TRUE), add = TRUE)

  ## Temporary library path
  lib <- paste(libpath, collapse = .Platform$path.sep)

  ## Workaround, R ignores "", need to set to non-existant file
  if (lib == "") lib <- tempfile()

  real_callback <- if (show) {
    if (is.null(callback)) {
      function(x) cat(x, sep = "", "\n")
    } else {
      function(x) { cat(x, sep = "", "\n"); callback(x) }
    }
  } else {
    callback
  }

  if (is.na(env["R_LIBS"])) env["R_LIBS"] <- lib
  if (is.na(env["R_LIBS_USER"])) env["R_LIBS_USER"] <- lib
  if (is.na(env["R_LIBS_SITE"])) env["R_LIBS_SITE"] <- lib
  if (!system_profile) env["R_PROFILE"] <- profile
  if (!user_profile) env["R_PROFILE_USER"] <- profile

  out <- with_envvar(
    env,
    safe_system(bin, args = args, callback = real_callback, echo = echo)
  )

  if (!is.null(stdout)) cat(out$stdout, file = stdout)

  ## If the same file is selected for stdout and stderr,
  ## then we need to append here
  if (!is.null(stderr)) {
    append <- ! is.null(stdout) &&
      normalizePath(stdout) == normalizePath(stderr, mustWork = FALSE)
    cat(out$stderr, file = stderr, append = append)
  }

  if (fail_on_status && out$status != 0) {
    myerr <- structure(
      list(
        message = paste("Command failed:\n", out$command, "\n", out$stderr),
        status = out$status,
        stdout = out$stdout,
        stderr = out$stderr
      ),
      class = c("rcmdError", "error", "condition")
    )
    stop(myerr)
  }

  out
}

## We set the lib path here as well, in case it was set in .Renviron, etc.
## The supplied libpath should take precedence over .Renviron.

make_profile <- function(repos, libpath) {
  profile <- tempfile()
  cat("options(repos=", deparse(repos), ")\n", sep = "", file = profile)
  cat(".libPaths(", deparse(libpath), ")\n", sep = "", file = profile,
      append = TRUE)
  profile
}
