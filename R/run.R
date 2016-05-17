
run_r <- function(bin, args, libpath, repos, stdout, stderr, show,
                  callback, system_profile, user_profile, env) {

  ## Temporary profile
  profile <- make_profile(repos)
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
    safe_system(bin, args = args, callback = real_callback)
  )

  if (!is.null(stdout)) cat(out$stdout, file = stdout)
  if (!is.null(stderr)) cat(out$stderr, file = stderr)

  out
}

make_profile <- function(repos) {
  profile <- tempfile()
  cat("options(repos=", deparse(repos), ")\n", sep = "", file = profile)
  profile
}
