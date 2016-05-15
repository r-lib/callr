
r_eval_expert <- function(expr_file, extra) {

  ## Parameters
  extra$libpath <- extra$libpath %||% .libPaths()
  extra$repos   <- extra$repos   %||% getOption("repos")
  extra$stdout  <- extra$stdout  %||% NULL
  extra$stderr  <- extra$stderr  %||% NULL

  check_extra_args(extra)

  res <- tempfile()

  rscript <- make_vanilla_script(expr_file, res)
  on.exit(unlink(rscript), add = TRUE)

  rbin <- paste0(R.home("bin"), "/R")

  ## Temporary profile
  profile <- tempfile()
  cat("options(repos=", deparse(extra$repos), ")\n", sep = "", file = profile)
  on.exit(unlink(profile), add = TRUE)

  ## Temporary library path
  lib <- paste(extra$libpath, collapse = .Platform$path.sep)

  out <- with_envvar(
    c(R_LIBS = lib,
      R_LIBS_USER = lib,
      R_LIBS_SITE = lib,
      R_PROFILE_USER = profile),
    safe_system(rbin, args = c("--slave", "-f", rscript))
  )

  if (!is.null(extra$stdout)) cat(out$stdout, file = extra$stdout)
  if (!is.null(extra$stderr)) cat(out$stderr, file = extra$stderr)

  if (out$status != 0) stop("callr error: ", out$stderr)

  res
}

check_extra_args <- function(extra) {

  stopifnot(
    is.character(extra$libpath),
    is.character(extra$repos),
    is.null(extra$stdout) || is_string(extra$stdout),
    is.null(extra$stderr) || is_string(extra$stderr)
  )
}
