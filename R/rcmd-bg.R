
#' Run an R CMD command in the background
#'
#' The child process is started in the background, and the function
#' return immediately.
#'
#' TODO: this function is currently broken and untested.
#'
#' @inheritParams rcmd
#' @return It returns a [processx::process] object.
#'
#' @family R CMD commands
#' @export
#' @importFrom processx process

rcmd_bg <- function(cmd, cmdargs = character(), libpath = .libPaths(),
                    stdout = "|", stderr = "|", repos = getOption("repos"),
                    system_profile = FALSE, user_profile = FALSE,
                    env = character(), wd = ".") {

  check_my_args()

  bin_args <- get_bin_and_args(cmd, cmdargs)

  ## Temporary wd
  oldwd <- setwd(wd)
  on.exit(setwd(oldwd), add = TRUE)

  ## Temporary profile
  profile <- make_profile(repos)
  on.exit(try(unlink(profile), silent = TRUE), add = TRUE)

  ## Temporary library path
  lib <- paste(libpath, collapse = .Platform$path.sep)

  ## Workaround, R ignores "", need to set to non-existant file
  if (lib == "") lib <- tempfile()

  if (is.na(env["R_LIBS"])) env["R_LIBS"] <- lib
  if (is.na(env["R_LIBS_USER"])) env["R_LIBS_USER"] <- lib
  if (is.na(env["R_LIBS_SITE"])) env["R_LIBS_SITE"] <- lib
  if (!system_profile) env["R_PROFILE"] <- profile
  if (!user_profile) env["R_PROFILE_USER"] <- profile

  with_envvar(
    env,
    process$new(bin_args$rbin, bin_args$cmdargs, stdout = stdout,
                stderr = stderr)
  )
}

#' @export

rcmd_bg_safe <- function(cmd, cmdargs = character(), libpath = .libPaths(),
                         stdout = "|", stderr = "|",
                         repos = c(getOption("repos"),
                           c(CRAN = "https://cran.rstudio.com")),
                         system_profile = FALSE, user_profile = FALSE,
                         env = rcmd_safe_env(), ...) {

  rcmd_bg(cmd, cmdargs = cmdargs, libpath = libpath, stdout = stdout,
          stderr = stderr, repos = repos, system_profile = system_profile,
          user_profile = user_profile, env = env, ...)
}
