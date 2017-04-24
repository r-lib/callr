
#' Run an R child process, with no configuration
#'
#' It tries to mimic a fresh R installation. In particular:
#' * No library path setting.
#' * No CRAN(-like) repository is set.
#' * The system and user profiles are not run.
#'
#' @param ... Additional arguments are passed to [r()].
#' @inheritParams r
#'
#' @family callr functions
#' @export
#' @examples
#'
#' # Compare to r()
#' r(function() .libPaths())
#' r_vanilla(function() .libPaths())
#'
#' r(function() getOption("repos"))
#' r_vanilla(function() getOption("repos"))

r_vanilla <- function(func, args = list(), libpath = character(),
                      repos = c(CRAN = "@CRAN@"), cmdargs = "--slave",
                      system_profile = FALSE, user_profile = FALSE,
                      env = character(), ...) {

  r(func, args = args, libpath = libpath, repos = repos,
    cmdargs = cmdargs, system_profile = system_profile,
    user_profile = user_profile, env = env, ...)
}

#' Run an R child process in safe mode
#'
#' The following options are set up:
#' * The library path is set to the current path.
#' * Makes sure that at least one reasonable CRAN mirror is set up.
#'     * Some command line arguments are added to avoid saving
#'       `.RData` files, etc. See them above.
#'     * The system and user profile files are ignored.
#'     * Various environment variables are set: `CYGWIN` to avoid
#'       warnings about DOS-style paths, `R_TESTS` to avoid issues
#'       when `callr` is invoked from unit tests, `R_BROWSER`
#'       and `R_PDFVIEWER` to avoid starting a browser or a PDF viewer.
#'       See [rcmd_safe_env()].
#'
#' @param ... Additional arguments are passed to [r()].
#' @inheritParams r
#'
#' @family callr functions
#' @export

r_safe <- function(func, args = list(), libpath = .libPaths(),
                   repos = c(getOption("repos"),
                     c(CRAN = "https://cran.rstudio.com")),
                   cmdargs = c("--no-site-file", "--no-environ", "--slave",
                     "--no-save", "--no-restore"), system_profile = FALSE,
                   user_profile = FALSE, env = rcmd_safe_env(), ...) {

  r(func, args = args, libpath = libpath, repos = repos,
    cmdargs = cmdargs, system_profile = system_profile,
    user_profile = user_profile, env = env, ...)
}
