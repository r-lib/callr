
#' Run an R child process, with no configuration
#'
#' It tries to mimic a fresh R installation. In particular: \itemize{
#' \item No library path setting.
#' \item No CRAN(-like) repository is set.
#' \item The system and user profiles are not run.
#' }
#'
#' @param ... Additional arguments are passed to \code{\link{r}}.
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
#' The following options are set up: \itemize{
#'   \item The library path is set to the current path.
#'   \item Makes sure that at least one reasonable CRAN mirror is set up.
#'   \item Some command line arguments are added to avoid saving
#'     \code{.RData} files, etc. See them above.
#'   \item The system and user profile files are ignored.
#'   \item Various environment variables are set: \code{CYGWIN} to avoid
#'     warnings about DOS-style paths, \code{R_TESTS} to avoid issues
#'     when \code{callr} is invoked from unit tests, \code{R_BROWSER}
#'     and \code{R_PDFVIEWER} to avoid starting a browser or a PDF viewer.
#' }
#'
#' @param ... Additional arguments are passed to \code{\link{r}}.
#' @inheritParams r
#'
#' @family callr functions
#' @export

r_safe <- function(func, args = list(), libpath = .libPaths(),
                   repos = c(getOption("repos"),
                     c(CRAN = "https://cran.rstudio.com")),
                   cmdargs = c("--no-site-file", "--no-environ", "--slave",
                     "--no-save", "--no-restore"), system_profile = FALSE,
                   user_profile = FALSE, env =
                     c(CYGWIN = "nodosfilewarning", R_TESTS = "",
                       R_BROWSER = "false", R_PDFVIEWER = "false"), ...) {

  r(func, args = args, libpath = libpath, repos = repos,
    cmdargs = cmdargs, system_profile = system_profile,
    user_profile = user_profile, env = env, ...)
}
