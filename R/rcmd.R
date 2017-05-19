
#' Run an R CMD command
#'
#' Run an R CMD command form within R. This will usually start
#' another R process, from a shell script.
#'
#' Starting from `callr` 1.1.0, `rcmd()` has safer defaults, the same as
#' the `rcmd_safe()` default values. Use [rcmd_copycat()] for the old
#' defaults.
#'
#' @param cmd Command to run. See `R --help` from the command
#'   line for the various commands. In the current version of R (3.2.4)
#'   these are: BATCH, COMPILE, SHLIB, INSTALL, REMOVE, build, check,
#'   LINK, Rprof, Rdconv, Rd2pdf, Rd2txt, Stangle, Sweave, Rdiff, config,
#'   javareconf, rtags.
#' @param cmdargs Command line arguments.
#' @param stdout Optionally a file name to send the standard output to.
#' @param stderr Optionally a file name to send the standard error to.
#' @param echo Whether to echo the complete command run by \code{rcmd}.
#' @param wd Working directory to use for running the command. Defaults
#'   to the current working directory.
#' @param fail_on_status Whether to throw an R error if the command returns
#'   with a non-zero status code. By default no error is thrown.
#' @inheritParams r
#' @return A list with the command line (\code{$command}),
#'   standard output (\code{$stdout}), standard error (\code{stderr})
#'   and exit status (\code{$status}) of the external \code{R CMD} command.
#'
#' @family R CMD commands
#' @export
#'
#' @examples
#' rcmd("config", "CC")

rcmd <- function(cmd, cmdargs = character(), libpath = .libPaths(),
                 repos = c(getOption("repos"),
                   c(CRAN = "https://cloud.r-project.org")),
                 stdout = NULL, stderr = NULL, echo = FALSE, show = FALSE,
                 callback = NULL,
                 system_profile = FALSE, user_profile = FALSE,
                 env = rcmd_safe_env(), wd = ".",
                 fail_on_status = FALSE) {

  if(os_platform() == "windows") {
    rbin <- file.path(R.home("bin"), "Rcmd.exe")
    cmdargs <- c(cmd, cmdargs)

  } else {
    rbin <- file.path(R.home("bin"), "R")
    cmdargs <- c("CMD", cmd, cmdargs)
  }

  run_r(
    bin = rbin,
    args = cmdargs,
    libpath = libpath,
    repos = repos,
    stdout = stdout,
    stderr = stderr,
    echo = echo,
    show = show,
    callback = callback,
    system_profile = system_profile,
    user_profile = user_profile,
    env = env,
    wd = wd,
    fail_on_status = fail_on_status
  )
}

#' @rdname rcmd
#' @export

rcmd_safe <- rcmd

#' \code{rcmd_safe_env} returns a set of environment variables that are
#' more appropriate for \code{rcmd_safe}.
#'
#' @export
#' @rdname rcmd_safe

rcmd_safe_env <- function() {

  vars <- c(
    CYGWIN = "nodosfilewarning",
    R_TESTS = "",
    R_BROWSER = "false",
    R_PDFVIEWER = "false"
  )

  if (is.na(Sys.getenv("NOT_CRAN", unset = NA))) {
    vars[["NOT_CRAN"]] <- "true"
  }

  vars
}

#' Call and R CMD command, while mimicking the current R session
#'
#' This function is similar to [rcmd()], but it has slightly different
#' defaults:
#' * The `repos` options is unchanged.
#' * No extra environment variables are defined.
#'
#' @inheritParams rcmd
#' @param ... Additional arguments are passed to [rcmd()].
#'
#' @family R CMD commands
#' @export

rcmd_copycat <- function(cmd, cmdargs = character(), libpath = .libPaths(),
                         repos = getOption("repos"), env = character(),
                         ...) {

  rcmd(cmd, cmdargs = cmdargs, libpath = libpath, repos = repos, env = env)
}
