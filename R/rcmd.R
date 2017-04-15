
#' Run an R CMD command
#'
#' Run an R CMD command form within R. This will usually start
#' another R process, from a shell script.
#'
#' @param cmd Command to run. See \code{R --help} from the command
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
                 repos = getOption("repos"), stdout = NULL,
                 stderr = NULL, echo = FALSE, show = FALSE, callback = NULL,
                 block_callback = NULL, spinner = show && interactive(),
                 system_profile = FALSE, user_profile = FALSE,
                 env = character(), timeout = Inf, wd = ".",
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
    block_callback = block_callback,
    spinner = spinner,
    system_profile = system_profile,
    user_profile = user_profile,
    env = env,
    timeout = timeout,
    wd = wd,
    fail_on_status = fail_on_status
  )
}

#' Call R CMD <command> safely
#'
#' Very similar to \code{\link{rcmd}}, but with different defaults,
#' that tend to create a less error-prone execution environment for the
#' child process.
#'
#' @param ... Additional arguments are passed to \code{\link{rcmd}}.
#' @inheritParams rcmd
#'
#' @family R CMD commands
#' @export

rcmd_safe <- function(cmd, cmdargs = character(), libpath = .libPaths(),
                      repos = c(getOption("repos"),
                        c(CRAN = "https://cran.rstudio.com")),
                      system_profile = FALSE, user_profile = FALSE,
                      env = rcmd_safe_env(), ...) {

  rcmd(cmd, cmdargs = cmdargs, libpath = libpath, repos = repos,
       system_profile = system_profile, user_profile = user_profile,
       env = env, ...)
}

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
