
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
#' @param libpath R library path to set up using environment variables.
#'   Defaults to the current library path.
#' @param repos The \code{repos} option to set CRAN mirrors and other
#'   CRAN-like repositories.
#' @param stdout Optionally a file name to send the standard output to.
#' @param stderr Optionally a file name to send the standard error to.
#' @return A list with the standard output (\code{$stdout}), standard
#'   error (\code{stderr}) and exit status (\code{$status}) of the
#'   external \code{R CMD} command.
#'
#' @export
#'
#' @examples
#' rcmd("config", "CC")

rcmd <- function(cmd, cmdargs = character(), libpath = .libPaths(),
                 repos = getOption("repos"), stdout = NULL,
                 stderr = NULL) {

  if(.Platform$OS.type == "windows") {
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
    stderr = stderr
  )
}
