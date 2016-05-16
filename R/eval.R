
#' Evaluate an expression in another R session
#'
#' Parameters \itemize{
#' \item \sQuote{libpath} The library path. Defaults to the current
#'   library path.
#' \item \sQuote{repos} The \sQuote{repos} option. Defaults to
#'   \code{getOption("repos")}.
#' \item \sQuote{stderr} The name of the file the standard output of
#'   the child R process will be written to.
#'   By default the child process runs with the \code{--slave} option,
#'   so the commands executed are not echoed and will not be shown
#'   in the standard output. Also note that you need to call `print()`
#'   explicitly to show the output of the command(s).
#' \item \sQuote{stderr} The name of the file the standard error of
#'   the child R process will be written to.
#'   In particular \code{message()} sends output to the standard
#'   error. If nothing was sent to the standard error, then this file
#'   will be empty.
#' }
#'
#' @section Setting environment variables:
#'
#' \code{callr} itself does not support setting environment variables
#' for the child process, because this can be done easily with other
#' tools. In particular, you can call \code{callr} withing the
#' the \code{withr} package's \code{with_envvar} function:
#'
#' \preformatted{  withr::with_envvar(
#'     c(CALL_R_TESTING = "foobar"),
#'     r_eval(function() Sys.getenv("CALL_R_TESTING"))
#'   )}
#'
#' @section Setting the path:
#'
#' Similarly to environment variables, the PATH can be set using
#' the \code{withr} package:
#'
#' \preformatted{  withr::with_path(
#'     "/bin", action = "replace",
#'     r_eval(function() Sys.getenv("PATH"))
#'   )}
#'
#' @param func Function object to call in the new R process.
#' @param args Arguments to pass to the function.
#' @param ... Extra arguments, see below.
#' @return Value of the evaluated expression.
#'
#' @examples
#'
#' # Workspace is empty
#' r_eval(ls)
#'
#' # library path is the same by default
#' r_eval(.libPaths)
#' .libPaths()
#'
#' @export

r_eval <- function(func, args = list(), ...) {

  stopifnot(is.function(func))
  stopifnot(is.list(args))
  extra <- list(...)

  ## Parameters
  extra$libpath <- extra$libpath %||% .libPaths()
  extra$repos   <- extra$repos   %||% getOption("repos")
  extra$stdout  <- extra$stdout  %||% NULL
  extra$stderr  <- extra$stderr  %||% NULL
  check_extra_args(extra)

  ## Save function to file
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(list(func, args), file = tmp)

  on.exit(unlink(res), add = TRUE)
  res <- r_eval_tmp(tmp, extra)

  readRDS(res)
}

check_extra_args <- function(extra) {

  stopifnot(
    is.character(extra$libpath),
    is.character(extra$repos),
    is.null(extra$stdout) || is_string(extra$stdout),
    is.null(extra$stderr) || is_string(extra$stderr)
  )
}

r_eval_tmp <- function(expr_file, extra) {

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
