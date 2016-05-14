
#' Evaluate an expression in another R session
#'
#' Supported modes: \itemize{
#' \item \sQuote{vanilla} Do not load any startup files, and
#'   use a temporary empty package library. It tries to imitate
#'   an empty vanilla R session. Note that it does not set
#'   \code{options("repos")} currently, so if you want to use
#'   a CRAN mirror, you need to set that.
#' \item \sQuote{copycat} Try to mimic the settings from the
#'   current R session: startup files, package libraries,
#'   loaded and attached packages, and the \code{options("repos")}
#'   settings will be replicated in the new R session. Not yet
#'   implemented.
#' \item \sQuote{expert} Various parameters are set up manually.
#'   See parameters below.
#' }
#'
#' Parameters in expert mode: \itemize{
#' \item \sQuote{libpath} The library path. Defaults to the current
#'   library path in expert mode.
#' \item \sQuote{repos} The \sQuote{repos} option. Defaults to
#'   \code{getOption("repos")} in expert mode.
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
#' @param mode How to set up the other R session. See details below.
#' @param func Function object to call in the new R process.
#' @param args Arguments to pass to the function.
#' @param ... Extra arguments, used in expert mode.
#' @return Value of the evaluated expression.
#'
#' @examples
#'
#' # Workspace is empty in vanilla mode
#' r_eval(ls)
#'
#' # library path is "empty" in vanilla mode
#' r_eval(.libPaths)
#'
#' # This should ideally only include base and recommended packages
#' r_eval(installed.packages)
#'
#' @export

r_eval <- function(func, args = list(),
                   mode = c("vanilla", "copycat", "expert"), ...) {

  mode <- match.arg(mode)
  stopifnot(is.function(func))
  stopifnot(is.list(args))
  extra <- list(...)

  if (length(extra) && mode != "expert") {
    warning("Extra arguments are only considered in expert mode")
  }

  ## Save function to file
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(list(func, args), file = tmp)

  on.exit(unlink(res), add = TRUE)
  res <- if (mode == "vanilla") {
    r_eval_vanilla(tmp)

  } else if (mode == "copycat") {
    r_eval_copycat(tmp)

  } else if (mode == "expert") {
    r_eval_expert(tmp, extra)
  }

  readRDS(res)
}

r_eval_vanilla <- function(expr_file) {
  params <- list(
    libpath = character(),
    repos = c(CRAN = "@CRAN@")
  )
  r_eval_expert(expr_file, params)
}

r_eval_copycat <- function(expr) {
  stop("'copycat' is not yet implemented")
}
