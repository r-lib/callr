
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
#' @param mode How to set up the other R session. See details below.
#' @param expr Expression to evaluate.
#' @param ... Extra arguments, used in expert mode.
#' @return Value of the evaluated expression.
#'
#' @examples
#'
#' # Workspace is empty in vanilla mode
#' r_eval(ls())
#'
#' # library path is "empty" in vanilla mode
#' r_eval(.libPaths())
#'
#' # This should ideally only include base and recommended packages
#' r_eval(installed.packages())
#'
#' @export

r_eval <- function(expr, mode = c("vanilla", "copycat", "expert"), ...) {

  mode <- match.arg(mode)
  expr <- substitute(expr)
  extra <- list(...)

  if (length(extra) && mode != "expert") {
    warning("Extra arguments are only considered in expert mode")
  }

  ## Save expression to file
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(expr, file = tmp)

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
