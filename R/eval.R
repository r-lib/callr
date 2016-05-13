
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
#' }
#'
#' @param mode How to set up the other R session. See details below.
#' @param expr Expression to evaluate.
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

r_eval <- function(expr, mode = c("vanilla", "copycat")) {

  mode <- match.arg(mode)
  expr <- substitute(expr)

  ## Save expression to file
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(expr, file = tmp)

  on.exit(unlink(res), add = TRUE)
  res <- if (mode == "vanilla") {
    r_eval_vanilla(tmp)

  } else if (mode == "copycat") {
    r_eval_copycat(tmp)
  }

  readRDS(res)
}

r_eval_vanilla <- function(expr_file) {
  res <- tempfile()

  rscript <- make_vanilla_script(expr_file, res)
  on.exit(unlink(rscript), add = TRUE)

  rbin <- file.path(R.home("bin"), "R")

  out <- with_envvar(
    c(R_LIBS = "",
      R_LIBS_USER = "",
      R_LIBS_SITE = "",
      R_PROFILE_USER = tempfile()),
    safe_system(rbin, args = c("-q", "-f", rscript))
  )

  if (out$status != 0) stop("callr error: ", out$stderr)

  res
}

r_eval_copycat <- function(expr) {
  stop("'copycat' is not yet implemented")
}
