
#' Create options for an [r_process] object
#'
#' @param ... Options to override, named arguments.
#' @return A list of options.
#'
#' `r_process_options()` creates a set of options to initialize a new
#' object from the `r_process` class. Its arguments must be named, the
#' names are used as option names. The options correspond to (some of)
#' the arguments of the [r()] function. At least the `func` option must be
#' specified, this is the R function to run in the background.
#'
#' @export
#' @examples
#' ## List all options and their default values:
#' r_process_options()

r_process_options <- function(...) {
  update_options(r_process_options_default(), ...)
}

#' Create options for an [rcmd_process] object
#'
#' @param ... Options to override, named arguments.
#' @return A list of options.
#'
#' `rcmd_process_options()` creates a set of options to initialize a new
#' object from the `rcmd_process` class. Its arguments must be named, the
#' names are used as option names. The options correspond to (some of)
#' the arguments of the [rcmd()] function. At least the `cmd` option must
#' be specified, to select the `R CMD` subcommand to run. Typically
#' `cmdargs` is specified as well, to supply more arguments to `R CMD`.
#'
#' @export
#' @examples
#' ## List all options and their default values:
#' rcmd_process_options()

rcmd_process_options <- function(...) {
  update_options(rcmd_process_options_default(), ...)
}

r_process_options_default <- function() {
  list(
    func = NULL,
    args = list(),
    libpath = .libPaths(),
    repos = c(getOption("repos"), CRAN = "https://cloud.r-project.org"),
    stdout = "|",
    stderr = "|",
    error = getOption("callr.error", "error"),
    cmdargs = c("--slave", "--no-save", "--no-restore"),
    system_profile = FALSE,
    user_profile = FALSE,
    env = character(),
    supervise = FALSE,
    load_hook = NULL
  )
}

rcmd_process_options_default <- function() {
  list(
    cmd = NULL,
    cmdargs = character(),
    libpath = .libPaths(),
    stdout = "|",
    stderr = "|",
    repos = c(getOption("repos"), CRAN = "https://cloud.r-project.org"),
    system_profile = FALSE,
    user_profile = FALSE,
    env = rcmd_safe_env(),
    wd = ".",
    supervise = FALSE
  )
}

#' @importFrom utils modifyList

update_options <- function(old_opts, ...) {
  new_opts <- list(...)
  stopifnot(is.named(new_opts))
  check_for_option_names(old_opts, new_opts)
  modifyList(old_opts, new_opts)
}

check_for_option_names <- function(old, new) {
  if (length(miss <- setdiff(names(new), names(old)))) {
    stop("Unknown option", if (length(miss) > 1) "s", ":",
         enumerate(sQuote(miss)))
  }
}
