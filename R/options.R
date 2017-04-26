
#' @export

r_process_options <- function(...) {
  update_options(r_process_options_default(), ...)
}

#' @export

rcmd_process_options <- function(...) {
  update_options(rcmd_process_options_default(), ...)
}

r_process_options_default <- function() {
  list(
    func = NULL,
    args = list(),
    libpath = .libPaths(),
    repos = getOption("repos"),
    stdout = "|",
    stderr = "|",
    error = c("error", "stack", "debugger"),
    cmdargs = "--slave",
    system_profile = TRUE,
    user_profile = TRUE,
    env = character()
  )
}

rcmd_process_options_default <- function() {
  list(
    cmd = NULL,
    cmdargs = character(),
    libpath = .libPaths(),
    stdout = "|",
    stderr = "|",
    repos = getOption("repos"),
    system_profile = FALSE,
    user_profile = FALSE,
    env = character(),
    wd = "."
  )
}

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
