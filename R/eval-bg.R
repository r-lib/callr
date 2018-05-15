
#' Evaluate an expression in another R session, in the background
#'
#' Starts evaluating an R function call in a background R process, and
#' returns immediately.
#'
#' @inheritParams r
#' @param supervise Whether to register the process with a supervisor. If \code{TRUE},
#'   the supervisor will ensure that the process is killed when the R process
#'   exits.
#' @return An `r_process` object, which inherits from [process],
#'   so all `process` methods can be called on it, and in addition it also
#'   has a `get_result()` method to collect the result.
#'
#' @export
#' @examples
#' \dontrun{
#' rx <- r_bg(function() 1 + 2)
#'
#' # wait until it is done
#' rx$wait()
#' rx$is_alive()
#' rx$get_result()
#' }

r_bg <- function(func, args = list(), libpath = .libPaths(),
                 repos = c(getOption("repos"),
                   c(CRAN = "https://cloud.r-project.org")),
                 stdout = "|", stderr = "|",
                 error = getOption("callr.error", "error"),
                 cmdargs = c("--no-site-file", "--slave",
                   "--no-save", "--no-restore"),
                 system_profile = FALSE, user_profile = FALSE,
                 env = rcmd_safe_env(), supervise = FALSE) {

  r_process$new(options = as.list(environment()))
}
