
#' Run an R CMD command in the background
#'
#' The child process is started in the background, and the function
#' return immediately.
#'
#' @inheritParams rcmd
#' @param supervise Whether to register the process with a supervisor. If \code{TRUE},
#'   the supervisor will ensure that the process is killed when the R process
#'   exits.
#' @return It returns a [process] object.
#'
#' @family R CMD commands
#' @export

rcmd_bg <- function(cmd, cmdargs = character(), libpath = .libPaths(),
                    stdout = "|", stderr = "|",
                    poll_connection = TRUE,
                    repos = c(getOption("repos"),
                      c(CRAN = "https://cloud.r-project.org")),
                    system_profile = FALSE, user_profile = FALSE,
                    env = rcmd_safe_env(), wd = ".",
                    supervise = FALSE) {

  rcmd_process$new(options = as.list(environment()))
}
