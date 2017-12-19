
#' Run an R CMD command in the background
#'
#' The child process is started in the background, and the function
#' return immediately.
#'
#' @inheritParams rcmd
#' @return It returns a [process] object.
#'
#' @family R CMD commands
#' @export

rcmd_bg <- function(cmd, cmdargs = character(), libpath = .libPaths(),
                    stdout = "|", stderr = "|",
                    repos = c(getOption("repos"),
                      c(CRAN = "https://cloud.r-project.org")),
                    system_profile = FALSE, user_profile = FALSE,
                    env = rcmd_safe_env(), wd = ".") {

  rcmd_process$new(options = as.list(environment()))
}
