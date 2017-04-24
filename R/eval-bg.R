
#' Evaluate an expression in another R session, in the background
#'
#' Starts evaluating an R function call in a background R process, and
#' return immediately.
#'
#' @inheritParams r
#' @return An `r_process` object, which inherits from [processx::process],
#'   so all `process` methods can be called on it, and in addition it also
#'   has a `get_result()` method to collect the result.
#'
#' @export
#' @importFrom processx process
#' @examples
#' rx <- r_bg(function() 1 + 2)
#'
#' # wait until it is done
#' rx$wait()
#' rx$is_alive()
#' rx$get_result()

r_bg <- function(func, args = list(), libpath = .libPaths(),
                 repos = getOption("repos"), stdout = "|", stderr = "|",
                 error = c("error", "stack", "debugger"),
                 cmdargs = "--slave",
                 system_profile = TRUE, user_profile = TRUE,
                 env = character()) {

  ## This contains the context that we set up in steps
  options <- convert_and_check_my_args(as.list(environment()))

  options <- setup_script_files(options)
  options <- setup_context(options)
  options <- setup_r_binary_and_args(options)

  with_envvar(env, r_process$new(options))
}

#' @param ... Additional argument, passed to [r_bg()].
#' @export
#' @rdname r_bg

r_bg_safe <- function(func, args = list(), libpath = .libPaths(),
                      repos = c(getOption("repos"),
                        c(CRAN = "https://cran.rstudio.com")),
                      cmdargs = c("--no-site-file", "--no-environ", "--slave",
                        "--no-save", "--no-restore"), system_profile = FALSE,
                      user_profile = FALSE, env = rcmd_safe_env(), ...) {

  r_bg(func, args = args, libpath = libpath, repos = repos,
       cmdargs = cmdargs, system_profile = system_profile,
       user_profile = user_profile, env = env, ...)
}
