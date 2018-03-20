
#' Evaluate an expression in another R session
#'
#' From `callr` version 2.0.0, `r()` is equivalent to `r_safe()`, and
#' tries to set up a less error prone execution environment. In particular:
#' * It makes sure that at least one reasonable CRAN mirror is set up.
#' * Adds some command line arguments are added to avoid saving
#'   `.RData` files, etc.
#' * Ignores the system and user profiles.
#' * Various environment variables are set: `CYGWIN` to avoid
#'   warnings about DOS-style paths, `R_TESTS` to avoid issues
#'   when `callr` is invoked from unit tests, `R_BROWSER`
#'   and `R_PDFVIEWER` to avoid starting a browser or a PDF viewer.
#'   See [rcmd_safe_env()].
#'
#' The pre-2.0.0 `r()` function is called [r_copycat()] now.
#'
#' @param func Function object to call in the new R process.
#'   The function should be self-contained and only refer to
#'   other functions and use variables explicitly from other packages
#'   using the `::` notation. The environment of the function
#'   is set to `.GlobalEnv` before passing it to the child process.
#'   Because of this, it is good practice to create an anonymous
#'   function and pass that to `callr`, instead of passing
#'   a function object from a (base or other) package. In particular
#'   ```
#'   r(.libPaths)
#'   ```
#'   does not work, because `.libPaths` is defined in a special
#'   environment, but
#'   ```
#'   r(function() .libPaths())
#'   ```
#'   works just fine.
#' @param args Arguments to pass to the function. Must be a list.
#' @param libpath The library path.
#' @param repos The *repos* option. If `NULL`, then no
#'   *repos* option is set. This options is only used if
#'   `user_profile` or `system_profile` is set `FALSE`,
#'   as it is set using the system or the user profile.
#' @param stdout The name of the file the standard output of
#'   the child R process will be written to.
#'   If the child process runs with the `--slave` option (the default),
#'   then the commands are not echoed and will not be shown
#'   in the standard output. Also note that you need to call `print()`
#'   explicitly to show the output of the command(s).
#' @param stderr The name of the file the standard error of
#'   the child R process will be written to.
#'   In particular `message()` sends output to the standard
#'   error. If nothing was sent to the standard error, then this file
#'   will be empty. This can be the same file as `stderr`, although there
#'   is no guarantee that the lines will be in the correct chronological
#'   order.
#' @param error What to do if the remote process throws an error.
#'   See details below.
#' @param cmdargs Command line arguments to pass to the R process.
#'   Note that `c("-f", rscript)` is appended to this, `rscript`
#'   is the name of the script file to run. This contains a call to the
#'   supplied function and some error handling code.
#' @param show Logical, whether to show the standard output on the screen
#'   while the child process is running. Note that this is independent
#'   of the `stdout` and `stderr` arguments. The standard
#'   error is not shown currently.
#' @param callback A function to call for each line of the standard
#'   output and standard error from the child process. It works together
#'   with the `show` option; i.e. if `show = TRUE`, and a
#'   callback is provided, then the output is shown of the screen, and the
#'   callback is also called.
#' @param block_callback A function to call for each block of the standard
#'   output and standard error. This callback is not line oriented, i.e.
#'   multiple lines or half a line can be passed to the callback.
#' @param spinner Whether to show a calming spinner on the screen while
#'   the child R session is running. By default it is shown if
#'   `show = TRUE` and the R session is interactive.
#' @param system_profile Whether to use the system profile file.
#' @param user_profile Whether to use the user's profile file.
#' @param env Environment variables to set for the child process.
#' @param timeout Timeout for the function call to finish. It can be a
#'   [base::difftime] object, or a real number, meaning seconds.
#'   If the process does not finish before the timeout period expires,
#'   then a `system_command_timeout_error` error is thrown. `Inf`
#'   means no timeout.
#' @return Value of the evaluated expression.
#'
#' @section Error handling:
#'
#' `callr` handles errors properly. If the child process throws an
#' error, then `callr` throws an error with the same error message
#' in the parent process.
#'
#' The `error` expert argument may be used to specify a different
#' behavior on error. The following values are possible:
#' * `error` is the default behavior: throw an error in the parent, with
#'   the same error message. In fact the same error object is thrown again.
#' * `stack` also throws an error in the parent, but the error
#'   is of a special kind, class `callr_error`, and it contains
#'   both the original error object, and the call stack of the child,
#'   as written out by [utils::dump.frames()].
#' * `debugger` is similar to `stack`, but in addition
#'   to returning the complete call stack, it also start up a debugger
#'   in the child call stack, via [utils::debugger()].
#'
#' The default error behavior can be also set using the `callr.error`
#' option. This is useful to debug code that uses `callr`.
#'
#' @family callr functions
#' @examples
#' \dontrun{
#' # Workspace is empty
#' r(function() ls())
#'
#' # library path is the same by default
#' r(function() .libPaths())
#' .libPaths()
#' }
#'
#' @export

r <- function(func, args = list(), libpath = .libPaths(),
              repos = c(getOption("repos"),
                c(CRAN = "https://cloud.r-project.org")),
              stdout = NULL, stderr = NULL,
              error = getOption("callr.error", "error"),
              cmdargs = c("--no-site-file", "--no-environ", "--slave",
                "--no-save", "--no-restore"),
              show = FALSE, callback = NULL,
              block_callback = NULL, spinner = show && interactive(),
              system_profile = FALSE, user_profile = FALSE,
              env = rcmd_safe_env(), timeout = Inf) {

  ## This contains the context that we set up in steps
  options <- convert_and_check_my_args(as.list(environment()))

  ## This cleans up everything...
  on.exit(unlink(options$tmp_files, recursive = TRUE), add = TRUE)

  options <- setup_script_files(options)
  options <- setup_context(options)
  options <- setup_callbacks(options)
  options <- setup_r_binary_and_args(options)

  out <- run_r(options)

  get_result(output = out, options)
}
