
#' Evaluate an expression in another R session
#'
#' @param func Function object to call in the new R process.
#' @param args Arguments to pass to the function. Must be a list.
#'   vector.
#' @param libpath The library path. Defaults to the current
#'   library path.
#' @param repos The \sQuote{repos} option. Defaults to
#'   \code{getOption("repos")}.
#' @param stdout The name of the file the standard output of
#'   the child R process will be written to.
#'   By default the child process runs with the \code{--slave} option,
#'   so the commands executed are not echoed and will not be shown
#'   in the standard output. Also note that you need to call `print()`
#'   explicitly to show the output of the command(s).
#' @param stderr The name of the file the standard error of
#'   the child R process will be written to.
#'   In particular \code{message()} sends output to the standard
#'   error. If nothing was sent to the standard error, then this file
#'   will be empty.
#' @param error What to do if the remote process throws an error.
#'   See details below.
#' @param cmdargs Command line arguments to pass to the R process.
#'   Note that \code{c("-f", rscript)} is appended to this, \code{rscript}
#'   is the name of the script file to run. This contains a call to the
#'   supplied function and some error handling code.
#' @param show Logical, whether to show the standard output on the screen
#'   while the child process is running. Note that this is independent
#'   of the \code{stdout} and \code{stderr} arguments. The standard
#'   error is not shown currently.
#' @param callback A function to call for each line of the standard
#'   output from the child process. It works together with the \code{show}
#'   option; i.e. if \code{show = TRUE}, and a callback is provided, then
#'   the output is shown of the screen, and the callback is also called.
#' @return Value of the evaluated expression.
#'
#' @section Error handling:
#'
#' \code{callr} handles errors properly. If the child process throws an
#' error, then \code{callr} throws an error with the same error message
#' in the parent process.
#'
#' The \sQuote{error} expert option may be used to specify a different
#' behavior on error. The following values are possible: \itemize{
#' \item \sQuote{error} is the default behavior: throw an error
#'   in the parent, with the same error message. In fact the same
#'   error object is thrown again.
#' \item \sQuote{stack} also throws an error in the parent, but the error
#'   is of a special kind, class \code{callr_condition}, and it contains
#'   both the original error object, and the call stack of the child,
#'   as written out by \code{\link[utils]{dump.frames}}.
#' \item \sQuote{debugger} is similar to \sQuote{stack}, but in addition
#'   to returning the complete call stack, it also start up a debugger
#'   in the child call stack, via \code{\link[utils]{debugger}}.
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
#' @examples
#'
#' # Workspace is empty
#' r_eval(ls)
#'
#' # library path is the same by default
#' r_eval(.libPaths)
#' .libPaths()
#'
#' @export

r_eval <- function(func, args = list(), libpath = .libPaths(),
                   repos = getOption("repos"), stdout = NULL, stderr = NULL,
                   error = c("error", "stack", "debugger"),
                   cmdargs = "--slave", show = FALSE, callback = NULL) {

  libpath <- as.character(libpath)
  repos <- as.character(repos)
  if (!is.null(stdout)) stdout <- as.character(stdout)
  if (!is.null(stderr)) stderr <- as.character(stderr)
  cmdargs <- as.character(cmdargs)

  stopifnot(
    is.function(func),
    is.list(args),
    is.character(libpath),
    is.character(repos),
    is.null(stdout) || is_string(stdout),
    is.null(stderr) || is_string(stderr),
    is.character(cmdargs),
    is_flag(show),
    is.null(callback) || is.function(callback)
  )
  error <- match.arg(error)

  ## Save function to file
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(list(func, args), file = tmp)

  res <- r_eval_tmp(tmp, libpath, repos, stdout, stderr, error, cmdargs,
                    show, callback)

  get_result(res)
}

r_eval_tmp <- function(expr_file, libpath, repos, stdout, stderr, error,
                       cmdargs, show, callback) {

  res <- tempfile()

  rscript <- make_vanilla_script(expr_file, res, error)
  on.exit(unlink(rscript), add = TRUE)

  out <- run_r(
    bin = paste0(R.home("bin"), "/R"),
    args = c(cmdargs, "-f", rscript),
    libpath = libpath,
    repos = repos,
    stdout = stdout,
    stderr = stderr,
    show = show,
    callback = callback
  )

  res
}

#' Read the result object from the output file, or the error
#'
#' Even if an error happens, the ouput file might still exist,
#' because saveRDS creates the file before evaluating its object
#' argument. So we need to check for the error file to decide
#' if an error happened.
#'
#' @param res Name of the result file to read. For the error file,
#'   \code{".error"} is appended.
#' @return If no error happened, the result is returned. Otherwise
#'   we handle the error.
#'
#' @keywords internal
#' @importFrom utils debugger

get_result <- function(res) {

  on.exit(try(unlink(res), silent = TRUE), add = TRUE)
  on.exit(try(unlink(paste0(res, ".error")), silent = TRUE), add = TRUE)

  if (! file.exists(paste0(res, ".error"))) return(readRDS(res))

  err <- readRDS(paste0(res, ".error"))

  if (err[[1]] == "error") {
    stop(err[[2]])

  } else if (err[[1]] == "stack") {
    myerr <- structure(
      list(
        message = conditionMessage(err[[2]]),
        call = conditionCall(err[[2]]),
        stack = clean_stack(err[[3]])
      ),
      class = c("callrError", "error", "condition")
    )
    stop(myerr)

  } else if (err[[1]] == "debugger") {
    debugger(clean_stack(err[[3]]))

  } else {
    stop("Unknown callr error strategy: ", err[[1]]) # nocov
  }
}

#' @importFrom utils head tail

clean_stack <- function(stack) {
  ## We remove the first 4 calls (withCallingHandlers,
  ## saveRDS, do.call and do.call) and the last two
  ## (.handleSimpleError and h(simpleerror).
  att <- attributes(stack)
  att$names <- head(tail(att$names, -4), -2)
  res <- head(tail(stack, -4), -2)
  attributes(res) <- att

  res
}
