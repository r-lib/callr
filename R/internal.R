
r_internal <- function(func, args, libpath, repos, stdout, stderr,
                       error, cmdargs, show, callback, system_profile,
                       user_profile, env) {

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
    is.null(callback) || is.function(callback),
    is_flag(system_profile),
    is_flag(user_profile),
    is.character(env)
  )

  ## Save function to file
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  environment(func) <- .GlobalEnv
  saveRDS(list(func, args), file = tmp)

  res <- r_tmp(tmp, libpath, repos, stdout, stderr, error, cmdargs,
               show, callback, system_profile, user_profile, env)

  get_result(res)
}

r_tmp <- function(expr_file, libpath, repos, stdout, stderr, error,
                  cmdargs, show, callback, system_profile,
                  user_profile, env) {

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
    callback = callback,
    system_profile = system_profile,
    user_profile = user_profile,
    env = env
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
