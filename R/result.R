
#' Read the result object from the output file, or the error
#'
#' Even if an error happens, the ouput file might still exist,
#' because saveRDS creates the file before evaluating its object
#' argument. So we need to check for the error file to decide
#' if an error happened.
#'
#' @param out List of the output object from [run()] and
#'   the name of the result file to read. For the error file,
#'   `.error` is appended to this.
#' @param options The context, including all parameters.
#' @return If no error happened, the result is returned. Otherwise
#'   we handle the error.
#'
#' @keywords internal

get_result <- function(output, options) {

  res <- options$result_file

  ## Timeout?
  if (output$timeout) stop(make_error(output))

  ## No output file and no error file? Some other (system?) error then,
  ## unless exit status was zero, which is probably just quit().
  ## (Newer R versions do not write a corrupt RDS file in this case.)
  ret <- NULL
  errorres <- paste0(res, ".error")
  killmsg <- paste(
    "could not start R, exited with non-zero status,",
    "has crashed or was killed")
  if (! file.exists(res) && ! file.exists(errorres)) {
    if (is.na(output$status) || output$status != 0) {
      stop(make_error(output, killmsg))
    } else  {
      return(ret)
    }
  }

  ## No error file? Then probably all is well, return the output
  ## If this is currupt, then the R process has crashed
  ## This cannot happen from R 3.5.0, because that version only writes
  ## out the output file if no error or crash has happened.
  ## (Older R versions write a corrupt RDS file in this case.)
  if (! file.exists(errorres)) {
    tryCatch(
      ret <- readRDS(res),
      error = function(e) {
        if (is.na(output$status) || output$status != 0) {
          stop(make_error(output, killmsg))
        }
      }
    )

    return(ret)
  }

  ## The error RDS might be corrupt, too, if we crashed/got killed after
  ## an error
  tryCatch(
    err <- readRDS(errorres),
    error = function(e) stop(make_error(output, killmsg))
  )

  if (err[[1]] == "error") {
    err[[2]]$message <- err[[2]]$message %||% "interrupt"
    stop(err[[2]])

  } else if (err[[1]] == "stack") {
    myerr <- structure(
      list(
        message = conditionMessage(err[[2]]),
        call = conditionCall(err[[2]]),
        stack = clean_stack(err[[3]])
      ),
      class = c("callr_error", "error", "condition")
    )
    stop(myerr)

  } else if (err[[1]] == "debugger") {
    utils::debugger(clean_stack(err[[3]]))

  } else {
    stop("Unknown callr error strategy: ", err[[1]]) # nocov
  }
}

clean_stack <- function(stack) {
  att <- attributes(stack)
  att$names <- utils::head(utils::tail(att$names, -11), -2)
  res <- utils::head(utils::tail(stack, -11), -2)
  attributes(res) <- att

  res
}
