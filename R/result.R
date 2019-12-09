
#' Read the result object from the output file, or the error
#'
#' Even if an error happens, the output file might still exist,
#' because [saveRDS()] creates the file before evaluating its object
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
  if (output$timeout) throw(new_callr_error(output))

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
      throw(new_callr_error(output, killmsg))
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
          throw(new_callr_error(output, killmsg))
        }
      }
    )

    return(ret)
  }

  ## The error RDS might be corrupt, too, if we crashed/got killed after
  ## an error
  tryCatch(
    remerr <- readRDS(errorres),
    error = function(e) throw(new_callr_error(output, killmsg))
  )

  if (remerr[[1]] == "error") {
    remerr[[2]]$message <- remerr[[2]]$message %||% "interrupt"
    msg <- conditionMessage(remerr[[2]]$error)
    newerr <- new_callr_error(output, msg)
    throw(newerr, parent = remerr[[2]])

  } else if (remerr[[1]] == "stack") {
    myerr <- structure(
      list(
        message = conditionMessage(remerr[[2]]),
        call = conditionCall(remerr[[2]]),
        stack = clean_stack(remerr[[3]])
      ),
      class = c("callr_error", "error", "condition")
    )
    throw(myerr)

  } else if (remerr[[1]] == "debugger") {
    utils::debugger(clean_stack(remerr[[3]]))

  } else {
    throw(new_error("Unknown callr error strategy: ", remerr[[1]])) # nocov
  }
}

clean_stack <- function(stack) {
  att <- attributes(stack)
  att$names <- utils::head(utils::tail(att$names, -11), -2)
  res <- utils::head(utils::tail(stack, -11), -2)
  attributes(res) <- att

  res
}
