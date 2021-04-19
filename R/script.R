
make_vanilla_script_expr <- function(expr_file, res, error,
                                     pre_hook = NULL, post_hook = NULL,
                                     messages = FALSE) {

  ## Code to handle errors in the child
  ## This will inserted into the main script
  err <- if (error == "error") {
    substitute({
      callr_data <- as.environment("tools:callr")$`__callr_data__`
      err <- callr_data$err

      assign(".Traceback", .traceback(4), envir = callr_data)

      dump.frames("__callr_dump__")
      assign(".Last.dump", .GlobalEnv$`__callr_dump__`, envir = callr_data)
      rm("__callr_dump__", envir = .GlobalEnv)

      # callr_remote_error does have conditionMessage and conditionCall
      # methods that refer to $error, but in the subprocess callr is not
      # loaded, maybe, and these methods are not defined. So we do add
      # the message and call of the original error
      e$call <- deparse(conditionCall(e), nlines = 6)
      e2 <- err$new_error(conditionMessage(e), call. = conditionCall(e))
      class(e2) <- c("callr_remote_error", class(e2))
      e2$error <- e
      # To find the frame of the evaluated function, we search for
      # do.call in the stack, and then skip one more frame, the other
      # do.call. This method only must change if the eval code changes,
      # obviously. Also, it might fail if the pre-hook has do.call() at
      # the top level.
      calls <- sys.calls()
      dcframe <- which(vapply(
        calls,
        function(x) length(x) >= 1 && identical(x[[1]], quote(do.call)),
        logical(1)))[1]
      if (!is.na(dcframe)) e2$`_ignore` <- list(c(1, dcframe + 1L))
      e2$`_pid` <- Sys.getpid()
      e2$`_timestamp` <- Sys.time()
      if (inherits(e, "rlib_error_2_0")) e2$parent <- e$parent
      e2 <- err$add_trace_back(e2, embed = FALSE)
      saveRDS(list("error", e2), file = paste0(`__res__`, ".error")) },
      list(`__res__` = res)
    )

  } else if (error %in% c("stack", "debugger")) {
    substitute(
      {
        callr_data <- as.environment("tools:callr")$`__callr_data__`
        assign(".Traceback", .traceback(4), envir = callr_data)
        dump.frames("__dump__")         # nocov start
        saveRDS(
          list(`__type__`, e, .GlobalEnv$`__dump__`),
          file = paste0(`__res__`, ".error")
        )                               # nocov end
      },
      list(
        "__type__" = error,
        "__res__" = res
      )
    )
  } else {
    throw(new_error("Unknown `error` argument: `", error, "`"))
  }

  if (messages) {
    message <- function() {
      substitute({
        pxlib <- as.environment("tools:callr")$`__callr_data__`$pxlib
        if (is.null(e$code)) e$code <- "301"
        msg <- paste0("base64::", pxlib$base64_encode(serialize(e, NULL)))
        data <- paste0(e$code, " ", nchar(msg), "\n", msg)
        pxlib$write_fd(3L, data)

        if (inherits(e, "cli_message") &&
            !is.null(findRestart("cli_message_handled"))) {
          invokeRestart("cli_message_handled")
        } else if (inherits(e, "message") &&
                   !is.null(findRestart("muffleMessage"))) {
          invokeRestart("muffleMessage")
        }
      })
    }
  } else {
    message <- function() substitute(signalCondition(e))
  }

  ## The function to run and its arguments are saved as a list:
  ## list(fun, args). args itself is a list.
  ## So the first do.call will create the call: do.call(fun, args)
  ## The second do.call will perform fun(args).
  ##
  ## The c() is needed because the first .GlobalEnv is itself
  ## an argument to the do.call within the do.call.
  ##
  ## It is important that we do not create any temporary variables,
  ## the function is called from an empty global environment.
  substitute(
     {
      tryCatch(                         # nocov start
        withCallingHandlers(
          {
            `__pre_hook__`
            saveRDS(
              do.call(
                do.call,
                c(readRDS(`__expr_file__`), list(envir = .GlobalEnv, quote = TRUE)),
                envir = .GlobalEnv,
                quote = TRUE
              ),
              file = `__res__`
            )
            flush(stdout())
            flush(stderr())
            `__post_hook__`
            invisible()
          },
          error = function(e) { `__error__` },
          interrupt = function(e) { `__error__` },
          callr_message = function(e) { try(`__message__`) }
        ),

        ## We need to `stop()` here again, otherwise the error message
        ## is not printed to stderr. See
        ## https://github.com/r-lib/callr/issues/80
        ## However, on R 3.1 and R 3.2 throwing an error here
        ## will crash the R process. With `try()` the error is still
        ## printed to stderr, but no real error is thrown.
        error = function(e) { `__post_hook__`; try(stop(e)) },
        interrupt = function(e) {  `__post_hook__`; e }
      )                                 # nocov end
    },

    list(`__error__` = err, `__expr_file__` = expr_file, `__res__` = res,
         `__pre_hook__` = pre_hook, `__post_hook__` = post_hook,
         `__message__` = message())
  )
}

make_vanilla_script_file <- function(expr_file, res, error) {
  expr <- make_vanilla_script_expr(expr_file, res, error)
  script <- deparse(expr)

  tmp <- tempfile("callr-scr-")
  cat(script, file = tmp, sep = "\n")
  tmp
}
