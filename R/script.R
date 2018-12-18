
make_vanilla_script_expr <- function(expr_file, res, error,
                                     pre_hook = NULL, post_hook = NULL) {

  ## Code to handle errors in the child
  ## This will inserted into the main script
  err <- if (error == "error") {
    substitute(
      saveRDS(list("error", e), file = paste0(`__res__`, ".error")),
      list(`__res__` = res)
    )

  } else if (error %in% c("stack", "debugger")) {
    substitute(
      {
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
    stop("Unknown `error` argument: `", error, "`")
  }

  message <- function() {
    substitute({
      if (is.null(e$code)) e$code <- "301"
      msg <- paste0("base64::", processx::base64_encode(serialize(e, NULL)))
      data <- paste(e$code, msg, "\n")
      con <- processx::conn_create_fd(3, close = FALSE)
      while (1) {
        data <- processx::conn_write(con, data)
        if (!length(data)) break;
        Sys.sleep(.1)
      }
      if (!is.null(findRestart("muffleMessage"))) {
        invokeRestart("muffleMessage")
      }
    })
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
                c(readRDS(`__expr_file__`), list(envir = .GlobalEnv)),
                envir = .GlobalEnv
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

  tmp <- tempfile()
  cat(script, file = tmp, sep = "\n")
  tmp
}
