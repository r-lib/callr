
make_vanilla_script <- function(expr_file, res, error) {

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
  }

  ## The function to run and its arguments are saved as list:
  ## list(fun, args). args itself is a list.
  ## So the first do.call will create the call: do.call(fun, args)
  ## This second do.call will perform fun(args).
  ##
  ## The c() is needed because the first .GlobalEnv is itself
  ## an argument to the do.call within the do.call.
  ##
  ## It is important that we do not create any temporary variables,
  ## the function is called from an empty global environment.
  script <- substitute(
    {
      withCallingHandlers(              # nocov start
        {
          saveRDS(
            do.call(
              do.call,
              c(readRDS(`__expr_file__`), list(envir = .GlobalEnv)),
              envir = .GlobalEnv
            ),
            file = `__res__`
          )
        },
        error = function(e) { `__error__`; stop(e) }
      )                                 # nocov end
    },

    list(`__error__` = err, `__expr_file__` = expr_file, `__res__` = res)
  )

  script <- deparse(script)

  tmp <- tempfile()
  cat(script, file = tmp, sep = "\n")
  tmp
}
