
make_vanilla_script_expr <- function(expr_file, res, error,
                                     re_stdout = NULL, re_stderr = NULL) {

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

  ## stdout / stderr redirection
  if (!is.null(re_stdout)) {
    xstdout <- substitute(
      processx::conn_set_stdout(
        .__ocon__ <- processx::conn_create_file(`__fn__`, write = TRUE)),
      list(`__fn__` = re_stdout)
    )
    xstdout2 <- substitute({
      processx::conn_set_stdout(
        processx::conn_create_file(tempfile(), write = TRUE))
      close(.__ocon__);
    })
  } else {
    xstdout <- xstdout2 <- substitute(invisible())
  }

  if (!is.null(re_stderr)) {
    xstderr <- substitute(
      processx::conn_set_stderr(
        .__econ__ <- processx::conn_create_file(`__fn__`, write = TRUE)),
      list(`__fn__` = re_stderr)
    )
    xstderr2 <- substitute({
      processx::conn_set_stderr(
        processx::conn_create_file(tempfile(), write = TRUE))
      close(.__econ__);
    })
  } else {
    xstderr <- xstderr2 <- substitute(invisible())
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
            `__stdout__`
            `__stderr__`
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
            `__stdout2__`
            `__stderr2__`
          },
          error = function(e) { `__error__` },
          interrupt = function(e) { `__error__` }
        ),
        error = function(e) e,
        interrupt = function(e) e
      )                                 # nocov end
    },

    list(`__error__` = err, `__expr_file__` = expr_file, `__res__` = res,
         `__stdout__` = xstdout, `__stderr__` = xstderr,
         `__stdout2__` = xstdout2, `__stderr2__` = xstderr2)
  )
}

make_vanilla_script_file <- function(expr_file, res, error) {
  expr <- make_vanilla_script_expr(expr_file, res, error)
  script <- deparse(expr)

  tmp <- tempfile()
  cat(script, file = tmp, sep = "\n")
  tmp
}
