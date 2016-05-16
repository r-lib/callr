
make_vanilla_script <- function(expr_file, res) {

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

  script <- quote({
    saveRDS(
      do.call(
        do.call,
        c(readRDS("{expr_file}"), list(envir = .GlobalEnv)),
        envir = .GlobalEnv
      ),
      file = "{res}"
    )
  })

  script <- deparse(script)
  script <- gsub("\n", ";", script, fixed = TRUE)

  tmp <- tempfile()
  script <- gsub("{expr_file}", expr_file, script, fixed = TRUE)
  script <- gsub("{res}", res, script, fixed = TRUE)

  cat(script, file = tmp)

  tmp
}
