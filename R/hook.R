
common_hook <- function() {
  substitute({
    # This should not happen in a new R session, but just to be safe
    while ("tools:callr" %in% search()) detach("tools:callr")
    env <- readRDS(`__envfile__`)
    do.call("attach", list(env, pos = length(search()), name = "tools:callr"))
    data <- env$`__callr_data__`
    data$pxlib <- data$load_client_lib(data$sofile[[paste0("arch-", .Platform$r_arch)]])
    options(error = function() invokeRestart("abort"))
    rm(list = c("data", "env"))

    lapply(
      c("R_ENVIRON", "R_ENVIRON_USER", "R_PROFILE", "R_PROFILE_USER",
        "R_LIBS", "R_LIBS_USER", "R_LIBS_SITE"),
      function(var) {
        bakvar <- paste0("CALLR_", var, "_BAK")
        val <- Sys.getenv(bakvar, NA_character_)
        if (!is.na(val)) {
          do.call("Sys.setenv", structure(list(val), names = var))
        } else {
          Sys.unsetenv(var)
        }
        Sys.unsetenv(bakvar)
      }
    )

    Sys.unsetenv("CALLR_CHILD_R_LIBS")
    Sys.unsetenv("CALLR_CHILD_R_LIBS_SITE")
    Sys.unsetenv("CALLR_CHILD_R_LIBS_USER")

  }, list("__envfile__" = env_file))
}

default_load_hook <- function(user_hook = NULL) {
  prepare_client_files()
  hook <- common_hook()

  if (!is.null(user_hook)) {
    hook <- substitute({ d; u }, list(d = hook, u = user_hook))
  }
  paste0(deparse(hook), "\n")
}

session_load_hook <- function(user_hook = NULL) {
  chook <- common_hook()
  ehook <- substitute({
    data <- as.environment("tools:callr")$`__callr_data__`
    data$pxlib$disable_fd_inheritance()
    rm(data)
  })

  hook <- substitute({ c; e }, list(c = chook, e = ehook))

  if (!is.null(user_hook)) {
    hook <- substitute({ d; u }, list(d = hook, u = user_hook))
  }

  hook <- substitute({
    err_ <- TRUE
    callr_startup_hook <- function() {
      on.exit(if (err_) quit("no", 1, TRUE))
      { `_hook_` }
      err_ <<- FALSE
    }
    callr_startup_hook()
    rm(err_, callr_startup_hook)
  }, list("_hook_" = hook))

  paste0(deparse(hook), "\n")
}
