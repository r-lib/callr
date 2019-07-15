
common_hook <- function() {
  substitute({
    # This should not happen in a new R session, but just to be safe
    while ("tools:callr" %in% search()) detach("tools:callr")
    env <- readRDS(`__envfile__`)
    do.call("attach", list(env, pos = length(search()), name = "tools:callr"))
    data <- env$`__callr_data__`
    data$pxlib <- data$load_client_lib(data$sofile)
    options(error = function() invokeRestart("abort"))
    rm(list = c("data", "env"))
  }, list("__envfile__" = env_file))
}

default_load_hook <- function(user_hook = NULL) {
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
  paste0(deparse(hook), "\n")
}
