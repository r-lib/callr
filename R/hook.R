
common_hook <- function() {
  substitute({
    # This should not happen in a new R session, but just to be safe
    while ("tools:callr" %in% search()) detach("tools:callr")
    env <- readRDS(`__envfile__`)
    do.call("attach", list(env, pos = length(search()), name = "tools:callr"))
    data <- env$`__callr_data__`
    data$pxlib <- data$processx_loader()
    data$pxlib$disable_fd_inheritance()
    options(error = function() invokeRestart("abort"))
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
  hook <- common_hook()
  if (!is.null(user_hook)) {
    hook <- substitute({ d; u }, list(d = hook, u = user_hook))
  }
  paste0(deparse(hook), "\n")
}
