
common_hook <- function() {
  envfile <- normalizePath(system.file("env.rds", package = "callr"))
  substitute({
    # This should not happen in a new R session, but just to be safe
    while ("tools:callr" %in% search()) detach("tools:callr")
    env <- readRDS(`__envfile__`)
    attach(env, pos = length(search()), name = "tools:callr")
    options(error = function() invokeRestart("abort"))
  }, list("__envfile__" = envfile))
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
    get("conn_disable_inheritance", asNamespace("processx"))()
  })

  hook <- substitute({ c; e }, list(c = chook, e = ehook))

  if (!is.null(user_hook)) {
    hook <- substitute({ d; u }, list(d = hook, u = user_hook))
  }
  paste0(deparse(hook), "\n")
}
