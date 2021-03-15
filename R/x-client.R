
client_env <- local({
  env <- new.env(parent = emptyenv())
  env$`__callr_data__` <- new.env(parent = baseenv())

  errfile <- file.path("R", "errors.R")
  source(errfile, local = env$`__callr_data__`, keep.source = FALSE)
  loadfile <- file.path("R", "load-client.R")
  source(loadfile, local = env$`__callr_data__`, keep.source = FALSE)

  env
})
