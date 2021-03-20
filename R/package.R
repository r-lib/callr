
#' Call R from R
#'
#' It is sometimes useful to perform a computation in a separate R
#' process, without affecting the current R process at all. This packages
#' does exactly that.
#'
#' @name callr
"_PACKAGE"

## R CMD check workaround
dummy_r6 <- function() R6::R6Class

clients <- NULL
sofiles <- NULL
env_file <- NULL

## We save this as an RDS, so it can be loaded quickly
.onLoad <- function(libname, pkgname) {
  err$onload_hook()
  env_file <<- tempfile("callr-env-")
  clients <<- asNamespace("processx")$client
  sofiles <<- get_client_files()
  client_env$`__callr_data__`$sofile <- sofiles
}

prepare_client_files <- function() {
  for (aa in names(client_env$`__callr_data__`$sofile)) {
    fn <- client_env$`__callr_data__`$sofile[[aa]]
    if (!file.exists(fn)) writeBin(clients[[aa]]$bytes, fn)
  }

  if (!file.exists(env_file)) {
    saveRDS(client_env, file = env_file, version = 2, compress = FALSE)
  }
  invisible()
}

get_client_files <- function() {
  archs <- ls(clients)
  vapply(archs, function(aa) {
    file.path(
      tempdir(),
      paste0(
        "callr-client-",
        sub("arch-", "", aa),
        "-",
        substr(clients[[aa]]$md5, 1, 7),
        .Platform$dynlib.ext
      )
    )
  }, character(1))
}

.onUnload <- function(libpath) {
  unlink(
    normalizePath(c(sofiles, env_file), mustWork = FALSE),
    recursive = TRUE,
    force = TRUE
  )
}
