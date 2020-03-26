
#' Call R from R
#'
#' It is sometimes useful to perform a computation in a separate R
#' process, without affecting the current R process at all. This packages
#' does exactly that.
#'
#' @theme assets/extra.css assets/rd.js
#' @name callr
"_PACKAGE"

## R CMD check workaround
dummy_r6 <- function() R6::R6Class

env_file <- NULL

## We save this as an RDS, so it can be loaded quickly
.onLoad <- function(libname, pkgname) {
  env <- new.env(parent = emptyenv())
  env$`__callr_data__` <- new.env(parent = baseenv())

  err$onload_hook()

  # We need some R code in the subprocess, we parse it here, so the
  # subprocess just needs to load it. This code will also load the
  # shared lib of the compiled functions that we need.

  client_file <- file.path(libname, pkgname, "client.R")
  if (!file.exists(client_file)) {
    client_file <- file.path(libname, pkgname, "inst", "client.R")
  }
  # This is for compatibility with current CRAN pak (0.1.2)
  if (!file.exists(client_file)) {
    client_file <- system.file("client.R", package = "callr")
  }
  if (client_file == "" || !file.exists(client_file)) {
    stop("Cannot find client R file")
  }

  source(
    client_file, local = env$`__callr_data__`,
    keep.source = FALSE)

  # An env var can override the location of the client lib
  # We also unset the env var here, so sub-sub-processes are
  # not affected by it. If they should, then the subprocess needs
  # to set this up again.
  px <- Sys.getenv("CALLR_PROCESSX_CLIENT_LIB", "")
  Sys.unsetenv("CALL_PROCESSX_CLIENT_LIB")
  if (px == "") px <- getNamespaceInfo("processx", "path")

  arch <- .Platform$r_arch
  so <- paste0("client", .Platform$dynlib.ext)
  sofile <- file.path(px, "libs", arch, so)

  # Maybe not multi-arch build on a multi-arch system?
  # Can this happen at all?
  if (!file.exists(sofile)) {
    sofile <- file.path(px, "libs", so)
  }

  # Try this as well, this is for devtools/pkgload
  if (!file.exists(sofile)) {
    sofile <- file.path("src", so)
  }

  # stop() here and not throw(), because this function should be standalone
  if (!file.exists(sofile)) stop("Cannot find client so file")

  env$`__callr_data__`$sofile <- sofile

  env_file <<- tempfile("callr-env-")
  saveRDS(env, file = env_file, version = 2, compress = FALSE)
  invisible()
}

.onUnload <- function(libpath) {
  if (!is.null(env_file)) unlink(env_file)
}
