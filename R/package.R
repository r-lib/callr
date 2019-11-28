
#' Call R from R
#'
#' It is sometimes useful to perform a computation in a separate R
#' process, without affecting the current R process at all. This packages
#' does exactly that.
#'
#' @docType package
#' @name callr
NULL

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
  client_file <- system.file("client.R", package = "callr")
  if (client_file == "") stop("Cannot find client R file")

  source(
    client_file, local = env$`__callr_data__`,
    keep.source = FALSE)

  arch <- .Platform$r_arch
  ext <- .Platform$dynlib.ext
  sofile <- system.file(
    "libs", arch, paste0("client", ext),
    package = "processx")

  # Maybe not multi-arch build on a multi-arch system?
  # Can this happent at all?
  if (sofile == "") {
    sofile <- system.file(
      "libs", paste0("client", ext),
      package = "processx")
  }

  # Try this as well, this is for devtools/pkgload
  if (sofile == "") {
    sofile <- system.file(
      "src", paste0("client", ext),
      package = "processx")
  }

  # stop() here and not throw(), because this function should be standalone
  if (sofile == "") stop("Cannot find client file")

  env$`__callr_data__`$sofile <- sofile

  env_file <<- tempfile("callr-env-")
  saveRDS(env, file = env_file, version = 2, compress = FALSE)
  invisible()
}

.onUnload <- function(libpath) {
  if (!is.null(env_file)) unlink(env_file)
}
