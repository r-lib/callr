
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
  env$`__callr_data__` <- new.env(parent = emptyenv())
  env$`__callr_data__`$err <- err

  arch <- .Platform$r_arch
  ext <- .Platform$dynlib.ext
  sofile <- system.file(
    paste0("libs", arch), paste0("client", ext),
    package = "processx")

  # Try this as well, this is for devtools/pkgload
  if (sofile == "") {
    sofile <- system.file(
      "src", paste0("client", ext),
      package = "processx")
  }

  # stop() here and not throw(), because this function should be standalone
  if (sofile == "") stop("Cannot find client file")

  # not only creates a new default value, but drops the bytecode,
  # which might have a reference to the processx package env, which we
  # want to avoid
  lcl <- asNamespace("processx")$load_client_lib
  fml <- formals(lcl)
  fml$sofile <- sofile
  formals(lcl) <- fml
  env$`__callr_data__`$processx_loader <- lcl

  env_file <<- tempfile()
  saveRDS(env, file = env_file, version = 2, compress = FALSE)
  invisible()
}
