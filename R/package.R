
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

## We save this as an RDS, so it can be loaded quickly
env <- new.env(parent = emptyenv())
env$`__callr_data__` <- new.env(parent = emptyenv())
env$`__callr_data__`$err <- err
saveRDS(
  env,
  file = file.path(system.file(package = "callr"), "env.rds"),
  version = 2)
rm(env)
