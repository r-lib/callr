
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
