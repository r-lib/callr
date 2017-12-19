
#' @importFrom assertthat assert_that on_failure<-
NULL

is_string <- function(x) {
  is.character(x) &&
  length(x) == 1 &&
  !is.na(x)
}

on_failure(is_string) <- function(call, env) {
  paste0(deparse(call$x), " is not a string (length 1 character)")
}

is_string_or_null <- function(x) {
  is.null(x) || is_string(x)
}

on_failure(is_string_or_null) <- function(call, env) {
  paste0(deparse(call$x), " must be a string (length 1 character) or NULL")
}

is_flag <- function(x) {
  is.logical(x) &&
  length(x) == 1 &&
  !is.na(x)
}

on_failure(is_flag) <- function(call, env) {
  paste0(deparse(call$x), " is not a flag (length 1 logical)")
}

is_integerish_scalar <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && round(x) == x
}

on_failure(is_integerish_scalar) <- function(call, env) {
  paste0(deparse(call$x), " is not a length 1 integer")
}

is_pid <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && round(x) == x
}

on_failure(is_pid) <- function(call, env) {
  paste0(deparse(call$x), " is not a process id (length 1 integer)")
}

is_flag_or_string <- function(x) {
  is_string(x) || is_flag(x)
}

on_failure(is_flag_or_string) <- function(call, env) {
  paste0(deparse(call$x), " is not a flag or a string")
}

is_existing_file <- function(x) {
  is_string(x) && file.exists(x)
}

on_failure(is_existing_file) <- function(call, env) {
  paste0("File ", deparse(call$x), " does not exist")
}

is_time_interval <- function(x) {
  (inherits(x, "difftime") && length(x) == 1) ||
    (is.numeric(x) && length(x) == 1 && !is.na(x))
}

on_failure(is_time_interval) <- function(call, env) {
  paste0(deparse(call$x), " is not a valid time interval")
}

is_list_of_processes <- function(x) {
  is.list(x) && vapply(x, inherits, FUN.VALUE = logical(1), "process")
}

on_failure(is_list_of_processes) <- function(call, env) {
  paste0(deparse(call$x), " is not a list of process objects")
}
