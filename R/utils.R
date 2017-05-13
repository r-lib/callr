
win2unix <- function(str) {
  gsub("\r\n", "\n", str, fixed = TRUE)
}

read_char <- function(path, ...) {
  readChar(path, nchars = file.info(path)$size, useBytes = TRUE, ...)
}

is.named <- function(x) {
  length(names(x)) == length(x) && all(names(x) != "")
}

set_envvar <- function(envs) {
  if (length(envs) == 0) return()

  stopifnot(is.named(envs))

  old <- Sys.getenv(names(envs), names = TRUE, unset = NA)
  set <- !is.na(envs)

  both_set <- set & !is.na(old)

  if (any(set))  do.call("Sys.setenv", as.list(envs[set]))
  if (any(!set)) Sys.unsetenv(names(envs)[!set])

  invisible(old)
}

with_envvar <- function(new, code) {
  old <- set_envvar(new)
  on.exit(set_envvar(old))
  force(code)
}

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

os_platform <- function() .Platform$OS.type

try_silently <- function(expr) try(expr, silent = TRUE)

enumerate <- function(x) {
  if (length(x) == 0) {
    ""
  } else if (length(x) == 1) {
    x
  } else {
    l <- length(x)
    paste0(paste(x[-l], collapse = ", "), " and ", x[[l]])
  }
}
