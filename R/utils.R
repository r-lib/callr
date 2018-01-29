
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

strrep <- function(x, times) {
  x <- as.character(x)
  if (length(x) == 0L) return(x)
  r <- .mapply(
    function(x, times) {
      if (is.na(x) || is.na(times)) return(NA_character_)
      if (times <= 0L) return("")
      paste0(replicate(times, x), collapse = "")
    },
    list(x = x, times = times),
    MoreArgs = list()
  )

  unlist(r, use.names = FALSE)
}
