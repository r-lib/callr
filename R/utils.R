
#' Default value for the `repos` option in callr subprocesses
#'
#' callr sets the `repos` option in subprocesses, to make sure that
#' a CRAN mirror is set up. This is because the subprocess cannot bring
#' up the menu of CRAN mirrors for the user to choose from.
#'
#' @return Named character vector, the default value of the `repos`
#' option in callr subprocesses.
#'
#' @export
#' @examples
#' default_repos()

default_repos <- function() {
  opt <- getOption("repos")
  if (! "CRAN" %in% names(opt) || opt[["CRAN"]] == "@CRAN@") {
    opt[["CRAN"]] <- "https://cloud.r-project.org"
  }
  opt
}

`%||%` <- function(l, r) if (is.null(l)) r else l

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

## Thanks to Romain for the idea!
## https://github.com/romainfrancois/trump/blob/
## 7845b83343afa356e4259c054e7c9a910034f170/R/trump.R

crash <- function() {
  get("attach")( structure(list(), class = "UserDefinedDatabase")  )
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

is_string <- function(x) {
  is.character(x) &&
  length(x) == 1 &&
  !is.na(x)
}

read_all <- function(filename) {
  con <- file(filename, open = "rb")
  on.exit(close(con), add = TRUE)
  res <- raw(0)
  while (length(more <- readBin(con, what = "raw", 10000)) && length(more)) {
    res <- c(res, more)
  }
  rawToChar(res)
}

is_complete_expression <- function(x) {
  err <- NULL
  tryCatch(parse(text = x), error = function(e) err <<- e)
  if (is.null(err)) return(TRUE)
  exp <- tryCatch(parse(text = "1+"), error = function(e) e$message)
  exp1 <- strsplit(exp, "\n")[[1]][[1]]
  msg <- sub("^.*:\\s*([^:]+)$",  "\\1", exp1, perl = TRUE)
  ! grepl(msg, conditionMessage(err), fixed = TRUE)
}

bold <- function(x) {
  tryCatch(
    get("bold", asNamespace("crayon"))(x),
    error = function(e) x)
}

update_history <- function(cmd) {
  tmp <- tempfile("callr-hst-")
  on.exit(unlink(tmp, recursive = TRUE))
  utils::savehistory(tmp)
  cat(cmd, "\n", sep = "", file = tmp, append = TRUE)
  utils::loadhistory(tmp)
}
