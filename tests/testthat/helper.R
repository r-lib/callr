
try_silently <- function(expr) {
  tryCatch(
    expr,
    error = function(x) "error",
    warning = function(x) "warning",
    message = function(x) "message"
  )
}

read_next <- function(x, timeout = 3000) {
  pr <- x$poll_process(timeout)
  if (any(pr == "ready")) {
    x$read()
  } else {
    stop("R session is not ready, timed out...")
  }
}

has_locale <- function(l) {
  has <- TRUE
  tryCatch(
    withr::with_locale(c(LC_CTYPE = l), "foobar"),
    warning = function(w) has <<- FALSE,
    error = function(e) has <<- FALSE
  )
  has
}

test_paths <- function(callr_drop, callr_keep,
                       system_drop = callr_drop, system_keep = callr_keep,
                       system_vanilla_drop = system_drop,
                       system_vanilla_keep = system_keep) {

  # env vars we manipulate and need to restore in the child process
  envs <- c("R_ENVIRON", "R_ENVIRON_USER", "R_PROFILE", "R_PROFILE_USER",
            "R_LIBS", "R_LIBS_USER", "R_LIBS_SITE")

  # env vars that should not be set, in addition to the CALLR_*_BAK ones
  xenvs <- c("CALLR_CHILD_R_LIBS", "CALLR_CHILD_R_LIBS_SITE",
             "CALLR_CHILD_R_LIBS_USER")

  check_env <- function(subenv) {
    miss <- paste0("CALLR_", envs, "_BAK")
    expect_equal(Sys.getenv()[envs], subenv[envs])
    expect_false(any(miss %in% names(subenv)))
  }

  fc <- function() {
    lib <- normalizePath(.libPaths())
    list(env = Sys.getenv(), lib = lib)
  }

  out <- callr::r(fc , libpath = callr_keep)
  expect_equal(
    out$lib,
    unique(normalizePath(c(callr_keep, .Library.site, .Library)))
  )
  check_env(out$env)

  rbin <- setup_r_binary_and_args(list())$bin
  rbin <- shQuote(rbin)

  f1 <- function(rbin) {
    lib <- system(paste(rbin, "-q -e \".libPaths()\""), intern = TRUE)
    list(env = Sys.getenv(), lib = lib)
  }

  out <- callr::r(f1, list(rbin = rbin), libpath = system_keep)
  if (length(system_keep)) {
    expect_false(any(grepl(basename(normalizePath(system_keep)), out)))
  }
  if (length(system_drop)) {
    expect_false(any(grepl(basename(normalizePath(system_drop)), out)))
  }
  check_env(out$env)

  fvanilla <- function(rbin) {
    lib <- system(paste(rbin, "--vanilla -q -e \".libPaths()\""), intern = TRUE)
    list(env = Sys.getenv(), lib = lib)
  }

  outvanilla <- callr::r(
    fvanilla, list(rbin = rbin),
    libpath = system_vanilla_keep
  )
  if (length(system_vanilla_keep)) {
    expect_false(
      any(grepl(basename(normalizePath(system_vanilla_keep)), outvanilla))
    )
  }
  if (length(system_vanilla_drop)) {
    expect_false(
      any(grepl(basename(normalizePath(system_vanilla_drop)), outvanilla))
    )
  }
  check_env(out$env)
}

test_temp_file <- function(fileext = "", pattern = "test-file-",
                           envir = parent.frame(), create = TRUE) {
  tmp <- tempfile(pattern = pattern, fileext = fileext)
  if (identical(envir, .GlobalEnv)) {
    message("Temporary files will _not_ be cleaned up")
  } else {
    withr::defer(
      try(unlink(tmp, recursive = TRUE, force = TRUE), silent = TRUE),
      envir = envir)
  }
  if (create) {
    cat("", file = tmp)
    normalizePath(tmp)
  } else {
    tmp
  }
}

test_temp_dir <- function(pattern = "test-dir-", envir = parent.frame()) {
  tmp <- test_temp_file(pattern = pattern, envir = envir, create = FALSE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  normalizePath(tmp)
}

expect_error <- function(..., class = "error") {
  testthat::expect_error(..., class = class)
}

test_package_root <- function() {
  x <- tryCatch(
    rprojroot::find_package_root_file(),
    error = function(e) NULL)

  if (!is.null(x)) return(x)

  pkg <- testthat::testing_package()
  x <- tryCatch(
    rprojroot::find_package_root_file(
      path = file.path("..", "..", "00_pkg_src", pkg)),
    error = function(e) NULL)

  if (!is.null(x)) return(x)

  stop("Cannot find package root")
}

in_covr <- function() {
  Sys.getenv("R_COVR") == "true"
}

skip_in_covr <- function() {
  if (in_covr()) skip("In covr")
}

clean_envvars <- function() {
  c(R_DEFAULT_PACKAGES = "NULL", R_ENABLE_JIT = "0")
}

without_env <- function(f) {
  environment(f) <- .GlobalEnv
  f
}

expect_r_process_snapshot <- function(..., interactive = TRUE, echo = TRUE,
                                      transform = NULL, variant = NULL) {
  # errors.R assumes non-interactive in testthat, but we don't want that
  withr::local_envvar(TESTTHAT = NA_character_)
  dots <- eval(substitute(alist(...)))
  nms <- names(dots)
  if (all(nms == "")) {
    code_pos <- rep(TRUE, length(dots))
  } else {
    code_pos <- nms == ""
  }
  code <- unlist(lapply(dots[code_pos], deparse))
  args <- dots[!code_pos]

  record_output <- asciicast::record_output
  output <- do.call(
    "record_output",
    c(list(code), args, interactive = interactive, echo = echo)
  )

  r_process <- function() writeLines(output)

  expect_snapshot(r_process(), transform = transform, variant = variant)
}

redact_srcref <- function(x) {
  sub("[ ]*at [-a-zA-Z0-9]+[.]R:[0-9]+:[0-9]+?", "", x)
}

redact_callr_rs_result <- function(x) {
  sub("done callr-rs-result-[a-f0-9]+", "done callr-rs-result-<id>", x)
}

fix_eol <- function(x) {
  gsub("\\r\\n", "\\n", x, fixed = TRUE)
}
