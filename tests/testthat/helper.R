
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

test_paths <- function(drop, keep) {
  rbin <- setup_r_binary_and_args(list())$bin
  rbin <- shQuote(rbin)

  f1 <- function(rbin) {
    system(paste(rbin, "-q -e \".libPaths()\""), intern = TRUE)
  }

  fvanilla <- function(rbin) {
    system(paste(rbin, "--vanilla -q -e \".libPaths()\""), intern = TRUE)
  }

  expect_equal(
    callr::r(function() normalizePath(.libPaths()), libpath = keep),
    unique(normalizePath(c(keep, .Library.site, .Library))))

  out <- callr::r(f1, list(rbin = rbin), libpath = keep)
  expect_true(any(grepl(basename(normalizePath(keep)), out)))
  expect_false(any(grepl(basename(normalizePath(drop)), out)))

  outvanilla <- callr::r(fvanilla, list(rbin = rbin), libpath = keep)
  expect_true(any(grepl(basename(normalizePath(keep)), outvanilla)))
  expect_false(any(grepl(basename(normalizePath(drop)), outvanilla)))
}

skip_if_offline <- function(host = "httpbin.org", port = 80) {

  res <- tryCatch(
    pingr::ping_port(host, count = 1L, port = port),
    error = function(e) NA
  )

  if (is.na(res)) skip("No internet connection")
}
