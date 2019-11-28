
context("rcmd_process")

test_that("create and run rcmd_process", {
  opts <- rcmd_process_options(cmd = "config", cmdargs = "CC")
  x <- rcmd_process$new(opts)
  x$wait()
  expect_equal(x$get_exit_status(), 0)
  out <- x$read_output_lines()
  expect_match(out, ".")
  rm(x)
  gc()
})

test_that("cleans up temp files", {

  skip_on_cran()

  rc <- function() {
    library(callr)
    scriptfile <- tempfile(fileext = ".R")

    old <- dir(tempdir(), pattern = "^callr-")

    rp <- rcmd_bg("config", "CC")
    on.exit(tryCatch(rp$kill, error = function(e) NULL), add = TRUE)
    rp$wait(5000)
    result <- rp$get_exit_status()
    rp$kill()

    rm(rp)
    gc()
    gc()
    new <- setdiff(dir(tempdir(), "^callr-"), old)

    list(result = result, new = new)
  }

  out <- r(rc)
  expect_identical(out$result, 0L)
  expect_identical(out$new, character())
})
