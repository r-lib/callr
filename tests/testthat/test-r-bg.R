
test_that("r_bg runs", {
  x <- r_bg(function() 1 + 1)
  x$wait()
  expect_identical(x$get_result(), 2)
  rm(x)
  gc()
})

test_that("r_bg takes arguments", {
  x <- r_bg(function(x) x + 10, args = list(32))
  x$wait()
  expect_identical(x$get_result(), 42)
  rm(x)
  gc()
})

test_that("r_bg can be killed", {
  x <- r_bg(function() Sys.sleep(2))
  x$kill()
  expect_false(x$is_alive())
  expect_error(x$get_result())
  rm(x)
  gc()
})

test_that("r_bg can get the error back", {
  x <- r_bg(function() 1 + "A", error = "error")
  x$wait()
  expect_error(x$get_result(), "non-numeric argument to binary operator")
  rm(x)
  gc()
})

test_that("can read standard output", {
  x <- r_bg(function() cat("Hello world!\n"))
  x$wait()
  expect_equal(x$read_output_lines(), "Hello world!")
  rm(x)
  gc()
})

test_that("can read standard error", {
  x <- r_bg(function() message("Hello world!"))
  x$wait()
  expect_equal(x$read_error_lines(), "Hello world!")
  rm(x)
  gc()
})

test_that("can read stdout and stderr", {
  x <- r_bg(function() { cat("Hello world!\n"); message("Again!") })
  x$wait()
  expect_equal(x$read_output_lines(), "Hello world!")
  expect_equal(x$read_error_lines(), "Again!")
  rm(x)
  gc()
})

test_that("cleans up temporary files", {

  skip_on_cran()

  rbg <- function() {
    library(callr)
    old <- dir(tempdir(), pattern = "^callr-")
    rp <- callr::r_bg(function() 1+1)
    on.exit(tryCatch(rp$kill, error = function(e) NULL), add = TRUE)
    rp$wait(5000)
    rp$kill()
    result <- rp$get_result()

    rm(rp)
    gc()
    gc()

    unloadNamespace("callr")
    new <- setdiff(dir(tempdir(), "^callr-"), old)

    list(result = result, new = new)
  }

  out <- r(rbg)
  expect_identical(out$result, 2)
  expect_identical(out$new, character())
})
