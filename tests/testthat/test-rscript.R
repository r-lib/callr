
test_that("rscript", {
  out <- rscript("fixtures/script.R", show = FALSE)
  expect_equal(out$status, 0L)
  expect_equal(
    out$stdout,
    if (os_platform() == "windows") "stdout\r\n" else "stdout\n")
  expect_equal(
    out$stderr,
    if (os_platform() == "windows") "stderr\r\n" else "stderr\n")
  gc()
})

test_that("rscript_process", {
  px  <- rscript_process$new(
    rscript_process_options(script = "fixtures/script.R"))
  on.exit(try(px$kill(), silent = TRUE), add = TRUE)
  px$wait(5000)

  expect_equal(px$get_exit_status(), 0)
  expect_equal(px$read_output_lines(), "stdout")
  expect_equal(px$read_error_lines(), "stderr")
  rm(px); gc()
})

test_that("stderr -> stdout", {
  out <- rscript("fixtures/script2.R", show = FALSE, stderr = "2>&1")
  exp <- if (os_platform() == "windows") {
    "out1err1out2err2\r\n"
  } else {
    "out1err1out2err2\n"
  }
  expect_equal(out$stdout, exp)
  expect_null(out$stderr)

  out2 <- test_temp_file(create = FALSE)
  rscript("fixtures/script2.R", show = FALSE, stdout = out2, stderr = out2)
  expect_equal(readLines(out2), "out1err1out2err2")
  gc()
})

test_that("cleans up temporary files", {

  skip_on_cran()

  rsc <- function() {
    library(callr)
    scriptfile <- tempfile(fileext = ".R")
    cat("cat('foobar')\n", file = scriptfile)

    old <- dir(tempdir(), pattern = "^callr-")

    result <- callr::rscript(scriptfile)

    new <- setdiff(dir(tempdir(), "^callr-"), old)

    list(result = result, new = new)
  }

  out <- r(rsc)
  expect_identical(out$result$stdout, "foobar")
  expect_identical(out$new, character())
})

test_that("bg process cleans up temporary files", {

  skip_on_cran()

  rsc <- function() {
    library(callr)
    scriptfile <- tempfile(fileext = ".R")
    cat("cat('foobar')\n", file = scriptfile)

    old <- dir(tempdir(), pattern = "^callr-")

    opts <- rscript_process_options(script = scriptfile)
    rp <- rscript_process$new(opts)
    on.exit(tryCatch(rp$kill, error = function(e) NULL), add = TRUE)
    rp$wait(5000)
    result <- rp$read_output()
    rp$kill()

    rm(rp)
    gc()
    gc()
    new <- setdiff(dir(tempdir(), "^callr-"), old)

    list(result = result, new = new)
  }

  out <- r(rsc)
  expect_identical(out$result, "foobar")
  expect_identical(out$new, character())
})
