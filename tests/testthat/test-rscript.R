
context("rscript")

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
