
context("stress test")

test_that("can start 100 processes quickly", {
  skip_on_cran()
  px <- get_tool("px")
  expect_error(for (i in 1:100) run(px), NA)
})

test_that("run() a lot of times, with small timeouts", {
  skip_on_cran()
  px <- get_tool("px")
  for (i in 1:100) {
    tic <- Sys.time()
    err <- tryCatch(
      run(px, c("sleep", "5"), timeout = 1/1000),
      error = identity
    )
    expect_s3_class(err, "system_command_timeout_error")
    expect_true(Sys.time() - tic < as.difftime(3, units = "secs"))
  }
})

test_that("run() and kill while polling", {
  skip_on_cran()
  px <- get_tool("px")
  for (i in 1:10) {
    tic <- Sys.time()
    err <- tryCatch(
      run(px, c("sleep", "5"), timeout = 1/2),
      error = identity
    )
    expect_s3_class(err, "system_command_timeout_error")
    expect_true(Sys.time() - tic < as.difftime(3, units = "secs"))
  }
})

test_that("restart a lot of times", {
  skip_on_cran()

  px <- get_tool("px")

  for (i in 1:100) {
    p <- process$new(px, c("sleep", "5"))
    on.exit(p$kill(), add = TRUE)
    expect_true(p$is_alive())

    p$kill()

    expect_false(p$is_alive())

    p$restart()
    expect_true(p$is_alive())

    p$kill()
  }
})
