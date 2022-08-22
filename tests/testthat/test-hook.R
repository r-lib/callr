
test_that("set options with hook", {
  add_hook(
    test_hook = function (options) {
      within(options, {
        env["TEST_HOOK"] <- "hello"
        cmdargs <- c(cmdargs, "extra_arg")
      })
    }
  )

  options <- r_process_options(
    func = function() {
      list(env = Sys.getenv("TEST_HOOK"), args = commandArgs())
    }
  )
  x <- r_process$new(options)
  x$wait()
  res <- x$get_result()
  expect_equal(res$env, "hello")
  expect_length(grep("^extra_arg$", res$args), 1L)

  # Remove hook
  add_hook(test_hook = NULL)

  x <- r_process$new(options)
  x$wait()
  res <- x$get_result()
  expect_equal(res$env, "")
  expect_length(grep("^extra_arg$", res$args), 0L)
})
