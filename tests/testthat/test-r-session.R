
context("r_session")

test_that("regular use", {
  opt <- r_session_options()
  rs <- r_session$new(opt)
  on.exit(rs$kill())

  ## Wait until ready, but max 3s
  r_session_wait_or_kill(rs)

  ## Start a command
  rs$call(function() 42)
  r_session_wait_or_kill(rs)

  ## Get result
  expect_equal(rs$get_result(), 42)
  expect_equal(rs$get_state(), "idle")

  ## Run another command, with arguments
  rs$call(function(x, y)  x + y, list(x = 42, y = 42))
  r_session_wait_or_kill(rs)

  ## Get result
  expect_equal(rs$get_result(), 84)
  expect_equal(rs$get_state(), "idle")

  ## Finish
  rs$finish()
  expect_equal(rs$get_state(), "finished")
  expect_false(rs$is_alive())
})

test_that("run", {
  opt <- r_session_options()
  rs <- r_session$new(opt)
  on.exit(rs$kill())

  ## Wait until ready, but max 3s
  r_session_wait_or_kill(rs)

  expect_equal(rs$run(function() 42)$result, 42)
  expect_equal(rs$run(function() 42)$result, 42)
  expect_equal(
    rs$run(function(x, y) x + y, list(x = 42, y = 42))$result,
    84)

  ## Finish
  rs$finish()
  expect_equal(rs$get_state(), "finished")
  expect_false(rs$is_alive())
})


test_that("get stdout/stderr from file", {
  rs <- r_session$new()
  on.exit(rs$kill())

  ## Wait until ready, but max 3s
  r_session_wait_or_kill(rs)

  for (i in 1:10) {
    res <- rs$run(function() { cat("foo\n"); message("bar"); 42 })
    expect_equal(res, list(result = 42, output = "foo\n", error = "bar\n"))

    res <- rs$run(function() { cat("bar\n"); message("foo"); 43 })
    expect_equal(res, list(result = 43, output = "bar\n", error = "foo\n"))
  }
})
