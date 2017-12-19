
context("waiting on processes")

test_that("no deadlock when no stdout + wait", {

  skip("failure would freeze")

  p <- process$new("seq", c("1", "100000"))
  p$wait()
})

test_that("wait with timeout", {

  px <- get_tool("px")
  p <- process$new(px, c("sleep", "3"))
  expect_true(p$is_alive())

  t1 <- proc.time()
  p$wait(timeout = 100)
  t2 <- proc.time()

  expect_true(p$is_alive())
  expect_true((t2 - t1)["elapsed"] >   50/1000)
  expect_true((t2 - t1)["elapsed"] < 3000/1000)

  p$kill()
  expect_false(p$is_alive())
})

test_that("wait after process already exited", {

  px <- get_tool("px")
  p <- process$new(
    px, c("outln", "foo", "outln", "bar", "outln", "foobar"))

  ## Make sure it is done
  p$wait()

  ## Now wait() should return immediately, regardless of timeout
  expect_true(system.time(p$wait())[["elapsed"]] < 1)
  expect_true(system.time(p$wait(3000))[["elapsed"]] < 1)
})
