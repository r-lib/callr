
context("poll multiple processes")

test_that("single process", {

  px <- get_tool("px")
  p <- process$new(px, c("sleep", "1", "outln", "foo", "outln", "bar"),
                   stdout = "|")
  on.exit(p$kill(), add = TRUE)

  ## Timeout
  expect_equal(
    poll(list(p), 0),
    list(c(output = "timeout", error = "nopipe"))
  )

  p$wait()
  expect_equal(
    poll(list(p), -1),
    list(c(output = "ready", error = "nopipe"))
  )

  p$read_output_lines()
  expect_equal(
    poll(list(p), -1),
    list(c(output = "ready", error = "nopipe"))
  )

  p$kill()
  expect_equal(
    poll(list(p), -1),
    list(c(output = "ready", error = "nopipe"))
  )

  close(p$get_output_connection())
  expect_equal(
    poll(list(p), -1),
    list(c(output = "closed", error = "nopipe"))
  )
})

test_that("multiple processes", {

  px <- get_tool("px")
  cmd1 <- c("sleep", "1", "outln", "foo", "outln", "bar")
  cmd2 <- c("sleep", "2", "errln", "foo", "errln", "bar")

  p1 <- process$new(px, cmd1, stdout = "|")
  p2 <- process$new(px, cmd2, stderr = "|")

  ## Timeout
  res <- poll(list(p1 = p1, p2 = p2), 0)
  expect_equal(
    res,
    list(
      p1 = c(output = "timeout", error = "nopipe"),
      p2 = c(output = "nopipe", error = "timeout")
    )
  )

  p1$wait()
  res <- poll(list(p1 = p1, p2 = p2), -1)
  expect_equal(res$p1, c(output = "ready", error = "nopipe"))
  expect_equal(res$p2[["output"]], "nopipe")
  expect_true(res$p2[["error"]] %in% c("silent", "ready"))

  close(p1$get_output_connection())
  p2$wait()
  res <- poll(list(p1 = p1, p2 = p2), -1)
  expect_equal(
    res,
    list(
      p1 = c(output = "closed", error = "nopipe"),
      p2 = c(output = "nopipe", error = "ready")
    )
  )

  close(p2$get_error_connection())
  res <- poll(list(p1 = p1, p2 = p2), 0)
  expect_equal(
    res,
    list(
      p1 = c(output = "closed", error = "nopipe"),
      p2 = c(output = "nopipe", error = "closed")
    )
  )

})

test_that("multiple polls", {

  px <- get_tool("px")
  cmd <- c("sleep", "1", "outln", "foo", "sleep", "1", "outln", "bar")
  p <- process$new(px, cmd, stdout = "|", stderr = "|")

  out <- character()
  while (p$is_alive()) {
    poll(list(p), 2000)
    out <- c(out, p$read_output_lines())
  }

  expect_identical(out, c("foo", "bar"))
})

test_that("polling and buffering", {

  px <- get_tool("px")

  for (i in 1:10) {

    ## We set up two processes, one produces a output, that we do not
    ## read out from the cache. The other one does not produce output.
    p1 <- process$new(px, c(rbind("outln", 1:20), "sleep", "3"), stdout = "|", stderr = "|")
    p2 <- process$new(px, c("sleep", "3"), stdout = "|", stderr = "|")

    ## We poll until p1 has output. We read out some of the output,
    ## and leave the rest in the buffer.
    p1$poll_io(-1)
    expect_equal(p1$read_output_lines(n = 1), "1")

    ## Now poll should return immediately, because there is output ready
    ## from p1. The status of p2 should be 'silent' (and not 'timeout')
    tick <- Sys.time()
    s <- poll(list(p1, p2), 3000)
    expect_equal(
      s,
      list(
        c(output = "ready", error = "silent"),
        c(output = "silent", error = "silent")
      )
    )
    if (s[[2]][1] != "silent") break;

    p1$kill()
    p2$kill()

    ## Check that poll has returned immediately
    dt <- Sys.time() - tick
    expect_true(dt < as.difftime(2, units = "secs"))
  }
})

test_that("polling and buffering #2", {

  px <- get_tool("px")

  ## We run this a bunch of times, because it used to fail
  ## non-deterministically on the CI
  for (i in 1:10) {

    ## Two processes, they both produce output. For the first process,
    ## we make sure that there is something in the buffer.
    ## For the second process we need to poll, but data should be
    ## available immediately.
    p1 <- process$new(px, rbind("outln", 1:20), stdout = "|")
    p2 <- process$new(px, rbind("outln", 21:30), stdout = "|")

    ## We poll until p1 has output. We read out some of the output,
    ## and leave the rest in the buffer.
    p1$poll_io(-1)
    expect_equal(p1$read_output_lines(n = 1), "1")

    ## We also need to poll p2, to make sure that there is
    ## output from it. But we don't read anything from it.
    expect_equal(p1$poll_io(-1)[["output"]], "ready")
    expect_equal(p2$poll_io(-1)[["output"]], "ready")

    ## Now poll should return ready for both processes, and it should
    ## return fast.
    tick <- Sys.time()
    s <- poll(list(p1, p2), 3000)
    expect_equal(
      s,
      list(
        c(output = "ready", error = "nopipe"),
        c(output = "ready", error = "nopipe")
      )
    )

    p1$kill()
    p2$kill()

    ## Check that poll has returned immediately
    expect_true(Sys.time() - tick < as.difftime(2, units = "secs"))
  }
})
