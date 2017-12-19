
context("io")

test_that("Output and error are discarded by default", {

  px <- get_tool("px")
  p <- process$new(px, c("outln", "foobar"))
  on.exit(try_silently(p$kill(grace = 0)), add = TRUE)

  expect_error(p$read_output_lines(n=1),  "not a pipe")
  expect_error(p$read_all_output_lines(), "not a pipe")
  expect_error(p$read_all_output(),       "not a pipe")
  expect_error(p$read_error_lines(n=1),   "not a pipe")
  expect_error(p$read_all_error_lines(),  "not a pipe")
  expect_error(p$read_all_error(),        "not a pipe")
})

test_that("We can get the output", {

  px <- get_tool("px")

  p <- process$new(px, c("out", "foo\nbar\nfoobar\n"),
                   stdout = "|", stderr = "|")
  on.exit(try_silently(p$kill(grace = 0)), add = TRUE)

  out <- p$read_all_output_lines()
  expect_identical(out, c("foo", "bar", "foobar"))
})

test_that("We can get the error stream", {

  tmp <- tempfile(fileext = ".bat")
  on.exit(unlink(tmp), add = TRUE)

  cat(">&2 echo hello", ">&2 echo world", sep = "\n", file = tmp)
  Sys.chmod(tmp, "700")

  p <- process$new(tmp, stderr = "|")
  on.exit(try_silently(p$kill(grace = 0)), add = TRUE)

  out <- sort(p$read_all_error_lines())
  expect_identical(out, c("hello", "world"))
})

test_that("Output & error at the same time", {

  tmp <- tempfile(fileext = ".bat")
  on.exit(unlink(tmp), add = TRUE)

  cat(
    if (os_type() == "windows") "@echo off",
    ">&2 echo hello",
    "echo wow",
    ">&2 echo world",
    "echo wooow",
    sep = "\n", file = tmp
  )
  Sys.chmod(tmp, "700")

  p <- process$new(tmp, stdout = "|", stderr = "|")
  on.exit(try_silently(p$kill(grace = 0)), add = TRUE)

  out <- p$read_all_output_lines()
  expect_identical(out, c("wow", "wooow"))

  err <- p$read_all_error_lines()
  expect_identical(err, c("hello", "world"))
})

test_that("Output and error to specific files", {

  tmp <- tempfile(fileext = ".bat")
  on.exit(unlink(tmp), add = TRUE)

  cat(
    if (os_type() == "windows") "@echo off",
    ">&2 echo hello",
    "echo wow",
    ">&2 echo world",
    "echo wooow",
    sep = "\n", file = tmp
  )
  Sys.chmod(tmp, "700")

  tmpout <- tempfile()
  tmperr <- tempfile()

  p <- process$new(tmp, stdout = tmpout, stderr = tmperr)
  on.exit(try_silently(p$kill(grace = 0)), add = TRUE)

  p$wait()

  ## In theory this is a race condition, because the OS might be still
  ## writing the files. But it is hard to wait until they are done.
  ## We'll see if this fails in practice, hopefully not.
  expect_identical(readLines(tmpout), c("wow", "wooow"))
  expect_identical(readLines(tmperr), c("hello", "world"))
})

test_that("is_incomplete", {

  px <- get_tool("px")
  p <- process$new(px, c("out", "foo\nbar\nfoobar\n"), stdout = "|")

  expect_true(p$is_incomplete_output())

  p$read_output_lines(n = 1)
  expect_true(p$is_incomplete_output())

  p$read_all_output_lines()
  expect_false(p$is_incomplete_output())
})

test_that("readChar on IO, unix", {

  ## Need to skip, because of the different EOL character
  skip_other_platforms("unix")

  px <- get_tool("px")

  p <- process$new(px, c("outln", "hello world!"), stdout = "|")
  p$wait()

  p$poll_io(-1)
  expect_equal(p$read_output(5), "hello")
  expect_equal(p$read_output(5), " worl")
  expect_equal(p$read_output(5), "d!\n")
})

test_that("readChar on IO, windows", {

  ## Need to skip, because of the different EOL character
  skip_other_platforms("windows")

  px <- get_tool("px")
  p <- process$new(px, c("outln", "hello world!"), stdout = "|")
  p$wait()

  p$poll_io(-1)
  expect_equal(p$read_output(5), "hello")
  p$poll_io(-1)
  expect_equal(p$read_output(5), " worl")
  p$poll_io(-1)
  expect_equal(p$read_output(5), "d!\r\n")
})
