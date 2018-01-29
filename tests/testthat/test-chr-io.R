
context("character IO")

test_that("Can read last line without trailing newline", {

  px <- get_tool("px")

  p <- process$new(px, c("out", "foobar"), stdout = "|")
  out <- p$read_all_output_lines()
  expect_equal(out, "foobar")
})

test_that("Can read single characters", {

  px <- get_tool("px")

  p <- process$new(px, c("out", "123"), stdout = "|")
  p$wait()

  p$poll_io(-1)
  expect_equal(p$read_output(1), "1")
  expect_equal(p$read_output(1), "2")
  expect_equal(p$read_output(1), "3")
  expect_equal(p$read_output(1), "")
  expect_false(p$is_incomplete_output())
})

test_that("Can read multiple characters", {

  px <- get_tool("px")

  p <- process$new(px, c("out", "123456789"), stdout = "|")
  p$wait()

  p$poll_io(-1)
  expect_equal(p$read_output(3), "123")
  expect_equal(p$read_output(4), "4567")
  expect_equal(p$read_output(2), "89")
  expect_equal(p$read_output(1), "")
  expect_false(p$is_incomplete_output())
})
