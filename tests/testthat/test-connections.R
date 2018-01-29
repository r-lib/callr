
context("Connections")

test_that("lot of text", {

  px <- get_tool("px")
  txt <- strrep("x", 100000)
  cat(txt, file = tmp <- tempfile())

  p <- process$new(px, c("cat", tmp), stdout = "|")
  out <- p$read_all_output_lines()

  expect_equal(txt, out)
})

test_that("UTF-8", {

  px <- get_tool("px")
  txt <- charToRaw(strrep("\xc2\xa0\xe2\x86\x92\xf0\x90\x84\x82", 20000))
  writeBin(txt, con = tmp <- tempfile())

  p <- process$new(px, c("cat", tmp), stdout = "|", encoding = "UTF-8")
  out <- p$read_all_output_lines()

  expect_equal(txt, charToRaw(out))
})

test_that("UTF-8 multibyte character cut in half", {

  px <- get_tool("px")

  rtxt <- charToRaw("a\xc2\xa0a")

  writeBin(rtxt[1:2], tmp1 <- tempfile())
  writeBin(rtxt[3:4], tmp2 <- tempfile())

  p <- process$new(px, c("cat", tmp1, "cat", tmp2), stdout = "|",
                   encoding = "UTF-8")
  out <- p$read_all_output_lines()
  expect_equal(rtxt, charToRaw(out))

  cmd <- paste("(cat", shQuote(tmp1), ";sleep 1;cat", shQuote(tmp2), ")")
  p <- process$new(px, c("cat", tmp1, "sleep", "1", "cat", tmp2),
                   stdout = "|", stderr = "|", encoding = "UTF-8")
  out <- p$read_all_output_lines()
  expect_equal(rtxt, charToRaw(out))
})

test_that("UTF-8 multibyte character cut in half at the end of the file", {

  px <- get_tool("px")
  rtxt <- charToRaw("a\xc2\xa0a")
  writeBin(c(rtxt, rtxt[1:2]), tmp1 <- tempfile())

  p <- process$new(px, c("cat", tmp1), stdout = "|", encoding = "UTF-8")
  expect_warning(
    out <- p$read_all_output_lines(),
    "Invalid multi-byte character at end of stream ignored"
  )
  expect_equal(charToRaw(out), c(rtxt, rtxt[1]))
})

test_that("Invalid UTF-8 characters in the middle of the string", {

  px <- get_tool("px")
  half <- charToRaw("\xc2\xa0")[1]
  rtxt <- sample(rep(c(half, charToRaw("a")), 100))
  writeBin(rtxt, tmp1 <- tempfile())

  p <- process$new(px, c("cat", tmp1), stdout = "|", encoding = "UTF-8")
  suppressWarnings(out <- p$read_all_output_lines())

  expect_equal(out, strrep("a", 100))
})

test_that("Convert from another encoding to UTF-8", {

  px <- get_tool("px")

  latin1 <- "\xe1\xe9\xed";
  writeBin(charToRaw(latin1), tmp1 <- tempfile())

  p <- process$new(px, c("cat", tmp1), stdout = "|", encoding = "latin1")
  suppressWarnings(out <- p$read_all_output_lines())

  expect_equal(charToRaw(out), charToRaw("\xc3\xa1\xc3\xa9\xc3\xad"))
})
