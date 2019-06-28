
context("mmap data transfer")

test_that("mmap transfer", {
  skip_on_os("windows")

  data <- list(a = 1:100, b = runif(100), c = charToRaw("foobar"))
  fun <- function(a, b, c) {
    list(mean(a), median(b), rawToChar(c))
  }

  res <- callr::r(fun, args = data, transfer = "mmap")

  expect_identical(res, do.call(fun, data))
})
