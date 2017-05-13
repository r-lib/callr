
context("utils")

test_that("set_var with empty list", {
  expect_equal(
    with_envvar(c(), Sys.getenv()),
    Sys.getenv()
  )
})

test_that("enumerate", {
  expect_equal(enumerate(character()), "")
  expect_equal(enumerate("foo"), "foo")
  expect_equal(enumerate(c("foo", "bar")), "foo and bar")
  expect_equal(enumerate(c("foo", "bar", "baz")), "foo, bar and baz")
})
