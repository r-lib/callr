
context("utils")

test_that("set_var with empty list", {
  expect_equal(
    with_envvar(c(), Sys.getenv()),
    Sys.getenv()
  )
})
