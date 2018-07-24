
context("utils")

test_that("is_complete_expression", {
  do_tests <- function() {
    expect_true(is_complete_expression(""))
    expect_true(is_complete_expression("1"))
    expect_true(is_complete_expression("1+1"))
    expect_true(is_complete_expression("foo + \n  bar"))
    expect_true(is_complete_expression("1 1"))

    expect_false(is_complete_expression("1+"))
    expect_false(is_complete_expression("1+1+"))
    expect_false(is_complete_expression("1+\n2+"))
  }

  do_tests()

  if (has_locale("de_DE")) {
    withr::with_envvar(c(LANGUAGE = "de_DE"), do_tests())
  }
})
