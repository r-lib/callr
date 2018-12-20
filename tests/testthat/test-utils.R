
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

test_that("default_repos", {
  def <- "https://cloud.r-project.org"

  withr::with_options(list(repos = NULL),
    expect_equal(
      default_repos(),
      c(CRAN = def)))

  withr::with_options(list(repos = character()),
    expect_equal(
      default_repos(),
      c(CRAN = def)))

  withr::with_options(list(repos = list()),
    expect_equal(
      default_repos(),
      list(CRAN = def)))

  withr::with_options(list(repos = c(foo = "bar")),
    expect_equal(
      default_repos(),
      c(foo = "bar", CRAN = def)))

  withr::with_options(list(repos = list(foo = "bar")),
    expect_equal(
      default_repos(),
      list(foo = "bar", CRAN = def)))

  withr::with_options(list(repos = c(foo = "bar", CRAN = "set")),
    expect_equal(
      default_repos(),
      c(foo = "bar", CRAN = "set")))

  withr::with_options(list(repos = c(foo = "bar", CRAN = "@CRAN@")),
    expect_equal(
      default_repos(),
      c(foo = "bar", CRAN = def)))
})
