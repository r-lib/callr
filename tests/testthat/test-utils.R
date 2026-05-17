test_that("callr_tempdir honors CALLR_TMPDIR", {
  skip_if_not_installed("withr")

  withr::local_envvar(CALLR_TMPDIR = NA_character_)
  expect_identical(callr_tempdir(), tempdir())

  withr::local_envvar(CALLR_TMPDIR = "")
  expect_identical(callr_tempdir(), tempdir())

  tmp <- withr::local_tempfile()
  withr::local_envvar(CALLR_TMPDIR = tmp)
  expect_false(dir.exists(tmp))
  expect_identical(normalizePath(callr_tempdir()), normalizePath(tmp))
  expect_true(dir.exists(tmp))
})

test_that("callr_tempfile places files under CALLR_TMPDIR", {
  skip_if_not_installed("withr")

  tmp <- withr::local_tempfile()
  withr::local_envvar(CALLR_TMPDIR = tmp)

  f <- callr_tempfile("callr-utest-")
  expect_identical(
    normalizePath(dirname(f)),
    normalizePath(tmp)
  )
  expect_match(basename(f), "^callr-utest-")
})

test_that("r() works with CALLR_TMPDIR set", {
  skip_if_not_installed("withr")
  skip_in_covr()

  tmp <- withr::local_tempfile()
  withr::local_envvar(CALLR_TMPDIR = tmp)

  # The directory should not exist yet — callr_tempdir() creates it on
  # demand the first time r() needs a temp file.
  expect_false(dir.exists(tmp))
  expect_equal(callr::r(function() 1 + 1), 2)
  expect_true(dir.exists(tmp))
})

test_that("is_complete_expression", {
  skip_if_not_installed("withr")
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
  skip_if_not_installed("withr")
  def <- "https://cloud.r-project.org"

  withr::with_options(
    list(repos = NULL),
    expect_equal(
      default_repos(),
      c(CRAN = def)
    )
  )

  withr::with_options(
    list(repos = character()),
    expect_equal(
      default_repos(),
      c(CRAN = def)
    )
  )

  withr::with_options(
    list(repos = list()),
    expect_equal(
      default_repos(),
      list(CRAN = def)
    )
  )

  withr::with_options(
    list(repos = c(foo = "bar")),
    expect_equal(
      default_repos(),
      c(foo = "bar", CRAN = def)
    )
  )

  withr::with_options(
    list(repos = list(foo = "bar")),
    expect_equal(
      default_repos(),
      list(foo = "bar", CRAN = def)
    )
  )

  withr::with_options(
    list(repos = c(foo = "bar", CRAN = "set")),
    expect_equal(
      default_repos(),
      c(foo = "bar", CRAN = "set")
    )
  )

  withr::with_options(
    list(repos = c(foo = "bar", CRAN = "@CRAN@")),
    expect_equal(
      default_repos(),
      c(foo = "bar", CRAN = def)
    )
  )
})
