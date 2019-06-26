
context("clean-subprocess")

test_that("r() does not load anything", {
  pkgs <- withr::with_envvar(
    clean_envvars(),
    r(function() loadedNamespaces()))
  expect_equal(pkgs, "base")
})

test_that("r_bg() does not load anything", {
  p <- withr::with_envvar(
    clean_envvars(),
    r_bg(function() loadedNamespaces()))
  on.exit(p$kill(), add = TRUE)
  p$wait(3000)
  pkgs <- p$get_result()
  expect_equal(pkgs, "base")
})

test_that("r_session does not load anything", {
  rs <- withr::with_envvar(clean_envvars(), r_session$new())
  pkgs <- rs$run(function() loadedNamespaces())
  expect_equal(pkgs, "base")
})
