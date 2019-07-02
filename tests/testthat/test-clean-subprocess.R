
context("clean-subprocess")

test_that("r() does not load anything", {
  pkgs <- withr::with_envvar(
    clean_envvars(),
    r(function() loadedNamespaces()))
  if (length(pkgs) > 1) print(pkgs)
  expect_equal(pkgs, "base")
})

test_that("r_bg() does not load anything", {
  p <- withr::with_envvar(
    clean_envvars(),
    r_bg(function() loadedNamespaces()))
  on.exit(p$kill(), add = TRUE)
  p$wait(3000)
  pkgs <- p$get_result()
  if (length(pkgs) > 1) print(pkgs)
  expect_equal(pkgs, "base")
})

test_that("r_session does not load anything", {
  rs <- withr::with_envvar(clean_envvars(), r_session$new())
  on.exit(rs$close(), add = TRUE)
  pkgs <- rs$run(function() loadedNamespaces())
  if (length(pkgs) > 1) print(pkgs)
  expect_equal(pkgs, "base")
  gc()
})
