
context("clean-subprocess")

test_that("r() does not load anything", {
  skip_in_covr()
  pkgs <- withr::with_envvar(
    clean_envvars(),
    r(function() loadedNamespaces()))
  if (length(pkgs) > 1) print(pkgs)
  ## Some R versions still load compiler...
  expect_true(all(pkgs %in% c("base", "compiler")))
})

test_that("r_bg() does not load anything", {
  skip_in_covr()
  p <- withr::with_envvar(
    clean_envvars(),
    r_bg(function() loadedNamespaces()))
  on.exit(p$kill(), add = TRUE)
  p$wait(3000)
  pkgs <- p$get_result()
  if (length(pkgs) > 1) print(pkgs)
  ## Some R versions still load compiler...
  expect_true(all(pkgs %in% c("base", "compiler")))
})

test_that("r_session does not load anything", {
  skip_in_covr()
  rs <- withr::with_envvar(clean_envvars(), r_session$new())
  on.exit(rs$close(), add = TRUE)
  pkgs <- rs$run(function() loadedNamespaces())
  if (length(pkgs) > 1) print(pkgs)
  ## Some R versions still load compiler...
  expect_true(all(pkgs %in% c("base", "compiler")))
  gc()
})
