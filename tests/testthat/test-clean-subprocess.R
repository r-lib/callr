
context("clean-subprocess")

test_that("r() does not load anything", {
  skip_in_covr()
  pkgs <- withr::with_envvar(
    clean_envvars(),
    r(without_env(function() loadedNamespaces())))
  if (length(pkgs) > 1) print(pkgs)
  ## Some R versions still load compiler...
  expect_true(all(pkgs %in% c("base", "compiler")))
})

test_that("r_bg() does not load anything", {
  skip_in_covr()
  p <- withr::with_envvar(
    clean_envvars(),
    r_bg(without_env(function() loadedNamespaces())))
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
  pkgs <- rs$run(without_env(function() loadedNamespaces()))
  if (length(pkgs) > 1) print(pkgs)
  ## Some R versions still load compiler...
  expect_true(all(pkgs %in% c("base", "compiler")))
  gc()
})

test_that("r() does not create objects in global env", {
  vars <- r(function() ls(.GlobalEnv))
  expect_identical(vars, character())
})

test_that("r_bg() does not create objects in global env", {
  p <- r_bg(function() ls(.GlobalEnv))
  on.exit(p$kill(), add = TRUE)
  p$wait(3000)
  vars <- p$get_result()
  expect_identical(vars, character())
})

test_that("r_session does not create objects in global env", {
  rs <- r_session$new()
  on.exit(rs$close(), add = TRUE)
  vars <- rs$run(function() ls(.GlobalEnv))
  expect_identical(vars, character())
  rs$close()
  gc()
})
