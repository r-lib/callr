test_that("r()", {
  # not passed by default
  error <- tryCatch(
    r(callr:::remove_source, list(r)),
    error = function(e) e
  )
  expect_match(conditionMessage(error), "could not find function")

  # keep it
  out <- r(callr:::remove_source, list(r), package = TRUE)
  expect_true(is.function(out))
  expect_equal(environmentName(environment(out)), "callr")

  # set it explicitly to package env
  out <- r(function(x) remove_source(x), list(r), package = "callr")
  expect_true(is.function(out))
  expect_equal(environmentName(environment(out)), "callr")
})

test_that("r_bg()", {
  # fails in covr :(
  testthat::skip_on_covr()
  # not passed by default
  p1 <- r_bg(callr:::remove_source, list(r))
  on.exit(p1$kill(), add = TRUE)
  p2 <- r_bg(callr:::remove_source, list(r), package = TRUE)
  on.exit(p2$kill(), add = TRUE)
  p3 <- r_bg(function(x) remove_source(x), list(r), package = "callr")
  on.exit(p3$kill(), add = TRUE)

  p1$wait(3000)
  p1$kill()
  error <- tryCatch(p1$get_result(), error = function(e) e)
  expect_match(conditionMessage(error), "could not find function")

  p2$wait(3000)
  p2$kill()
  out <- p2$get_result()
  expect_true(is.function(out))
  expect_equal(environmentName(environment(out)), "callr")

  p3$wait(3000)
  p3$kill()
  out <- p3$get_result()
  expect_true(is.function(out))
  expect_equal(environmentName(environment(out)), "callr")
})

test_that("r_session", {
  rs <- r_session$new()
  on.exit(rs$kill(), add = TRUE)

  error <- tryCatch(
    rs$run(callr:::remove_source, list(r)),
    error = function(e) e
  )
  expect_match(conditionMessage(error), "could not find function")

  # keep it
  out <- rs$run(callr:::remove_source, list(r), package = TRUE)
  expect_true(is.function(out))
  expect_equal(environmentName(environment(out)), "callr")

  # set it explicitly to package env
  out <- rs$run(function(x) remove_source(x), list(r), package = "callr")
  expect_true(is.function(out))
  expect_equal(environmentName(environment(out)), "callr")
})

test_that("r() preserves the environment of crated functions by default", {
  skip_if_not_installed("carrier")
  fn <- carrier::crate(function() payload, payload = 42)
  expect_equal(r(fn), 42)

  # explicit package = FALSE still wipes the environment
  err <- tryCatch(r(fn, package = FALSE), error = function(e) e)
  expect_match(conditionMessage(err), "payload")
})

test_that("r_bg() preserves the environment of crated functions by default", {
  skip_if_not_installed("carrier")
  testthat::skip_on_covr()
  fn <- carrier::crate(function() payload, payload = 42)
  p <- r_bg(fn)
  on.exit(p$kill(), add = TRUE)
  p$wait(3000)
  p$kill()
  expect_equal(p$get_result(), 42)
})

test_that("r_session$run() preserves the env of crated funcs by default", {
  skip_if_not_installed("carrier")
  fn <- carrier::crate(function() payload, payload = 42)
  rs <- r_session$new()
  on.exit(rs$kill(), add = TRUE)
  expect_equal(rs$run(fn), 42)
})
