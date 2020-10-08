
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
