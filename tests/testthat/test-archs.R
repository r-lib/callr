
test_that("r() to the other arch", {
  skip_on_cran()
  archs <- supported_archs()
  if (length(archs) < 1) return(expect_true(TRUE))
  ret <- unlist(lapply(
    archs, function(a) r(function() .Platform$r_arch, arch = a)
  ))
  expect_equal(ret, archs)
})

test_that("r_bg() to the other arch", {
  skip_on_cran()
  archs <- supported_archs()
  if (length(archs) < 1) return(expect_true(TRUE))
  procs <- lapply(archs, function(a) {
    r_bg(function() .Platform$r_arch, arch = a)
  })
  on.exit(lapply(procs, function(p) p$kill()), add = TRUE)
  for (p in procs) p$wait(3000)
  for (p in procs) expect_false(p$is_alive())
  res <- unlist(lapply(procs, function(p) p$get_result()))
  expect_equal(res, archs)
})

test_that("r_process to the other arch", {
  skip_on_cran()
  archs <- supported_archs()
  if (length(archs) < 1) return(expect_true(TRUE))
  procs <- lapply(archs, function(a) {
    opts <- r_process_options(
      func = function() .Platform$r_arch,
      arch = a
    )
    r_process$new(opts)
  })
  on.exit(lapply(procs, function(p) p$kill()), add = TRUE)
  for (p in procs) p$wait(3000)
  for (p in procs) expect_false(p$is_alive())
  res <- unlist(lapply(procs, function(p) p$get_result()))
  expect_equal(res, archs)
})

test_that("r_session to the other arch", {
  skip_on_cran()
  archs <- supported_archs()
  if (length(archs) < 1) return(expect_true(TRUE))
  ret <- unlist(lapply(archs, function(a) {
    opts <- r_session_options(arch = a)
    rs <- r_session$new(opts)
    on.exit(rs$close(), add = TRUE)
    rs$run(function() .Platform$r_arch)
  }))
  expect_equal(ret, archs)
})
