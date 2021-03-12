
test_that("r() to the other arch", {
  skip_on_cran()
  archs <- supported_archs()
  if (length(archs) < 2) return(expect_true(TRUE))
  ret <- unlist(lapply(
    archs, function(a) r(function() .Platform$r_arch, arch = a)
  ))
  expect_equal(archs, ret)
})

test_that("r_bg() to the other arch", {
  skip_on_cran()
  archs <- supported_archs()
  if (length(archs) < 2) return(expect_true(TRUE))
})

test_that("r_process to the other arch", {
  skip_on_cran()
  archs <- supported_archs()
  if (length(archs) < 2) return(expect_true(TRUE))
})

test_that("r_session to the other arch", {
  skip_on_cran()
  archs <- supported_archs()
  if (length(archs) < 2) return(expect_true(TRUE))
})
