test_that("r() runs through the bundled launcher", {
  skip_on_cran()
  launcher <- bundled_launcher_path()
  if (is.na(launcher)) {
    skip("No bundled launcher in this install")
  }
  arg0 <- r(function() commandArgs(FALSE)[[1]])
  expect_identical(arg0, launcher)
})

test_that("r_session runs through the bundled launcher", {
  skip_on_cran()
  launcher <- bundled_launcher_path()
  if (is.na(launcher)) {
    skip("No bundled launcher in this install")
  }
  rs <- r_session$new()
  on.exit(rs$close(), add = TRUE)
  arg0 <- rs$run(function() commandArgs(FALSE)[[1]])
  expect_identical(arg0, launcher)
})

test_that("non-default arch bypasses the bundled launcher", {
  skip_on_cran()
  if (is.na(bundled_launcher_path())) {
    skip("No bundled launcher in this install")
  }
  archs <- supported_archs()
  if (!nzchar(archs[[1]])) {
    skip("Build does not have a named arch subdirectory")
  }
  ret <- unlist(lapply(archs, function(a) {
    r(function() commandArgs(FALSE)[[1]], arch = a)
  }))
  expect_false(any(grepl("callrem(\\.exe)?$", ret)))
})
