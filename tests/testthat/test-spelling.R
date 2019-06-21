
context("spelling")

test_that("spell check", {
  pkg_dir <- test_package_root()
  results <- spelling::spell_check_package(pkg_dir)

  if (nrow(results)) {
    output <- sprintf(
      "Potential spelling errors: %s\n",
      paste(results$word, collapse = ", "))
    stop(output, call. = FALSE)
  } else {
    expect_true(TRUE)
  }
})
