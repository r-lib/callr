
test_that("all figures in man/figures are needed", {
  skip_on_cran()
  skip_on_covr()
  pkg_dir <- test_package_root()
  figs <- dir(file.path(pkg_dir, "man", "figures"))
  readme <- file.path(pkg_dir, "README.md")
  readme_figs <- grep("man/figures/", readLines(readme), value = TRUE)
  readme_figs <- sub("^.*man/figures/(.*[.]svg).*$", "\\1", readme_figs)
  expect_equal(
    sort(figs),
    sort(readme_figs)
  )
})
