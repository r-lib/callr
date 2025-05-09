test_that("messages in callr::r do not crash session", {
  skip_if_not_installed("cli")
  ret <- r(function() {
    cli::cli_text("fooobar")
    1 + 1
  })
  expect_identical(ret, 2)
  gc()
})

test_that("messages in callr::r_bg do not crash session", {
  skip_in_covr() # TODO: what wrong with this on Windows?
  skip_on_cran()
  skip_if_not_installed("cli")

  rx <- r_bg(function() {
    cli::cli_text("fooobar")
    1 + 1
  })
  rx$wait(5000)
  rx$kill()
  expect_equal(rx$get_exit_status(), 0)

  expect_equal(rx$get_result(), 2)
  processx::processx_conn_close(rx$get_output_connection())
  processx::processx_conn_close(rx$get_error_connection())
  gc()
})
