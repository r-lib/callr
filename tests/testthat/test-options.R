test_that("error for unknown options", {
  expect_snapshot(error = TRUE, {
    r_process_options(
      func = function() {},
      foo = "bar"
    )
  })

  expect_snapshot(error = TRUE, {
    r_process_options(
      func = function() {},
      foo = "bar",
      bar = "foo"
    )
  })

  gc()
})
