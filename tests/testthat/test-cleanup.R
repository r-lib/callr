
context("Cleanup")

test_that("process is cleaned up", {

  px <- get_tool("px")
  p <- process$new(px, c("sleep", "1"), cleanup = TRUE)
  pid <- p$get_pid()

  rm(p)
  gc()

  expect_false(process__exists(pid))
})

test_that("process can stay alive", {

  px <- get_tool("px")

  on.exit(tools::pskill(pid, 9), add = TRUE)
  p <- process$new(px, c("sleep", "60"), cleanup = FALSE)
  pid <- p$get_pid()

  rm(p)
  gc()

  expect_true(process__exists(pid))
})
