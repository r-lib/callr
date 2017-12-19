
context("print")

test_that("print", {

  px <- get_tool("px")
  p <- process$new(px, c("sleep", "5"))
  on.exit(try_silently(p$kill(grace = 0)), add = TRUE)
  expect_output(
    print(p),
    "PROCESS .* running, pid"
  )

  p$kill()
  expect_output(
    print(p),
    "PROCESS .* finished"
  )
})
