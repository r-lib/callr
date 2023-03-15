
test_that("callr_message, then error", {
  rs <- r_session$new()
  on.exit(rs$kill(), add = TRUE)

  do <- function() {
    msg <- structure(list(message = "hi"),
                     class = c("callr_message", "condition"))
    signalCondition(msg)
    signalCondition(msg)
    stop("nah-ah")
  }

  msg <- err <- NULL
  tryCatch(
    withCallingHandlers(
      rs$run(do),
      callr_message = function(m) msg <<- c(msg, list(m))),
    error = function(e) err <<- e)

  expect_s3_class(msg[[1]], "callr_message")
  expect_equal(conditionMessage(msg[[1]]), "hi")
  expect_s3_class(msg[[2]], "callr_message")
  expect_equal(conditionMessage(msg[[2]]), "hi")

  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "nah-ah")

  expect_true(rs$is_alive())
  expect_equal(rs$get_state(), "idle")
  expect_identical(rs$read_error_lines(), character())

  rs$close()
})

test_that("message handlers", {
  skip_if_not_installed("withr")
  rs <- r_session$new()
  on.exit(rs$kill(), add = TRUE)

  do <- function() {
    msg <- structure(list(message = "hi"),
                     class = c("myclass", "callr_message", "condition"))
    signalCondition(msg)
  }

  cond <- NULL
  withr::with_options(
    list(callr.condition_handler_myclass = function(x) {
      cond <<- x
    }),
    rs$run(do)
  )

  expect_s3_class(cond, "myclass")
  expect_equal(cond$message, "hi")

  rs$close()
})

test_that("large messages", {
  skip_if_not_installed("withr")
  rs <- r_session$new()
  on.exit(rs$close(), add = TRUE)

  do <- function() {
    msg <- structure(list(message = paste(1:150000, sep = " ")),
                     class = c("myclass", "callr_message", "condition"))
    signalCondition(msg)
    for (i in 1:5) {
      msg <- structure(list(message = paste("message", i)),
                       class = c("myclass", "callr_message", "condition"))
      signalCondition(msg)
    }
  }

  cond <- list()
  withr::with_options(
    list(callr.condition_handler_myclass = function(x) {
      cond <<- c(cond, list(x))
    }),
    rs$run(do)
  )

  expect_equal(length(cond), 6)
  expect_s3_class(cond[[1]], "myclass")
  expect_equal(cond[[1]]$message, paste(1:150000, sep = " "))
  for (i in 1:5) {
    expect_equal(cond[[i + 1]]$message, paste("message", i))
  }

  rs$close()
})
