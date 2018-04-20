
context("poll stress test")

test_that("many processes", {
  skip_on_cran()

  ## Create many processes
  num <- 100
  px <- get_tool("px")
  on.exit(try(lapply(pp, function(x) x$kill()), silent = TRUE), add = TRUE)
  pp <- lapply(1:num, function(i) {
    cmd <- c("sleep", "1", "outln", paste("out", i),
             "errln", paste("err", i))
    process$new(px, cmd, stdout = "|",  stderr = "|")
  })

  ## poll them
  results <- replicate(num, list(character(), character()), simplify = FALSE)
  while (TRUE) {
    pr <- poll(pp, -1)
    lapply(seq_along(pp), function(i) {
      if (pr[[i]]["output"] == "ready") {
        results[[i]][[1]] <<- c(results[[i]][[1]], pp[[i]]$read_output_lines())
      }
      if (pr[[i]]["error"] == "ready") {
        results[[i]][[2]] <<- c(results[[i]][[2]], pp[[i]]$read_error_lines())
      }
    })
    inc <- sapply(pp, function(x) x$is_incomplete_output() || x$is_incomplete_error())
    if (!any(inc)) break
  }

  exp <- lapply(1:num, function(i) list(paste("out", i), paste("err", i)))
  expect_identical(exp, results)
})
