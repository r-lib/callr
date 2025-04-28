# run throws

    Code
      rs$run(function() stop("foobar"))
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! foobar

# traceback

    Code
      rs$run(do)
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! oops

# error in the load hook

    Code
      local({
        rs <- r_session$new(opts)
        on.exit(rs$kill(), add = TRUE)
      })
    Condition
      Error:
      ! callr subprocess failed: Failed to start R session

