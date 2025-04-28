# errors are printed on stderr

    Code
      r(f, stdout = out <- tempfile(), stderr = err <- tempfile())
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! send to stderr 2

