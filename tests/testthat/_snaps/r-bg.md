# r_bg can be killed

    Code
      x$get_result()
    Condition
      Error:
      ! callr subprocess failed: could not start R, exited with non-zero status, has crashed or was killed

# r_bg can get the error back

    Code
      x$get_result()
    Condition
      Error:
      ! in callr subprocess.
      Caused by error:
      ! non-numeric argument to binary operator

