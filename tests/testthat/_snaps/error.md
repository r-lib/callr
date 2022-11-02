# error is propagated, .Last.error is set

    Code
      r_process()
    Output
      > callr::r(function() 1 + "A", error = "error")
      Error: 
      ! in callr subprocess.
      Caused by error in `1 + "A"`:
      ! non-numeric argument to binary operator
      Type .Last.error to see the more details.
      > .Last.error
      <callr_error/rlib_error_3_0/rlib_error/error>
      Error: 
      ! in callr subprocess.
      Caused by error in `1 + "A"`:
      ! non-numeric argument to binary operator
      ---
      Backtrace:
      1. callr::r(function() 1 + "A", error = "error")
      2. callr:::get_result(output = out, options)
      3. callr:::throw(callr_remote_error(remerr, output), parent = fix_msg(remerr[[3]]))
      ---
      Subprocess backtrace:
      1. base::.handleSimpleError(function (e)
      2. global h(simpleError(msg, call))

# error is propagated, printed if non-interactive mode

    Code
      r_process()
    Output
      > callr::r(function() 1 + "A", error = "error")
      Error: 
      ! in callr subprocess.
      Caused by error in `1 + "A"`:
      ! non-numeric argument to binary operator
      ---
      Backtrace:
      1. callr::r(function() 1 + "A", error = "error")
      2. callr:::get_result(output = out, options)
      3. callr:::throw(callr_remote_error(remerr, output), parent = fix_msg(remerr[[3]]))
      ---
      Subprocess backtrace:
      1. base::.handleSimpleError(function (e)
      2. global h(simpleError(msg, call))
      Execution halted

# error stack is passed, .Last.error is set

    Code
      r_process()
    Output
      > callr::r(function() {
      +     f <- function() g()
      +     g <- function() 1 + "A"
      +     f()
      + }, error = "stack")
      Error: 
      ! in callr subprocess.
      Caused by error in `1 + "A"`:
      ! non-numeric argument to binary operator
      i With remote `$stack`, use `utils::debugger()` to debug it.
      Type .Last.error to see the more details.
      > .Last.error
      <callr_error/rlib_error_3_0/rlib_error/error>
      Error: 
      ! in callr subprocess.
      Caused by error in `1 + "A"`:
      ! non-numeric argument to binary operator
      i With remote `$stack`, use `utils::debugger()` to debug it.
      ---
      Backtrace:
      1. callr::r(function() {
      2. callr:::get_result(output = out, options)
      3. callr:::throw(callr_remote_error_with_stack(remerr, output), parent = fix_msg(remer

# error behavior can be set using option

    Code
      r(function() 1 + "A")
    Error <callr_status_error>
      ! in callr subprocess.
      Caused by error in `1 + "A"`:
      ! non-numeric argument to binary operator

---

    Code
      r(function() {
        f <- (function() g())
        g <- (function() 1 + "A")
        f()
      })
    Error <callr_status_error>
      ! in callr subprocess.
      Caused by error in `1 + "A"`:
      ! non-numeric argument to binary operator

# parent errors

    Code
      err <- tryCatch(r(function() 1 + "A"), error = function(e) e)
      err$parent
    Output
      <simpleError in "1 + \"A\"": non-numeric argument to binary operator>

# parent errors, another level

    Code
      err <- tryCatch(callr::r(function() {
        withr::local_options(list(callr.error = "error"))
        callr::r(function() 1 + "A")
      }), error = function(e) e)
      err$parent
    Output
      <callr_error/rlib_error_3_0/rlib_error/error>
      Error: 
      ! in callr subprocess.
      Caused by error in `1 + "A"`:
      ! non-numeric argument to binary operator
      ---
      Subprocess backtrace:
      1. base::.handleSimpleError(function (e) ...
      2. global h(simpleError(msg, call))
    Code
      err$parent$parent
    Output
      <simpleError in "1 + \"A\"": non-numeric argument to binary operator>

# error traces are printed recursively

    Code
      r_process()
    Output
      > callr::r(function() callr::r(function() 1 + "a"))
      Error: 
      ! in callr subprocess.
      Caused by error: 
      ! in callr subprocess.
      Caused by error in `1 + "a"`:
      ! non-numeric argument to binary operator
      ---
      Backtrace:
      1. callr::r(function() callr::r(function() 1 + "a"))
      2. callr:::get_result(output = out, options)
      3. callr:::throw(callr_remote_error(remerr, output), parent = fix_msg(remerr[[3]]))
      ---
      Subprocess backtrace:
      1. callr::r(function() 1 + "a")
      2. callr:::get_result(output = out, options)
      3. base::throw(callr_remote_error(remerr, output), parent = fix_msg(remerr[[3]]))
      4. | base::signalCondition(cond)
      5. global (function (e)
      ---
      Subprocess backtrace:
      1. base::.handleSimpleError(function (e)
      2. global h(simpleError(msg, call))
      Execution halted

# errors in r_bg() are merged

    Code
      p$get_result()
    Error <callr_status_error>
      ! in callr subprocess.
      Caused by error in `1 + "A"`:
      ! non-numeric argument to binary operator

# errors in r_process are merged

    Code
      p$get_result()
    Error <callr_status_error>
      ! in callr subprocess.
      Caused by error in `1 + "A"`:
      ! non-numeric argument to binary operator

# errors in r_session$run() are merged

    Code
      rs$run(function() 1 + "A")
    Error <callr_status_error>
      ! in callr subprocess.
      Caused by error in `1 + "A"`:
      ! non-numeric argument to binary operator

---

    Code
      rs$run(function() 1 + "A")
    Error <callr_status_error>
      ! in callr subprocess.
      Caused by error in `1 + "A"`:
      ! non-numeric argument to binary operator

# errors in r_session$call() are merged

    Code
      rs$read()$error
    Output
      <callr_error/rlib_error_3_0/rlib_error/error>
      Error: 
      ! in callr subprocess.
      Caused by error in `1 + "A"`:
      ! non-numeric argument to binary operator
      ---
      Subprocess backtrace:
      1. base::.handleSimpleError(function (e) ...
      2. global h(simpleError(msg, call))

---

    Code
      rs$read()$error
    Output
      <callr_error/rlib_error_3_0/rlib_error/error>
      Error: 
      ! in callr subprocess.
      Caused by error in `1 + "A"`:
      ! non-numeric argument to binary operator
      ---
      Subprocess backtrace:
      1. base::.handleSimpleError(function (e) ...
      2. global h(simpleError(msg, call))

# child error is not modified

    Code
      err <- tryCatch(callr::r(function() stop("foobar")), error = function(e) e)
      err
    Output
      <callr_error/rlib_error_3_0/rlib_error/error>
      Error: 
      ! in callr subprocess.
      Caused by error in `(function () ...`:
      ! foobar
      ---
      Subprocess backtrace:
      1. base::stop("foobar")
      2. | base::.handleSimpleError(function (e) ...
      3. global h(simpleError(msg, call))
    Code
      class(err)
    Output
      [1] "callr_status_error" "callr_error"        "rlib_error_3_0"    
      [4] "rlib_error"         "error"              "condition"         
    Code
      class(err$parent)
    Output
      [1] "simpleError" "error"       "condition"  

# new_callr_error, timeout

    Code
      r_process()
    Output
      > callr::r(function() Sys.sleep(3), timeout = 1/5)
      Error in `get_result(output = out, options)`:
      ! callr timed out
      Type .Last.error to see the more details.

---

    Code
      callr::r(function() Sys.sleep(3), timeout = 1 / 5)
    Error <callr_timeout_error>
      ! callr timed out

# interrupting an R session

    Code
      rs$read()
    Output
      $code
      [1] 200
      
      $message
      [1] "done callr-rs-result-<id>"
      
      $result
      NULL
      
      $stdout
      [1] ""
      
      $stderr
      [1] ""
      
      $error
      <callr_timeout_error/callr_error/rlib_error_3_0/rlib_error/error>
      Error: 
      ! callr subprocess interrupted
      Caused by error: 
      ! interrupt
      
      attr(,"class")
      [1] "callr_session_result"

# format.call_status_error

    Code
      format(err)
    Output
      [1] "Error: "                                  
      [2] "! in callr subprocess."                   
      [3] "Caused by error in `1 + \"\"`:"           
      [4] "! non-numeric argument to binary operator"
      [5] "Type .Last.error to see the more details."

---

    Code
      print(err)
    Output
      <callr_error/rlib_error_3_0/rlib_error/error>
      Error: 
      ! in callr subprocess.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
      ---
      Subprocess backtrace:
      1. base::.handleSimpleError(function (e) ...
      2. global h(simpleError(msg, call))

---

    Code
      format(err)
    Output
      [1] "Error: "                                                     
      [2] "! in callr subprocess."                                      
      [3] "Caused by error in `1 + \"\"`:"                              
      [4] "! non-numeric argument to binary operator"                   
      [5] "i With remote `$stack`, use `utils::debugger()` to debug it."
      [6] "Type .Last.error to see the more details."                   

---

    Code
      print(err)
    Output
      <callr_error/rlib_error_3_0/rlib_error/error>
      Error: 
      ! in callr subprocess.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
      i With remote `$stack`, use `utils::debugger()` to debug it.

# format.call_status_error 2

    Code
      r_process()
    Output
      > withr::local_options(rlib_error_always_trace = TRUE)
      > err <- tryCatch(callr::r(function() 1 + ""), error = function(e) e)
      > writeLines(format(err, trace = TRUE))
      Error: 
      ! in callr subprocess.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
      ---
      Backtrace:
      1. base::tryCatch(callr::r(function() 1 + ""), error = function(e) e)
      2. base::tryCatchList(expr, classes, parentenv, handlers)
      3. base::tryCatchOne(expr, names, parentenv, handlers[[1L]])
      4. base::doTryCatch(return(expr), name, parentenv, handler)
      5. callr::r(function() 1 + "")
      6. callr:::get_result(output = out, options)
      7. callr:::throw(callr_remote_error(remerr, output), parent = fix_msg(remerr[[3]]))
      ---
      Subprocess backtrace:
      1. base::.handleSimpleError(function (e)
      2. global h(simpleError(msg, call))

# stdout/stderr is printed on error

    Code
      r_process()
    Output
      > callr::r(function() {
      +     warning("I have a bad feeling about this")
      +     stop("told ya")
      + })
      Error: 
      ! in callr subprocess.
      Caused by error in `(function () ...`:
      ! told ya
      i See `$stderr` for standard error.
      Type .Last.error to see the more details.
      > .Last.error
      <callr_error/rlib_error_3_0/rlib_error/error>
      Error: 
      ! in callr subprocess.
      Caused by error in `(function () ...`:
      ! told ya
      i See `$stderr` for standard error.
      ---
      Backtrace:
      1. callr::r(function() {
      2. callr:::get_result(output = out, options)
      3. callr:::throw(callr_remote_error(remerr, output), parent = fix_msg(remerr[[3]]))
      ---
      Subprocess backtrace:
      1. base::stop("told ya")
      2. | base::.handleSimpleError(function (e)
      3. global h(simpleError(msg, call))
      > .Last.error$stderr
      [1] "Warning message:\nIn (function ()  : I have a bad feeling about this\n"

# stdout/stderr is printed on error 2

    Code
      r_process()
    Output
      > callr::r(function() {
      +     writeLines("Just some output")
      +     stop("told ya")
      + })
      Error: 
      ! in callr subprocess.
      Caused by error in `(function () ...`:
      ! told ya
      i See `$stdout` for standard output.
      Type .Last.error to see the more details.
      > .Last.error
      <callr_error/rlib_error_3_0/rlib_error/error>
      Error: 
      ! in callr subprocess.
      Caused by error in `(function () ...`:
      ! told ya
      i See `$stdout` for standard output.
      ---
      Backtrace:
      1. callr::r(function() {
      2. callr:::get_result(output = out, options)
      3. callr:::throw(callr_remote_error(remerr, output), parent = fix_msg(remerr[[3]]))
      ---
      Subprocess backtrace:
      1. base::stop("told ya")
      2. | base::.handleSimpleError(function (e)
      3. global h(simpleError(msg, call))
      > .Last.error$stdout
      [1] "Just some output\n"

# stdout/stderr is printed on error 3

    Code
      r_process()
    Output
      > callr::r(function() {
      +     writeLines("Just some output")
      +     warning("I have a bad feeling about this")
      +     stop("told ya")
      + })
      Error: 
      ! in callr subprocess.
      Caused by error in `(function () ...`:
      ! told ya
      ---
      Standard output:
      Just some output
      ---
      Standard error:
      Warning message:
      In (function ()  : I have a bad feeling about this
      ---
      Backtrace:
      1. callr::r(function() {
      2. callr:::get_result(output = out, options)
      3. callr:::throw(callr_remote_error(remerr, output), parent = fix_msg(remerr[[3]]))
      ---
      Subprocess backtrace:
      1. base::stop("told ya")
      2. | base::.handleSimpleError(function (e)
      3. global h(simpleError(msg, call))
      Execution halted

# error is printed to file

    Code
      err$stderr
    Output
      [1] "Error in (function ()  : ouch\n"

---

    Code
      readLines(tmp)
    Output
      [1] "Error in (function ()  : ouch"

