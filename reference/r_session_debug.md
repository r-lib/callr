# Interactive debugging of persistent R sessions

The `r_session$debug()` method is an interactive debugger to inspect the
stack of the background process after an error.

## Details

Note that on callr version 3.8.0 and above, you need to set the
`callr.traceback` option to `TRUE` (in the main process) to make the
subprocess dump the frames on error. This is because saving the frames
can be costly for large objects passed as arguments.

`$debug()` starts a REPL (Read-Eval-Print-Loop), that evaluates R
expressions in the subprocess. It is similar to
[`browser()`](https://rdrr.io/r/base/browser.html) and
[`debugger()`](https://rdrr.io/r/utils/debugger.html) and also has some
extra commands:

- `.help` prints a short help message.

- `.where` prints the complete stack trace of the error. (The same as
  the `$traceback()` method.

- `.inspect <n>` switches the "focus" to frame `<n>`. Frame 0 is the
  global environment, so `.inspect 0` will switch back to that.

To exit the debugger, press the usual interrupt key, i.e. `CTRL+c` or
`ESC` in some GUIs.

Here is an example session that uses `$debug()` (some output is omitted
for brevity):

    # ----------------------------------------------------------------------
    > rs <- r_session$new()
    > rs$run(function() knitr::knit("no-such-file"))
    Error in rs_run(self, private, func, args) :
     callr subprocess failed: cannot open the connection

    > rs$debug()
    Debugging in process 87361, press CTRL+C (ESC) to quit. Commands:
      .where       -- print stack trace
      .inspect <n> -- inspect a frame, 0 resets to .GlobalEnv
      .help        -- print this message
      <cmd>        -- run <cmd> in frame or .GlobalEnv

    3: file(con, "r")
    2: readLines(input2, encoding = "UTF-8", warn = FALSE)
    1: knitr::knit("no-such-file") at #1

    RS 87361 > .inspect 1

    RS 87361 (frame 1) > ls()
     [1] "encoding"  "envir"     "ext"       "in.file"   "input"     "input.dir"
     [7] "input2"    "ocode"     "oconc"     "oenvir"    "oopts"     "optc"
    [13] "optk"      "otangle"   "out.purl"  "output"    "quiet"     "tangle"
    [19] "text"

    RS 87361 (frame 1) > input
    [1] "no-such-file"

    RS 87361 (frame 1) > file.exists(input)
    [1] FALSE

    RS 87361 (frame 1) > # <CTRL + C>
    # ----------------------------------------------------------------------
