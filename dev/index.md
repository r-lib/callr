# callr

> Call R from R

It is sometimes useful to perform a computation in a separate R process,
without affecting the current R process at all. This packages does
exactly that.

------------------------------------------------------------------------

- [Features](#features)
- [Installation](#installation)
- [Synchronous, one-off R processes](#synchronous-one-off-r-processes)
  - [Passing arguments](#passing-arguments)
  - [Passing large arguments with shared
    memory](#passing-large-arguments-with-shared-memory)
  - [Using packages](#using-packages)
  - [Error handling](#error-handling)
  - [Standard output and error](#standard-output-and-error)
- [Background R processes](#background-r-processes)
- [Multiple background R processes and
  `poll()`](#multiple-background-r-processes-and-poll)
- [Persistent R sessions](#persistent-r-sessions)
- [Running `R CMD` commands](#running-r-cmd-commands)
- [Observability](#observability)
- [Configuration](#configuration)
  - [Environment variables](#environment-variables)
- [Code of Conduct](#code-of-conduct)

## Features

- Calls an R function, with arguments, in a subprocess.
- Copies function arguments to the subprocess and copies the return
  value of the function back, seamlessly.
- Copies error objects back from the subprocess, including a stack
  trace.
- Shows and/or collects the standard output and standard error of the
  subprocess.
- Supports both one-off and persistent R subprocesses.
- Calls the function synchronously or asynchronously (in the
  background).
- Can call `R CMD` commands, synchronously or asynchronously.
- Can call R scripts, synchronously or asynchronously.
- Provides extensible `r_process`, `rcmd_process` and `rscript_process`
  R6 classes, based on
  [`processx::process`](http://processx.r-lib.org/reference/process.md).
- Emits [OpenTelemetry](https://opentelemetry.io/) traces for every R
  subprocess and propagates trace context across the process boundary.

## Installation

Install the stable version from CRAN:

``` r

install.packages("callr")
```

Install the development version from GitHub:

``` r

pak::pak("r-lib/callr")
```

## Synchronous, one-off R processes

Use [`r()`](https://callr.r-lib.org/dev/reference/r.md) to run an R
function in a new R process. The results are passed back seamlessly:

``` r

callr::r(function() var(iris[, 1:4]))
```


    #>              Sepal.Length Sepal.Width Petal.Length Petal.Width
    #> Sepal.Length    0.6856935  -0.0424340    1.2743154   0.5162707
    #> Sepal.Width    -0.0424340   0.1899794   -0.3296564  -0.1216394
    #> Petal.Length    1.2743154  -0.3296564    3.1162779   1.2956094
    #> Petal.Width     0.5162707  -0.1216394    1.2956094   0.5810063

### Passing arguments

You can pass arguments to the function by setting `args` to the list of
arguments. This is often necessary as these arguments are explicitly
copied to the child process, whereas the evaluated function cannot refer
to variables in the parent. For example, the following does not work:

``` r

mycars <- cars
callr::r(function() summary(mycars))
```


    #> Error:
    #> ! in callr subprocess.
    #> Caused by error in `(function () …`:
    #> ! object 'mycars' not found
    #> Type .Last.error to see the more details.

But this does:

``` r

mycars <- cars
callr::r(function(x) summary(x), args = list(mycars))
```


    #>      speed           dist
    #>  Min.   : 4.0   Min.   :  2.00
    #>  1st Qu.:12.0   1st Qu.: 26.00
    #>  Median :15.0   Median : 36.00
    #>  Mean   :15.4   Mean   : 42.98
    #>  3rd Qu.:19.0   3rd Qu.: 56.00
    #>  Max.   :25.0   Max.   :120.00

Note that the arguments will be serialized and saved to a file, so if
they are large R objects, it might take a long time for the child
process to start up.

### Passing large arguments with shared memory

For very large R objects, the serialization cost can dominate startup
time. The [mori](https://shikokuchuo.net/mori/) package provides a way
around this: `mori::share()` places an object in OS shared memory and
returns a lightweight handle. When callr serializes that handle, only a
few bytes are transferred — the child process maps the same shared
memory region directly, with no copy.

``` r

big_data <- rnorm(1e8)
shared_data <- mori::share(big_data)
system.time(print(mean(big_data)))
system.time(
   print(callr::r(function(x) mean(x), args = list(big_data)))
)
system.time(
   print(callr::r(function(x) mean(x), args = list(shared_data)))
)
```

With `r_session`, the same shared object can be passed to multiple calls
without re-serializing the data each time:

``` r

rs <- callr::r_session$new()
system.time(mean(big_data))
system.time(sd(big_data))
system.time(print(rs$run(function(x) mean(x), args = list(shared_data))))
system.time(print(rs$run(function(x) sd(x), args = list(shared_data))))
```

### Using packages

You can use any R package in the child process, just make sure to refer
to it explicitly with the `::` operator. For example, the following code
creates an [igraph](https://github.com/igraph/rigraph) graph in the
child, and calculates some metrics of it.

``` r

callr::r(function() { g <- igraph::sample_gnp(1000, 4/1000); igraph::diameter(g) })
```


    #> [1] 11

### Error handling

callr copies errors from the child process back to the main R session:

``` r

callr::r(function() 1 + "A")
```


    #> Error:
    #> ! in callr subprocess.
    #> Caused by error in `1 + "A"`:
    #> ! non-numeric argument to binary operator
    #> Type .Last.error to see the more details.

callr sets the `.Last.error` variable, and after an error you can
inspect this for more details about the error, including stack traces
both from the main R process and the subprocess.

``` r

.Last.error
```


    #> Error:
    #> ! in callr subprocess.
    #> Caused by error in `1 + "A"`:
    #> ! non-numeric argument to binary operator
    #> ---
    #> Backtrace:
    #> 1. callr::r(function() 1 + "A")
    #> 2. callr:::get_result(output = out, options)
    #> 3. callr:::throw(callr_remote_error(remerr, output), parent = fix_msg(remerr[[3]
    #> ]))
    #> ---
    #> Subprocess backtrace:
    #> 1. base::.handleSimpleError(function (e) …
    #> 2. global h(simpleError(msg, call))

The error objects has two parts. The first belongs to the main process,
and the second belongs to the subprocess.

`.Last.error` also includes a stack trace, that includes both the main R
process and the subprocess:

The top part of the trace contains the frames in the main process, and
the bottom part contains the frames in the subprocess, starting with the
anonymous function.

### Standard output and error

By default, the standard output and error of the child is lost, but you
can request callr to redirect them to files, and then inspect the files
in the parent:

``` r

x <- callr::r(function() { print("hello world!"); message("hello again!") },
  stdout = "/tmp/out", stderr = "/tmp/err"
)
readLines("/tmp/out")
```

``` r

readLines("/tmp/err")
```


    #> [1] "hello again!"

With the `stdout` option, the standard output is collected and can be
examined once the child process finished. The `show = TRUE` options will
also show the output of the child, as it is printed, on the console of
the parent.

## Background R processes

[`r_bg()`](https://callr.r-lib.org/dev/reference/r_bg.md) is similar to
[`r()`](https://callr.r-lib.org/dev/reference/r.md) but it starts the R
process in the background. It returns an `r_process` R6 object, that
provides a rich API:

``` r

rp <- callr::r_bg(function() Sys.sleep(.2))
rp
```


    #> PROCESS 'R', running, pid 8234.

This is a list of all `r_process` methods:

``` r

ls(rp)
```


    #>  [1] "as_ps_handle"          "cleanup"               "clone"
    #>  [4] "finalize"              "format"                "get_cmdline"
    #>  [7] "get_cpu_times"         "get_end_time"          "get_error_connection"
    #> [10] "get_error_file"        "get_exe"               "get_exit_status"
    #> [13] "get_input_connection"  "get_input_file"        "get_memory_info"
    #> [16] "get_name"              "get_output_connection" "get_output_file"
    #> [19] "get_pid"               "get_poll_connection"   "get_result"
    #> [22] "get_start_time"        "get_status"            "get_username"
    #> [25] "get_wd"                "has_error_connection"  "has_input_connection"
    #> [28] "has_output_connection" "has_poll_connection"   "initialize"
    #> [31] "interrupt"             "is_alive"              "is_incomplete_error"
    #> [34] "is_incomplete_output"  "is_supervised"         "kill"
    #> [37] "kill_tree"             "poll_io"               "print"
    #> [40] "read_all_error"        "read_all_error_lines"  "read_all_output"
    #> [43] "read_all_output_lines" "read_error"            "read_error_bytes"
    #> [46] "read_error_lines"      "read_output"           "read_output_bytes"
    #> [49] "read_output_lines"     "resume"                "signal"
    #> [52] "supervise"             "suspend"               "wait"
    #> [55] "write_input"

These include all methods of the
[`processx::process`](http://processx.r-lib.org/reference/process.md)
superclass and the new
[`get_result()`](https://callr.r-lib.org/dev/reference/get_result.md)
method, to retrieve the R object returned by the function call. Some of
the handiest methods are:

- `get_exit_status()` to query the exit status of a finished process.
- [`get_result()`](https://callr.r-lib.org/dev/reference/get_result.md)
  to collect the return value of the R function call.
- `interrupt()` to send an interrupt to the process. This is equivalent
  to a `CTRL+C` key press, and the R process might ignore it.
- `is_alive()` to check if the process is alive.
- `kill()` to terminate the process.
- `poll_io()` to wait for any standard output, standard error, or the
  completion of the process, with a timeout.
- `read_*()` to read the standard output or error.
- `suspend()` and `resume()` to stop and continue a process.
- `wait()` to wait for the completion of the process, with a timeout.

## Multiple background R processes and `poll()`

Multiple background R processes are best managed with the
[`processx::poll()`](http://processx.r-lib.org/reference/poll.md)
function that waits for events (standard output/error or termination)
from multiple processes. It returns as soon as one process has generated
an event, or if its timeout has expired. The timeout is in milliseconds.

``` r

rp1 <- callr::r_bg(function() { Sys.sleep(1/2); "1 done" })
rp2 <- callr::r_bg(function() { Sys.sleep(1/1000); "2 done" })
processx::poll(list(rp1, rp2), 1000)
```


    #> [[1]]
    #>   output    error  process
    #> "silent" "silent" "silent"
    #>
    #> [[2]]
    #>  output   error process
    #> "ready" "ready" "ready"
    #>

``` r

rp2$get_result()
```


    #> [1] "2 done"

``` r

processx::poll(list(rp1), 1000)
```


    #> [[1]]
    #>  output   error process
    #> "ready" "ready" "ready"
    #>

``` r

rp1$get_result()
```


    #> [1] "1 done"

## Persistent R sessions

`r_session` is another
[`processx::process`](http://processx.r-lib.org/reference/process.md)
subclass that represents a persistent background R session:

``` r

rs <- callr::r_session$new()
rs
```


    #> R SESSION, alive, idle, pid 8286.

`r_session$run()` is a synchronous call, that works similarly to
[`r()`](https://callr.r-lib.org/dev/reference/r.md), but uses the
persistent session. `r_session$call()` starts the function call and
returns immediately. The `r_session$poll_process()` method or
[`processx::poll()`](http://processx.r-lib.org/reference/poll.md) can
then be used to wait for the completion or other events from one or more
R sessions, R processes or other
[`processx::process`](http://processx.r-lib.org/reference/process.md)
objects.

Once an R session is done with an asynchronous computation, its
`poll_process()` method returns `"ready"` and the `r_session$read()`
method can read out the result.

``` r

rs <- callr::r_session$new()
rs$run(function() runif(10))
```


    #>  [1] 0.1370374 0.4180452 0.7200562 0.3891246 0.2430931 0.5951371 0.1180691
    #>  [8] 0.6539942 0.7024945 0.5481112

``` r

rs$call(function() rnorm(10))
rs
```


    #> R SESSION, alive, busy, pid 8295.

``` r

rs$poll_process(2000)
```


    #> [1] "ready"

``` r

rs$read()
```


    #> $code
    #> [1] 200
    #>
    #> $message
    #> [1] "done callr-rs-result-1fba4494598c"
    #>
    #> $result
    #>  [1] -2.02343249 -1.78501178 -1.68857366  0.08416230  2.19768104 -0.42919725
    #>  [7] -0.30996316 -0.84093972  0.05452635 -0.63255990
    #>
    #> $stdout
    #> [1] ""
    #>
    #> $stderr
    #> [1] ""
    #>
    #> $error
    #> NULL
    #>
    #> attr(,"class")
    #> [1] "callr_session_result"

## Running `R CMD` commands

The [`rcmd()`](https://callr.r-lib.org/dev/reference/rcmd.md) function
calls an `R CMD` command. For example, you can call `R CMD INSTALL`,
`R CMD check` or `R CMD config` this way:

``` r

callr::rcmd("config", "CC")
```


    #> $status
    #> [1] 0
    #>
    #> $stdout
    #> [1] "gcc -std=gnu2x\n"
    #>
    #> $stderr
    #> [1] ""
    #>
    #> $timeout
    #> [1] FALSE
    #>
    #> $command
    #> [1] "/opt/R/4.6.0/lib/R/bin/R" "CMD"
    #> [3] "config"                   "CC"
    #>

This returns a list with three components: the standard output, the
standard error, and the exit (status) code of the `R CMD` command.

## Observability

callr is instrumented with [OpenTelemetry](https://opentelemetry.io/).
When an OpenTelemetry SDK (such as [otelsdk](https://otelsdk.r-lib.org))
is loaded and configured, callr emits spans for every R subprocess it
starts and propagates the W3C `traceparent` header into the subprocess,
so spans created inside the child become children of the parent span. No
code changes are needed in callr-using code — the existing entry points
start emitting telemetry as soon as an SDK is configured.

See
[`vignette("opentelemetry", package = "callr")`](https://callr.r-lib.org/dev/articles/opentelemetry.md)
for what callr emits, how subprocess context propagation works, and how
to test your own instrumentation on top of it.

## Configuration

### Environment variables

- `CALLR_NO_TEMP_DLLS`: If `true`, then callr does not use a temporary
  directory to copy the client DLL files from, in the subprocess. By
  default callr copies the DLL file that drives the callr subprocess
  into a temporary directory and loads it from there. This is mainly to
  avoid locking a DLL file in the package library, on Windows. If this
  default causes issues for you, set it to `true`, and then callr will
  use the DLL file from the installed processx package. See also
  <https://github.com/r-lib/callr/issues/273>.

- `CALLR_TMPDIR`: If set to a non-empty path, callr writes its auxiliary
  temporary files into this directory instead of the session’s
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html). The directory is
  created if it does not exist. The value is read on each use, so it can
  be changed at run time via
  [`Sys.setenv()`](https://rdrr.io/r/base/Sys.setenv.html). Note that
  paths cached at package load time (the package’s own `callr-env-` file
  and the client DLL location) only honor this setting if the variable
  was set before `callr` was loaded, typically by exporting it in the
  shell or via `.Renviron`. See also
  <https://github.com/r-lib/callr/issues/172>.

## Code of Conduct

Please note that the callr project is released with a [Contributor Code
of Conduct](https://callr.r-lib.org/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
