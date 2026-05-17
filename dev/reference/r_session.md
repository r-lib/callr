# External R Session

A permanent R session that runs in the background. This is an R6 class
that extends the
[processx::process](http://processx.r-lib.org/reference/process.md)
class.

The process is started at the creation of the object, and then it can be
used to evaluate R function calls, one at a time.

## Super class

[`processx::process`](http://processx.r-lib.org/reference/process.md)
-\> `r_session`

## Public fields

- `status`:

  Status codes returned by `read()`.

## Methods

### Public methods

- [`r_session$new()`](#method-r_session-initialize)

- [`r_session$run()`](#method-r_session-run)

- [`r_session$run_with_output()`](#method-r_session-run_with_output)

- [`r_session$call()`](#method-r_session-call)

- [`r_session$poll_process()`](#method-r_session-poll_process)

- [`r_session$get_state()`](#method-r_session-get_state)

- [`r_session$get_running_time()`](#method-r_session-get_running_time)

- [`r_session$read()`](#method-r_session-read)

- [`r_session$close()`](#method-r_session-close)

- [`r_session$traceback()`](#method-r_session-traceback)

- [`r_session$debug()`](#method-r_session-debug)

- [`r_session$attach()`](#method-r_session-attach)

- [`r_session$print()`](#method-r_session-print)

- [`r_session$clone()`](#method-r_session-clone)

Inherited methods

- [`processx::process$as_ps_handle()`](http://processx.r-lib.org/reference/process.html#method-as_ps_handle)
- [`processx::process$format()`](http://processx.r-lib.org/reference/process.html#method-format)
- [`processx::process$get_cmdline()`](http://processx.r-lib.org/reference/process.html#method-get_cmdline)
- [`processx::process$get_cpu_times()`](http://processx.r-lib.org/reference/process.html#method-get_cpu_times)
- [`processx::process$get_end_time()`](http://processx.r-lib.org/reference/process.html#method-get_end_time)
- [`processx::process$get_error_connection()`](http://processx.r-lib.org/reference/process.html#method-get_error_connection)
- [`processx::process$get_error_file()`](http://processx.r-lib.org/reference/process.html#method-get_error_file)
- [`processx::process$get_exe()`](http://processx.r-lib.org/reference/process.html#method-get_exe)
- [`processx::process$get_exit_status()`](http://processx.r-lib.org/reference/process.html#method-get_exit_status)
- [`processx::process$get_input_connection()`](http://processx.r-lib.org/reference/process.html#method-get_input_connection)
- [`processx::process$get_input_file()`](http://processx.r-lib.org/reference/process.html#method-get_input_file)
- [`processx::process$get_memory_info()`](http://processx.r-lib.org/reference/process.html#method-get_memory_info)
- [`processx::process$get_name()`](http://processx.r-lib.org/reference/process.html#method-get_name)
- [`processx::process$get_output_connection()`](http://processx.r-lib.org/reference/process.html#method-get_output_connection)
- [`processx::process$get_output_file()`](http://processx.r-lib.org/reference/process.html#method-get_output_file)
- [`processx::process$get_pid()`](http://processx.r-lib.org/reference/process.html#method-get_pid)
- [`processx::process$get_poll_connection()`](http://processx.r-lib.org/reference/process.html#method-get_poll_connection)
- [`processx::process$get_result()`](http://processx.r-lib.org/reference/process.html#method-get_result)
- [`processx::process$get_start_time()`](http://processx.r-lib.org/reference/process.html#method-get_start_time)
- [`processx::process$get_status()`](http://processx.r-lib.org/reference/process.html#method-get_status)
- [`processx::process$get_username()`](http://processx.r-lib.org/reference/process.html#method-get_username)
- [`processx::process$get_wd()`](http://processx.r-lib.org/reference/process.html#method-get_wd)
- [`processx::process$has_error_connection()`](http://processx.r-lib.org/reference/process.html#method-has_error_connection)
- [`processx::process$has_input_connection()`](http://processx.r-lib.org/reference/process.html#method-has_input_connection)
- [`processx::process$has_output_connection()`](http://processx.r-lib.org/reference/process.html#method-has_output_connection)
- [`processx::process$has_poll_connection()`](http://processx.r-lib.org/reference/process.html#method-has_poll_connection)
- [`processx::process$interrupt()`](http://processx.r-lib.org/reference/process.html#method-interrupt)
- [`processx::process$is_alive()`](http://processx.r-lib.org/reference/process.html#method-is_alive)
- [`processx::process$is_incomplete_error()`](http://processx.r-lib.org/reference/process.html#method-is_incomplete_error)
- [`processx::process$is_incomplete_output()`](http://processx.r-lib.org/reference/process.html#method-is_incomplete_output)
- [`processx::process$is_supervised()`](http://processx.r-lib.org/reference/process.html#method-is_supervised)
- [`processx::process$kill()`](http://processx.r-lib.org/reference/process.html#method-kill)
- [`processx::process$kill_tree()`](http://processx.r-lib.org/reference/process.html#method-kill_tree)
- [`processx::process$poll_io()`](http://processx.r-lib.org/reference/process.html#method-poll_io)
- [`processx::process$read_all_error()`](http://processx.r-lib.org/reference/process.html#method-read_all_error)
- [`processx::process$read_all_error_lines()`](http://processx.r-lib.org/reference/process.html#method-read_all_error_lines)
- [`processx::process$read_all_output()`](http://processx.r-lib.org/reference/process.html#method-read_all_output)
- [`processx::process$read_all_output_lines()`](http://processx.r-lib.org/reference/process.html#method-read_all_output_lines)
- [`processx::process$read_error()`](http://processx.r-lib.org/reference/process.html#method-read_error)
- [`processx::process$read_error_bytes()`](http://processx.r-lib.org/reference/process.html#method-read_error_bytes)
- [`processx::process$read_error_lines()`](http://processx.r-lib.org/reference/process.html#method-read_error_lines)
- [`processx::process$read_output()`](http://processx.r-lib.org/reference/process.html#method-read_output)
- [`processx::process$read_output_bytes()`](http://processx.r-lib.org/reference/process.html#method-read_output_bytes)
- [`processx::process$read_output_lines()`](http://processx.r-lib.org/reference/process.html#method-read_output_lines)
- [`processx::process$resume()`](http://processx.r-lib.org/reference/process.html#method-resume)
- [`processx::process$signal()`](http://processx.r-lib.org/reference/process.html#method-signal)
- [`processx::process$supervise()`](http://processx.r-lib.org/reference/process.html#method-supervise)
- [`processx::process$suspend()`](http://processx.r-lib.org/reference/process.html#method-suspend)
- [`processx::process$wait()`](http://processx.r-lib.org/reference/process.html#method-wait)
- [`processx::process$write_input()`](http://processx.r-lib.org/reference/process.html#method-write_input)

------------------------------------------------------------------------

### `r_session$new()`

creates a new R background process. It can wait for the process to start
up (`wait = TRUE`), or return immediately, i.e. before the process is
actually ready to run. In the latter case you may call the
`poll_process()` method to make sure it is ready.

#### Usage

    r_session$new(options = r_session_options(), wait = TRUE, wait_timeout = 3000)

#### Arguments

- `options`:

  A list of options created via
  [`r_session_options()`](https://callr.r-lib.org/dev/reference/r_session_options.md).

- `wait`:

  Whether to wait for the R process to start and be ready for running
  commands.

- `wait_timeout`:

  Timeout for waiting for the R process to start, in milliseconds.

#### Returns

An `r_session` object.

------------------------------------------------------------------------

### `r_session$run()`

Similar to [`r()`](https://callr.r-lib.org/dev/reference/r.md), but runs
the function in a permanent background R session. It throws an error if
the function call generated an error in the child process.

#### Usage

    r_session$run(func, args = list(), package = NULL)

#### Arguments

- `func`:

  Function object to call in the background R process. Please read the
  notes for the similar argument of
  [`r()`](https://callr.r-lib.org/dev/reference/r.md).

- `args`:

  Arguments to pass to the function. Must be a list.

- `package`:

  Whether to keep the environment of `func` when passing it to the other
  package. Possible values are:

  - `NULL` (the default): equivalent to `TRUE` if `func` inherits from
    `"crate"` (i.e. was created with
    [`carrier::crate()`](https://rdrr.io/pkg/carrier/man/crate.html)),
    and `FALSE` otherwise.

  - `FALSE`: reset the environment to `.GlobalEnv`.

  - `TRUE`: keep the environment as is.

  - `pkg`: set the environment to the `pkg` package namespace.

#### Returns

The return value of the R expression.

------------------------------------------------------------------------

### `r_session$run_with_output()`

Similar to `$run()`, but returns the standard output and error of the
child process as well. It does not throw on errors, but returns a
non-`NULL` `error` member in the result list.

#### Usage

    r_session$run_with_output(func, args = list(), package = NULL)

#### Arguments

- `func`:

  Function object to call in the background R process. Please read the
  notes for the similar argument of
  [`r()`](https://callr.r-lib.org/dev/reference/r.md).

- `args`:

  Arguments to pass to the function. Must be a list.

- `package`:

  Whether to keep the environment of `func` when passing it to the other
  package. Possible values are:

  - `NULL` (the default): equivalent to `TRUE` if `func` inherits from
    `"crate"` (i.e. was created with
    [`carrier::crate()`](https://rdrr.io/pkg/carrier/man/crate.html)),
    and `FALSE` otherwise.

  - `FALSE`: reset the environment to `.GlobalEnv`.

  - `TRUE`: keep the environment as is.

  - `pkg`: set the environment to the `pkg` package namespace.

#### Returns

A list with the following entries.

- `result`: The value returned by `func`. On error this is `NULL`.

- `stdout`: The standard output of the process while evaluating

- `stderr`: The standard error of the process while evaluating the
  `func` call.

- `error`: On error it contains an error object, that contains the error
  thrown in the subprocess. Otherwise it is `NULL`.

- `code`, `message`: These fields are used by call internally and you
  can ignore them.

------------------------------------------------------------------------

### `r_session$call()`

Starts running a function in the background R session, and returns
immediately. To check if the function is done, call the `poll_process()`
method.

#### Usage

    r_session$call(func, args = list(), package = NULL)

#### Arguments

- `func`:

  Function object to call in the background R process. Please read the
  notes for the similar argument of
  [`r()`](https://callr.r-lib.org/dev/reference/r.md).

- `args`:

  Arguments to pass to the function. Must be a list.

- `package`:

  Whether to keep the environment of `func` when passing it to the other
  package. Possible values are:

  - `NULL` (the default): equivalent to `TRUE` if `func` inherits from
    `"crate"` (i.e. was created with
    [`carrier::crate()`](https://rdrr.io/pkg/carrier/man/crate.html)),
    and `FALSE` otherwise.

  - `FALSE`: reset the environment to `.GlobalEnv`.

  - `TRUE`: keep the environment as is.

  - `pkg`: set the environment to the `pkg` package namespace.

------------------------------------------------------------------------

### `r_session$poll_process()`

Poll the R session with a timeout. If the session has finished the
computation, it returns with `"ready"`. If the timeout is reached, it
returns with `"timeout"`.

#### Usage

    r_session$poll_process(timeout)

#### Arguments

- `timeout`:

  Timeout period in milliseconds.

#### Returns

Character string `"ready"` or `"timeout"`.

------------------------------------------------------------------------

### `r_session$get_state()`

Return the state of the R session.

#### Usage

    r_session$get_state()

#### Returns

Possible values:

- `"starting"`: starting up,

- `"idle"`: ready to compute,

- `"busy"`: computing right now,

- `"finished"`: the R process has finished.

------------------------------------------------------------------------

### `r_session$get_running_time()`

Returns the elapsed time since the R process has started, and the
elapsed time since the current computation has started. The latter is
`NA` if there is no active computation.

#### Usage

    r_session$get_running_time()

#### Returns

Named vector of `POSIXct` objects. The names are `"total"` and
`"current"`.

------------------------------------------------------------------------

### `r_session$read()`

Reads an event from the child process, if there is one available. Events
might signal that the function call has finished, or they can be
progress report events.

This is a low level function that you only need to use if you want to
process events (messages) from the R session manually.

#### Usage

    r_session$read()

#### Returns

`NULL` if no events are available. Otherwise a named list, which is also
a `callr_session_result` object. The list always has a `code` entry
which is the type of the event. See also
`r_session$public_fields$status` for symbolic names of the event types.

- `200`: (`DONE`) The computation is done, and the event includes the
  result, in the same form as for the
  [`run()`](http://processx.r-lib.org/reference/run.md) method.

- `201`: (`STARTED`) An R session that was in 'starting' state is ready
  to go.

- `202`: (`ATTACH_DONE`) Used by the
  [`attach()`](https://rdrr.io/r/base/attach.html) method.

- `301`: (`MSG`) A message from the subprocess. The message is a
  condition object with class `callr_message`. (It typically has other
  classes, e.g. `cli_message` for output from the cli package.)

- `500`: (`EXITED`) The R session finished cleanly. This means that the
  evaluated expression quit R.

- `501`: (`CRASHED`) The R session crashed or was killed.

- `502`: (`CLOSED`) The R session closed its end of the connection that
  callr uses for communication.

------------------------------------------------------------------------

### `r_session$close()`

Terminate the current computation and the R process. The session object
will be in `"finished"` state after this.

#### Usage

    r_session$close(grace = 1000)

#### Arguments

- `grace`:

  Grace period in milliseconds, to wait for the subprocess to exit
  cleanly, after its standard input is closed. If the process is still
  running after this period, it will be killed.

------------------------------------------------------------------------

### `r_session$traceback()`

The [`traceback()`](https://rdrr.io/r/base/traceback.html) method can be
used after an error in the R subprocess. It is equivalent to the
[`base::traceback()`](https://rdrr.io/r/base/traceback.html) call, in
the subprocess.

On callr version 3.8.0 and above, you need to set the `callr.traceback`
option to `TRUE` (in the main process) to make the subprocess save the
trace on error. This is because saving the trace can be costly for large
objects passed as arguments.

#### Usage

    r_session$traceback()

#### Returns

The same output as from
[`base::traceback()`](https://rdrr.io/r/base/traceback.html)

------------------------------------------------------------------------

### `r_session$debug()`

Interactive debugger to inspect the dumped frames in the subprocess,
after an error. See more at
[r_session_debug](https://callr.r-lib.org/dev/reference/r_session_debug.md).

On callr version 3.8.0 and above, you need to set the `callr.traceback`
option to `TRUE` (in the main process) to make the subprocess dump
frames on error. This is because saving the frames can be costly for
large objects passed as arguments.

#### Usage

    r_session$debug()

------------------------------------------------------------------------

### `r_session$attach()`

Experimental function that provides a REPL (Read-Eval-Print-Loop) to the
subprocess.

#### Usage

    r_session$attach()

------------------------------------------------------------------------

### `r_session$print()`

Print method for an `r_session`.

#### Usage

    r_session$print(...)

#### Arguments

- `...`:

  Arguments are not used currently.

------------------------------------------------------------------------

### `r_session$clone()`

The objects of this class are cloneable with this method.

#### Usage

    r_session$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) {
rs <- r_session$new()

rs$run(function() 1 + 2)

rs$call(function() Sys.sleep(1))
rs$get_state()

rs$poll_process(-1)
rs$get_state()
rs$read()
}
```
