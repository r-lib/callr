# External R Process

An R process that runs in the background. This is an R6 class that
extends the
[processx::process](http://processx.r-lib.org/reference/process.md)
class. The process starts in the background, evaluates an R function
call, and then quits.

## Super class

[`processx::process`](http://processx.r-lib.org/reference/process.md)
-\> `r_process`

## Methods

### Public methods

- [`r_process$new()`](#method-r_process-initialize)

- [`r_process$get_result()`](#method-r_process-get_result)

- [`r_process$cleanup()`](#method-r_process-cleanup)

- [`r_process$finalize()`](#method-r_process-finalize)

- [`r_process$clone()`](#method-r_process-clone)

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
- [`processx::process$print()`](http://processx.r-lib.org/reference/process.html#method-print)
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

### `r_process$new()`

Start a new R process in the background.

#### Usage

    r_process$new(options)

#### Arguments

- `options`:

  A list of options created via
  [`r_process_options()`](https://callr.r-lib.org/reference/r_process_options.md).

#### Returns

A new `r_process` object.

------------------------------------------------------------------------

### `r_process$get_result()`

Return the result, an R object, from a finished background R process. If
the process has not finished yet, it throws an error. (You can use
`wait()` method (see
[processx::process](http://processx.r-lib.org/reference/process.md)) to
wait for the process to finish, optionally with a timeout.) You can also
use [`processx::poll()`](http://processx.r-lib.org/reference/poll.md) to
wait for the end of the process, together with other processes or
events.

#### Usage

    r_process$get_result()

#### Returns

The return value of the R expression evaluated in the R process.

------------------------------------------------------------------------

### `r_process$cleanup()`

Delete the temporary files created for this R process. Only call this if
you are sure that the process is done and you don't need the result
anymore. If you don't call this method explicitly, the temporary files
will be deleted when the process object is garbage collected.

#### Usage

    r_process$cleanup()

------------------------------------------------------------------------

### `r_process$finalize()`

Clean up temporary files once an R process has finished and its handle
is garbage collected.

#### Usage

    r_process$finalize()

------------------------------------------------------------------------

### `r_process$clone()`

The objects of this class are cloneable with this method.

#### Usage

    r_process$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) {
## List all options and their default values:
r_process_options()

## Start an R process in the background, wait for it, get result
opts <- r_process_options(func = function() 1 + 1)
rp <- r_process$new(opts)
rp$wait()
rp$get_result()
}
```
