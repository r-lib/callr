# External `R CMD` Process

An `R CMD *` command that runs in the background. This is an R6 class
that extends the
[processx::process](http://processx.r-lib.org/reference/process.md)
class.

## Super class

[`processx::process`](http://processx.r-lib.org/reference/process.md)
-\> `rcmd_process`

## Methods

### Public methods

- [`rcmd_process$new()`](#method-rcmd_process-initialize)

- [`rcmd_process$clone()`](#method-rcmd_process-clone)

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

### `rcmd_process$new()`

Start an `R CMD` process.

#### Usage

    rcmd_process$new(options)

#### Arguments

- `options`:

  A list of options created via
  [`rcmd_process_options()`](https://callr.r-lib.org/dev/reference/rcmd_process_options.md).

#### Returns

A new `rcmd_process` object.

------------------------------------------------------------------------

### `rcmd_process$clone()`

The objects of this class are cloneable with this method.

#### Usage

    rcmd_process$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) {
options <- rcmd_process_options(cmd = "config", cmdargs = "CC")
rp <- rcmd_process$new(options)
rp$wait()
rp$read_output_lines()
}
```
