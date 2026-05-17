# Evaluate an expression in another R session, in the background

Starts evaluating an R function call in a background R process, and
returns immediately. Use `p$get_result()` to collect the result or to
throw an error if the background computation failed.

## Usage

``` r
r_bg(
  func,
  args = list(),
  libpath = .libPaths(),
  repos = default_repos(),
  stdout = "|",
  stderr = "|",
  poll_connection = TRUE,
  error = getOption("callr.error", "error"),
  cmdargs = c("--slave", "--no-save", "--no-restore"),
  system_profile = FALSE,
  user_profile = "project",
  env = rcmd_safe_env(),
  supervise = FALSE,
  package = FALSE,
  arch = "same",
  ...
)
```

## Arguments

- func:

  Function object to call in the new R process. The function should be
  self-contained and only refer to other functions and use variables
  explicitly from other packages using the `::` notation. By default the
  environment of the function is set to `.GlobalEnv` before passing it
  to the child process. (See the `package` option if you want to keep
  the environment.) Because of this, it is good practice to create an
  anonymous function and pass that to `callr`, instead of passing a
  function object from a (base or other) package. In particular

      r(.libPaths)

  does not work, because `.libPaths` is defined in a special
  environment, but

      r(function() .libPaths())

  works just fine.

- args:

  Arguments to pass to the function. Must be a list.

- libpath:

  The library path.

- repos:

  The `repos` option. If `NULL`, then no `repos` option is set. This
  options is only used if `user_profile` or `system_profile` is set
  `FALSE`, as it is set using the system or the user profile.

- stdout:

  The name of the file the standard output of the child R process will
  be written to. If the child process runs with the `--slave` option
  (the default), then the commands are not echoed and will not be shown
  in the standard output. Also note that you need to call
  [`print()`](https://rdrr.io/r/base/print.html) explicitly to show the
  output of the command(s). IF `NULL`, then standard output is not
  returned, but it is recorded and included in the error object if an
  error happens. Various special values for this argument such as `"|"`
  are explained in the `stdout` argument of
  [processx::process](http://processx.r-lib.org/reference/process.md).

- stderr:

  The name of the file the standard error of the child R process will be
  written to. In particular
  [`message()`](https://rdrr.io/r/base/message.html) sends output to the
  standard error. If nothing was sent to the standard error, then this
  file will be empty. This argument can be the same file as `stdout`, in
  which case they will be correctly interleaved. If this is the string
  `"2>&1"`, then standard error is redirected to standard output. IF
  `NULL`, then standard output is not returned, but it is recorded and
  included in the error object if an error happens. Various special
  values for this argument such as `"|"` are explained in the `stdout`
  argument of
  [processx::process](http://processx.r-lib.org/reference/process.md).

- poll_connection:

  Whether to have a control connection to the process. This is used to
  transmit messages from the subprocess to the main process.

- error:

  What to do if the remote process throws an error. See details below.

- cmdargs:

  Command line arguments to pass to the R process. Note that
  `c("-f", rscript)` is appended to this, `rscript` is the name of the
  script file to run. This contains a call to the supplied function and
  some error handling code.

- system_profile:

  Whether to use the system profile file.

- user_profile:

  Whether to use the user's profile file. If this is `"project"`, then
  only the profile from the working directory is used, but the
  `R_PROFILE_USER` environment variable and the user level profile are
  not. See also "Security considerations" below.

- env:

  Environment variables to set for the child process.

- supervise:

  Whether to register the process with a supervisor. If `TRUE`, the
  supervisor will ensure that the process is killed when the R process
  exits.

- package:

  Whether to keep the environment of `func` when passing it to the other
  package. Possible values are:

  - `FALSE`: reset the environment to `.GlobalEnv`. This is the default.

  - `TRUE`: keep the environment as is.

  - `pkg`: set the environment to the `pkg` package namespace.

- arch:

  Architecture to use in the child process, for multi-arch builds of R.
  By default the same as the main process. See
  [`supported_archs()`](https://callr.r-lib.org/dev/reference/supported_archs.md).
  If it contains a forward or backward slash character, then it is taken
  as the path to the R executable. Note that on Windows you need the
  path to `Rterm.exe`.

- ...:

  Extra arguments are passed to the
  [processx::process](http://processx.r-lib.org/reference/process.md)
  constructor.

## Value

An `r_process` object, which inherits from
[process](http://processx.r-lib.org/reference/process.md), so all
`process` methods can be called on it, and in addition it also has a
[`get_result()`](https://callr.r-lib.org/dev/reference/get_result.md)
method to collect the result.

## Draining standard output and error

With the default `stdout = "|"` and `stderr = "|"`, the child process
writes its output and error streams into OS pipes with a small fixed
buffer (typically 64 KB or less). If nothing drains these pipes, a
chatty child fills the buffer and then blocks on the next
[`write()`](https://rdrr.io/r/base/write.html). The child will not
terminate until the parent reads from the pipes, so `p$is_alive()` will
keep returning `TRUE` even though the work appears to be done.

To avoid this:

- Pass a filename to `stdout` / `stderr` to redirect output to files, or

- pass `NULL` to discard it, or

- periodically call `p$read_output()` and `p$read_error()` (or
  `p$read_all_output()` etc.) to drain the pipes while the process is
  running.

## Security considerations

`callr` makes a copy of the user's `.Renviron` file and potentially of
the local or user `.Rprofile`, in the session temporary directory. Avoid
storing sensitive information such as passwords, in your environment
file or your profile, otherwise this information will get scattered in
various files, at least temporarily, until the subprocess finishes. You
can use the keyring package to avoid passwords in plain files.

## Examples

``` r
if (FALSE) {
rx <- r_bg(function() 1 + 2)

# wait until it is done
rx$wait()
rx$is_alive()
rx$get_result()
}
```
