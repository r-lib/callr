# Run an `R CMD` command

Run an `R CMD` command form within R. This will usually start another R
process, from a shell script.

## Usage

``` r
rcmd(
  cmd,
  cmdargs = character(),
  libpath = .libPaths(),
  repos = default_repos(),
  stdout = NULL,
  stderr = NULL,
  poll_connection = TRUE,
  echo = FALSE,
  show = FALSE,
  callback = NULL,
  block_callback = NULL,
  spinner = show && interactive(),
  system_profile = FALSE,
  user_profile = "project",
  env = rcmd_safe_env(),
  timeout = Inf,
  wd = ".",
  fail_on_status = FALSE,
  ...
)

rcmd_safe(
  cmd,
  cmdargs = character(),
  libpath = .libPaths(),
  repos = default_repos(),
  stdout = NULL,
  stderr = NULL,
  poll_connection = TRUE,
  echo = FALSE,
  show = FALSE,
  callback = NULL,
  block_callback = NULL,
  spinner = show && interactive(),
  system_profile = FALSE,
  user_profile = "project",
  env = rcmd_safe_env(),
  timeout = Inf,
  wd = ".",
  fail_on_status = FALSE,
  ...
)
```

## Arguments

- cmd:

  Command to run. See `R --help` from the command line for the various
  commands. In the current version of R (3.2.4) these are: `BATCH`,
  `COMPILE`, `SHLIB`, `INSTALL`, `REMOVE`, `build`, `check`, `LINK`,
  `Rprof`, `Rdconv`, `Rd2pdf`, `Rd2txt`, `Stangle`, `Sweave`, `Rdiff`,
  `config`, `javareconf`, `rtags`.

- cmdargs:

  Command line arguments.

- libpath:

  The library path. If `NULL`, then the library path is not modified at
  all in the subprocess. This is useful for subprocesses that should use
  the library path of a fresh R session, e.g. as set up by a project
  `.Rprofile`.

- repos:

  The `repos` option. If `NULL`, then no `repos` option is set. This
  options is only used if `user_profile` or `system_profile` is set
  `FALSE`, as it is set using the system or the user profile.

- stdout:

  Optionally a file name to send the standard output to.

- stderr:

  Optionally a file name to send the standard error to. It may be the
  same as `stdout`, in which case standard error is redirected to
  standard output. It can also be the special string `"2>&1"`, in which
  case standard error will be redirected to standard output.

- poll_connection:

  Whether to have a control connection to the process. This is used to
  transmit messages from the subprocess to the parent.

- echo:

  Whether to echo the complete command run by `rcmd`.

- show:

  Logical, whether to show the standard output on the screen while the
  child process is running. Note that this is independent of the
  `stdout` and `stderr` arguments. The standard error is not shown
  currently.

- callback:

  A function to call for each line of the standard output and standard
  error from the child process. It works together with the `show`
  option; i.e. if `show = TRUE`, and a callback is provided, then the
  output is shown of the screen, and the callback is also called.

- block_callback:

  A function to call for each block of the standard output and standard
  error. This callback is not line oriented, i.e. multiple lines or half
  a line can be passed to the callback.

- spinner:

  Whether to show a calming spinner on the screen while the child R
  session is running. By default it is shown if `show = TRUE` and the R
  session is interactive.

- system_profile:

  Whether to use the system profile file.

- user_profile:

  Whether to use the user's profile file. If this is `"project"`, then
  only the profile from the working directory is used, but the
  `R_PROFILE_USER` environment variable and the user level profile are
  not. See also "Security considerations" below.

- env:

  Environment variables to set for the child process.

- timeout:

  Timeout for the function call to finish. It can be a
  [base::difftime](https://rdrr.io/r/base/difftime.html) object, or a
  real number, meaning seconds. If the process does not finish before
  the timeout period expires, then a `system_command_timeout_error`
  error is thrown. `Inf` means no timeout.

- wd:

  Working directory to use for running the command. Defaults to the
  current working directory.

- fail_on_status:

  Whether to throw an R error if the command returns with a non-zero
  status code. By default no error is thrown.

- ...:

  Extra arguments are passed to
  [`processx::run()`](http://processx.r-lib.org/reference/run.md).

## Value

A list with the command line `$command`), standard output (`$stdout`),
standard error (`stderr`), exit status (`$status`) of the external
`R CMD` command, and whether a timeout was reached (`$timeout`).

## Details

Starting from `callr` 2.0.0, `rcmd()` has safer defaults, the same as
the `rcmd_safe()` default values. Use
[`rcmd_copycat()`](https://callr.r-lib.org/reference/rcmd_copycat.md)
for the old defaults.

## Security considerations

`callr` makes a copy of the user's `.Renviron` file and potentially of
the local or user `.Rprofile`, in the session temporary directory. Avoid
storing sensitive information such as passwords, in your environment
file or your profile, otherwise this information will get scattered in
various files, at least temporarily, until the subprocess finishes. You
can use the keyring package to avoid passwords in plain files.

## See also

Other R CMD commands:
[`rcmd_bg()`](https://callr.r-lib.org/reference/rcmd_bg.md),
[`rcmd_copycat()`](https://callr.r-lib.org/reference/rcmd_copycat.md)

## Examples

``` r
if (FALSE) {
rcmd("config", "CC")
}
```
