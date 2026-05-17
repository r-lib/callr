# Run an `R CMD` command in the background

The child process is started in the background, and the function return
immediately.

## Usage

``` r
rcmd_bg(
  cmd,
  cmdargs = character(),
  libpath = .libPaths(),
  stdout = "|",
  stderr = "|",
  poll_connection = TRUE,
  repos = default_repos(),
  system_profile = FALSE,
  user_profile = "project",
  env = rcmd_safe_env(),
  wd = ".",
  supervise = FALSE,
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

- repos:

  The `repos` option. If `NULL`, then no `repos` option is set. This
  options is only used if `user_profile` or `system_profile` is set
  `FALSE`, as it is set using the system or the user profile.

- system_profile:

  Whether to use the system profile file.

- user_profile:

  Whether to use the user's profile file. If this is `"project"`, then
  only the profile from the working directory is used, but the
  `R_PROFILE_USER` environment variable and the user level profile are
  not. See also "Security considerations" below.

- env:

  Environment variables to set for the child process.

- wd:

  Working directory to use for running the command. Defaults to the
  current working directory.

- supervise:

  Whether to register the process with a supervisor. If `TRUE`, the
  supervisor will ensure that the process is killed when the R process
  exits.

- ...:

  Extra arguments are passed to the
  [processx::process](http://processx.r-lib.org/reference/process.md)
  constructor.

## Value

It returns a [process](http://processx.r-lib.org/reference/process.md)
object.

## Security considerations

`callr` makes a copy of the user's `.Renviron` file and potentially of
the local or user `.Rprofile`, in the session temporary directory. Avoid
storing sensitive information such as passwords, in your environment
file or your profile, otherwise this information will get scattered in
various files, at least temporarily, until the subprocess finishes. You
can use the keyring package to avoid passwords in plain files.

## See also

Other R CMD commands:
[`rcmd()`](https://callr.r-lib.org/dev/reference/rcmd.md),
[`rcmd_copycat()`](https://callr.r-lib.org/dev/reference/rcmd_copycat.md)
