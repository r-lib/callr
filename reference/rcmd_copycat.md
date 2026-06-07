# Call and `R CMD` command, while mimicking the current R session

This function is similar to
[`rcmd()`](https://callr.r-lib.org/reference/rcmd.md), but it has
slightly different defaults:

- The `repos` options is unchanged.

- No extra environment variables are defined.

## Usage

``` r
rcmd_copycat(
  cmd,
  cmdargs = character(),
  libpath = .libPaths(),
  repos = getOption("repos"),
  env = character(),
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

- env:

  Environment variables to set for the child process.

- ...:

  Additional arguments are passed to
  [`rcmd()`](https://callr.r-lib.org/reference/rcmd.md).

## Security considerations

`callr` makes a copy of the user's `.Renviron` file and potentially of
the local or user `.Rprofile`, in the session temporary directory. Avoid
storing sensitive information such as passwords, in your environment
file or your profile, otherwise this information will get scattered in
various files, at least temporarily, until the subprocess finishes. You
can use the keyring package to avoid passwords in plain files.

## See also

Other R CMD commands:
[`rcmd()`](https://callr.r-lib.org/reference/rcmd.md),
[`rcmd_bg()`](https://callr.r-lib.org/reference/rcmd_bg.md)
