# Run an R process that mimics the current R process

Differences to [`r()`](https://callr.r-lib.org/reference/r.md):

- No extra repositories are set up.

- The `--no-save`, `--no-restore` command line arguments are not used.
  (But `--slave` still is.)

- The system profile and the user profile are loaded.

- No extra environment variables are set up.

## Usage

``` r
r_copycat(
  func,
  args = list(),
  libpath = .libPaths(),
  repos = getOption("repos"),
  cmdargs = "--slave",
  system_profile = TRUE,
  user_profile = TRUE,
  env = character(),
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

  The library path. If `NULL`, then the library path is not modified at
  all in the subprocess. This is useful for subprocesses that should use
  the library path of a fresh R session, e.g. as set up by a project
  `.Rprofile`.

- repos:

  The `repos` option. If `NULL`, then no `repos` option is set. This
  options is only used if `user_profile` or `system_profile` is set
  `FALSE`, as it is set using the system or the user profile.

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

- ...:

  Additional arguments are passed to
  [`r()`](https://callr.r-lib.org/reference/r.md).

## Security considerations

`callr` makes a copy of the user's `.Renviron` file and potentially of
the local or user `.Rprofile`, in the session temporary directory. Avoid
storing sensitive information such as passwords, in your environment
file or your profile, otherwise this information will get scattered in
various files, at least temporarily, until the subprocess finishes. You
can use the keyring package to avoid passwords in plain files.

## See also

Other callr functions: [`r()`](https://callr.r-lib.org/reference/r.md),
[`r_vanilla()`](https://callr.r-lib.org/reference/r_vanilla.md)
