# Create options for an [r_session](https://callr.r-lib.org/dev/reference/r_session.md) object

Create options for an
[r_session](https://callr.r-lib.org/dev/reference/r_session.md) object

## Usage

``` r
r_session_options(...)
```

## Arguments

- ...:

  Options to override, named arguments.

## Value

Named list of options.

The current options are:

- `libpath`: Library path for the subprocess. By default the same as the
  *current* library path. I.e. *not* necessarily the library path of a
  fresh R session.) If `NULL`, then the library path is not modified at
  all in the subprocess, which is useful for subprocesses that should
  use the library path of a fresh R session, e.g. as set up by a project
  `.Rprofile`.

- `repos`: `repos` option for the subprocess. By default the current
  value of the main process.

- `stdout`: Standard output of the sub-process. This can be `NULL` or a
  pipe: `"|"`. If it is a pipe then the output of the subprocess is not
  included in the responses, but you need to poll and read it manually.
  This is for experts. Note that this option is not used for the startup
  phase that currently always runs with `stdout = "|"`.

- `stderr`: Similar to `stdout`, but for the standard error. Like
  `stdout`, it is not used for the startup phase, which runs with
  `stderr = "|"`.

- `error`: See 'Error handling' in
  [`r()`](https://callr.r-lib.org/dev/reference/r.md).

- `cmdargs`: See the same argument of
  [`r()`](https://callr.r-lib.org/dev/reference/r.md). (Its default
  might be different, though.)

- `system_profile`: See the same argument of
  [`r()`](https://callr.r-lib.org/dev/reference/r.md).

- `user_profile`: See the same argument of
  [`r()`](https://callr.r-lib.org/dev/reference/r.md).

- `env`: See the same argument of
  [`r()`](https://callr.r-lib.org/dev/reference/r.md).

- `load_hook`: `NULL`, or code (quoted) to run in the sub-process at
  start up. (I.e. not for every single
  [`run()`](http://processx.r-lib.org/reference/run.md) call.)

- `extra`: List of extra arguments to pass to
  [processx::process](http://processx.r-lib.org/reference/process.md).

Call `r_session_options()` to see the default values.
`r_session_options()` might contain undocumented entries, you cannot
change these.

## Examples

``` r
r_session_options()
#> $func
#> NULL
#> 
#> $args
#> NULL
#> 
#> $libpath
#> [1] "/home/runner/work/_temp/Library" "/opt/R/4.6.0/lib/R/site-library"
#> [3] "/opt/R/4.6.0/lib/R/library"     
#> 
#> $repos
#>                                                          RSPM 
#> "https://packagemanager.posit.co/cran/__linux__/noble/latest" 
#>                                                          CRAN 
#>                                    "https://cran.rstudio.com" 
#> 
#> $stdout
#> NULL
#> 
#> $stderr
#> NULL
#> 
#> $error
#> [1] "error"
#> 
#> $cmdargs
#> [1] "--no-readline" "--slave"       "--no-save"     "--no-restore" 
#> 
#> $system_profile
#> [1] FALSE
#> 
#> $user_profile
#> [1] "project"
#> 
#> $env
#>   TERM 
#> "dumb" 
#> 
#> $supervise
#> [1] FALSE
#> 
#> $load_hook
#> NULL
#> 
#> $extra
#> list()
#> 
#> $arch
#> [1] "same"
#> 
```
