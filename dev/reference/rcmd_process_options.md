# Create options for an [rcmd_process](https://callr.r-lib.org/dev/reference/rcmd_process.md) object

Create options for an
[rcmd_process](https://callr.r-lib.org/dev/reference/rcmd_process.md)
object

## Usage

``` r
rcmd_process_options(...)
```

## Arguments

- ...:

  Options to override, named arguments.

## Value

A list of options.

`rcmd_process_options()` creates a set of options to initialize a new
object from the `rcmd_process` class. Its arguments must be named, the
names are used as option names. The options correspond to (some of) the
arguments of the
[`rcmd()`](https://callr.r-lib.org/dev/reference/rcmd.md) function. At
least the `cmd` option must be specified, to select the `R CMD`
subcommand to run. Typically `cmdargs` is specified as well, to supply
more arguments to `R CMD`.

## Examples

``` r
## List all options and their default values:
rcmd_process_options()
#> $cmd
#> NULL
#> 
#> $cmdargs
#> character(0)
#> 
#> $libpath
#> [1] "/home/runner/work/_temp/Library" "/opt/R/4.5.3/lib/R/site-library"
#> [3] "/opt/R/4.5.3/lib/R/library"     
#> 
#> $stdout
#> [1] "|"
#> 
#> $stderr
#> [1] "|"
#> 
#> $poll_connection
#> [1] TRUE
#> 
#> $repos
#>                                                          RSPM 
#> "https://packagemanager.posit.co/cran/__linux__/noble/latest" 
#>                                                          CRAN 
#>                                    "https://cran.rstudio.com" 
#> 
#> $system_profile
#> [1] FALSE
#> 
#> $user_profile
#> [1] "project"
#> 
#> $env
#>             CYGWIN            R_TESTS          R_BROWSER 
#> "nodosfilewarning"                 ""            "false" 
#>        R_PDFVIEWER 
#>            "false" 
#> 
#> $wd
#> [1] "."
#> 
#> $supervise
#> [1] FALSE
#> 
#> $extra
#> list()
#> 
#> $arch
#> [1] "same"
#> 
```
