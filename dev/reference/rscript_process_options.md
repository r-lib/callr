# Create options for an [rscript_process](https://callr.r-lib.org/dev/reference/rscript_process.md) object

Create options for an
[rscript_process](https://callr.r-lib.org/dev/reference/rscript_process.md)
object

## Usage

``` r
rscript_process_options(...)
```

## Arguments

- ...:

  Options to override, named arguments.

## Value

A list of options.

`rscript_process_options()` creates a set of options to initialize a new
object from the `rscript_process` class. Its arguments must be named,
the names are used as option names. The options correspond to (some of)
the arguments of the
[`rscript()`](https://callr.r-lib.org/dev/reference/rscript.md)
function. At least the `script` option must be specified, the script
file to run.

## Examples

``` r
## List all options and their default values:
rscript_process_options()
#> $script
#> NULL
#> 
#> $cmdargs
#> character(0)
#> 
#> $libpath
#> [1] "/home/runner/work/_temp/Library" "/opt/R/4.6.0/lib/R/site-library"
#> [3] "/opt/R/4.6.0/lib/R/library"     
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
#> $color
#> [1] FALSE
#> 
#> $extra
#> list()
#> 
#> $arch
#> [1] "same"
#> 
```
