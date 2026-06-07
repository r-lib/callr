# Default value for the `repos` option in callr subprocesses

callr sets the `repos` option in subprocesses, to make sure that a CRAN
mirror is set up. This is because the subprocess cannot bring up the
menu of CRAN mirrors for the user to choose from.

## Usage

``` r
default_repos()
```

## Value

Named character vector, the default value of the `repos` option in callr
subprocesses.

## Examples

``` r
default_repos()
#>                                                          RSPM 
#> "https://packagemanager.posit.co/cran/__linux__/noble/latest" 
#>                                                          CRAN 
#>                                    "https://cran.rstudio.com" 
```
