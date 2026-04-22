# Create options for an [r_process](https://callr.r-lib.org/dev/reference/r_process.md) object

Create options for an
[r_process](https://callr.r-lib.org/dev/reference/r_process.md) object

## Usage

``` r
r_process_options(...)
```

## Arguments

- ...:

  Options to override, named arguments.

## Value

A list of options.

`r_process_options()` creates a set of options to initialize a new
object from the `r_process` class. Its arguments must be named, the
names are used as option names. The options correspond to (some of) the
arguments of the [`r()`](https://callr.r-lib.org/dev/reference/r.md)
function. At least the `func` option must be specified, this is the R
function to run in the background.

## Examples

``` r
## List all options and their default values:
r_process_options()
#> $func
#> NULL
#> 
#> $args
#> list()
#> 
#> $libpath
#> [1] "/home/runner/work/_temp/Library" "/opt/R/4.5.3/lib/R/site-library"
#> [3] "/opt/R/4.5.3/lib/R/library"     
#> 
#> $repos
#>                                                          RSPM 
#> "https://packagemanager.posit.co/cran/__linux__/noble/latest" 
#>                                                          CRAN 
#>                                    "https://cran.rstudio.com" 
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
#> $error
#> [1] "error"
#> 
#> $cmdargs
#> [1] "--slave"      "--no-save"    "--no-restore"
#> 
#> $system_profile
#> [1] FALSE
#> 
#> $user_profile
#> [1] "project"
#> 
#> $env
#> character(0)
#> 
#> $supervise
#> [1] FALSE
#> 
#> $load_hook
#>  [1] "{\n"                                                                                  
#>  [2] "    while (\"tools:callr\" %in% search()) {\n"                                        
#>  [3] "        detach(\"tools:callr\")\n"                                                    
#>  [4] "    }\n"                                                                              
#>  [5] "    env <- readRDS(\"/tmp/RtmpGC1JR2/callr-env-1f014800a1e1\")\n"                     
#>  [6] "    do.call(\"attach\", list(env, pos = length(search()), name = \"tools:callr\"))\n" 
#>  [7] "    data <- env$`__callr_data__`\n"                                                   
#>  [8] "    data$pxlib <- data$load_client_lib(data$sofile[[paste0(\"arch-\", \n"             
#>  [9] "        .Platform$r_arch)]], data$pxdir)\n"                                           
#> [10] "    options(error = function() invokeRestart(\"abort\"))\n"                           
#> [11] "    rm(list = c(\"data\", \"env\"))\n"                                                
#> [12] "    lapply(c(\"R_ENVIRON\", \"R_ENVIRON_USER\", \"R_PROFILE\", \"R_PROFILE_USER\", \n"
#> [13] "        \"R_LIBS\", \"R_LIBS_USER\", \"R_LIBS_SITE\"), function(var) {\n"             
#> [14] "        bakvar <- paste0(\"CALLR_\", var, \"_BAK\")\n"                                
#> [15] "        val <- Sys.getenv(bakvar, NA_character_)\n"                                   
#> [16] "        if (!is.na(val)) {\n"                                                         
#> [17] "            do.call(\"Sys.setenv\", structure(list(val), names = var))\n"             
#> [18] "        }\n"                                                                          
#> [19] "        else {\n"                                                                     
#> [20] "            Sys.unsetenv(var)\n"                                                      
#> [21] "        }\n"                                                                          
#> [22] "        Sys.unsetenv(bakvar)\n"                                                       
#> [23] "    })\n"                                                                             
#> [24] "    Sys.unsetenv(\"CALLR_CHILD_R_LIBS\")\n"                                           
#> [25] "    Sys.unsetenv(\"CALLR_CHILD_R_LIBS_SITE\")\n"                                      
#> [26] "    Sys.unsetenv(\"CALLR_CHILD_R_LIBS_USER\")\n"                                      
#> [27] "}\n"                                                                                  
#> 
#> $extra
#> list()
#> 
#> $package
#> [1] FALSE
#> 
#> $arch
#> [1] "same"
#> 
```
