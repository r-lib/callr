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
#>  [5] "    env <- readRDS(\"/tmp/Rtmpa3G0cx/callr-env-1d367f4889f3\")\n"                                                  
#>  [6] "    has_otel <- nzchar(Sys.getenv(\"TRACEPARENT\")) && requireNamespace(\"otel\", \n"                              
#>  [7] "        quietly = TRUE)\n"                                                                                         
#>  [8] "    assign(envir = env$`__callr_data__`, \"has_otel\", has_otel)\n"                                                
#>  [9] "    if (has_otel) {\n"                                                                                             
#> [10] "        hdrs <- as.list(c(traceparent = Sys.getenv(\"TRACEPARENT\"), \n"                                           
#> [11] "            tracestate = Sys.getenv(\"TRACESTATE\"), baggage = Sys.getenv(\"BAGGAGE\")))\n"                        
#> [12] "        prtctx <- otel::extract_http_context(hdrs)\n"                                                              
#> [13] "        assign(envir = env$`__callr_data__`, \"otel_span\", otel::start_local_active_span(\"callr subprocess\", \n"
#> [14] "            options = list(parent = prtctx), activation_scope = .GlobalEnv, \n"                                    
#> [15] "            end_on_exit = TRUE))\n"                                                                                
#> [16] "    }\n"                                                                                                           
#> [17] "    do.call(\"attach\", list(env, pos = length(search()), name = \"tools:callr\"))\n"                              
#> [18] "    data <- env$`__callr_data__`\n"                                                                                
#> [19] "    data$pxlib <- data$load_client_lib(data$sofile[[paste0(\"arch-\", \n"                                          
#> [20] "        .Platform$r_arch)]], data$pxdir)\n"                                                                        
#> [21] "    options(error = function() invokeRestart(\"abort\"))\n"                                                        
#> [22] "    rm(list = c(\"data\", \"env\", \"has_otel\"))\n"                                                               
#> [23] "    lapply(c(\"R_ENVIRON\", \"R_ENVIRON_USER\", \"R_PROFILE\", \"R_PROFILE_USER\", \n"                             
#> [24] "        \"R_LIBS\", \"R_LIBS_USER\", \"R_LIBS_SITE\"), function(var) {\n"                                          
#> [25] "        bakvar <- paste0(\"CALLR_\", var, \"_BAK\")\n"                                                             
#> [26] "        val <- Sys.getenv(bakvar, NA_character_)\n"                                                                
#> [27] "        if (!is.na(val)) {\n"                                                                                      
#> [28] "            do.call(\"Sys.setenv\", structure(list(val), names = var))\n"                                          
#> [29] "        }\n"                                                                                                       
#> [30] "        else {\n"                                                                                                  
#> [31] "            Sys.unsetenv(var)\n"                                                                                   
#> [32] "        }\n"                                                                                                       
#> [33] "        Sys.unsetenv(bakvar)\n"                                                                                    
#> [34] "    })\n"                                                                                                          
#> [35] "    Sys.unsetenv(\"CALLR_CHILD_R_LIBS\")\n"                                                                        
#> [36] "    Sys.unsetenv(\"CALLR_CHILD_R_LIBS_SITE\")\n"                                                                   
#> [37] "    Sys.unsetenv(\"CALLR_CHILD_R_LIBS_USER\")\n"                                                                   
#> [38] "}\n"                                                                                                               
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
