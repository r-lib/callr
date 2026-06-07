# Persistent External R Sessions

## Introduction

[`callr::r_session`](https://callr.r-lib.org/reference/r_session.md) is
a class for a persistent R session that runs in the background and you
can send commands to it. It extends the
[`processx::process`](http://processx.r-lib.org/reference/process.md)
class, so all methods of that class are still available for use.

## Starting and terminating an R session

Use `r_session$new()` to start an R session. By default
`r_session$new()` blocks, it does not return until the R session is up
and running and ready to run R commands. If an error happens during
process startup, including an R error, then `r_session$new()` throws an
error. A blocking `r_session$new()` waits at most `wait_timeout`
milliseconds for R to start up. `wait_timeout` is by default 3000
milliseconds, which should be plenty. Typically R starts up in about
100-300 milliseconds.

``` r

library(callr)
system.time(rs <- r_session$new())
```


    #>    user  system elapsed                                                         
    #>   0.019   0.004   0.199                                                         

``` r

rs
```


    #> R SESSION, alive, idle, pid 9334.                                               

``` r

rs$get_state()
```


    #> [1] "idle"                                                                      

To terminate an R session, call its `$close()` method:

``` r

rs$close()
rs
```


    #> R SESSION, finished, pid 9334.                                                  

Just like
[`processx::process`](http://processx.r-lib.org/reference/process.md)
objects, `r_session` objects have a finalizer, and they will be
terminated when the R object that represents them is garbage collected.

### Non-blocking startup

If you don’t want to wait for the session to start up, then use
`wait = FALSE` in `r_session$new()`. If you do that, `r_session$new()`
will still error if the OS cannot start up the R process, but R errors
will be reported asynchronously.

``` r

system.time(rs2 <- r_session$new(wait = FALSE))
```


    #>    user  system elapsed                                                         
    #>   0.002   0.003   0.007                                                         

``` r

rs2
```


    #> R SESSION, alive, starting, pid 9345.                                           

``` r

rs2$get_state()
```


    #> [1] "starting"                                                                  

You can use
[`processx::poll()`](http://processx.r-lib.org/reference/poll.md) to
wait for the R session being ready, with a timeout. The timeout can also
be 0ms for a quick check without waiting. This lets you do extra work in
the main process while the R process is starting up. It also lets you
start up multiple processes concurrently, see the next section.

``` r

processx::poll(list(rs2), 3000)
```


    #> [[1]]                                                                           
    #>   output    error  process                                                      
    #> "silent" "silent"  "ready"                                                      
    #>                                                                                 

The important part of the output is the `process` connection. This will
be `"ready"` if the R process is up and running, or if an error
happened. It will be `"timeout"` if it is not yet ready.

`output` and `error` will be `"ready"` if the R process emitted
something to its standard output and standard error, respectively.
Usually these will be `"silent"` because we suppress R output during
startup with command line options. This can be changed via the `options`
argument and
[`r_session_options()`](https://callr.r-lib.org/reference/r_session_options.md).

Once [`processx::poll()`](http://processx.r-lib.org/reference/poll.md)
reports a `"ready"` `process` connection, you can call the
`r_session$read()` method to see if the startup was successful.

If `r_session$$read()` reports “201 STARTED”, it is ready to run R code:

``` r

rs2$read()
```


    #> $code                                                                           
    #> [1] 201                                                                         
    #>                                                                                 
    #> $message                                                                        
    #> [1] "ready to go"                                                               
    #>                                                                                 
    #> attr(,"class")                                                                  
    #> [1] "callr_session_result"                                                      

## Options

You can use the `options` argument of `r_session$new()` to change the
default startup options. `options` must be a named list and it is best
to create its value with
[`r_session_options()`](https://callr.r-lib.org/reference/r_session_options.md).
Pass the options you want to change as named arguments to
[`r_session_options()`](https://callr.r-lib.org/reference/r_session_options.md).
See
[`?r_session_options`](https://callr.r-lib.org/reference/r_session_options.md)
for the details.

Here is an example that uses the `load_hook` option to run extra code
right after R has started up:

``` r

opts <- r_session_options(
  load_hook = quote({ message("I am running!"); Sys.sleep(1) })
)
rs3 <- r_session$new(wait = FALSE, options = opts)
processx::poll(list(rs3), 3000)
```


    #> [[1]]                                                                           
    #>   output    error  process                                                      
    #> "silent"  "ready" "silent"                                                      
    #>                                                                                 

``` r

rs3$read_error()
```


    #> [1] "I am running!\n"                                                           

``` r

rs3$poll_process(3000)
```


    #> [1] "ready"                                                                     

``` r

rs3$read()
```


    #> $code                                                                           
    #> [1] 201                                                                         
    #>                                                                                 
    #> $message                                                                        
    #> [1] "ready to go"                                                               
    #>                                                                                 
    #> attr(,"class")                                                                  
    #> [1] "callr_session_result"                                                      

Use the `$poll_process()` method to poll only the process for being
ready, without polling the standard output and error. Note, however,
that if the process generates enough output on stdout or stderr that
fills the pipe buffer between the processes, then it will stop running,
until the main process reads the pipe.

## Running multiple R sessions

If you need to start several R sessions quickly, then it is best to use
`wait = FALSE` and then
[`processx::poll()`](http://processx.r-lib.org/reference/poll.md) for
all processes until they are all ready.

``` r

num_procs <- 4
procs <- tibble::tibble(
  session = replicate(num_procs, r_session$new(wait = FALSE), simplify = FALSE),
  started_at = Sys.time(),
  start_result = list(NULL)
)
limit <- Sys.time() + as.difftime(5, units = "secs")
while ((now <- Sys.time()) < limit &&
       any(vapply(procs$session, function(p) p$get_state(), "") == "starting")) {
  timeout <- as.double(limit - now, units = "secs")
  pr <- processx::poll(procs$session, as.integer(timeout * 1000))
  lapply(seq_along(pr), function(i) {
    if (pr[[i]][["process"]] == "ready") {
      procs$start_result[[i]] <<- procs$session[[i]]$read()
    }
  })
}
Sys.time() - procs$started_at
```


    #> Time differences in secs                                                        
    #> [1] 0.2590313 0.2590313 0.2590313 0.2590313                                     

``` r

procs$start_result
```


    #> [[1]]                                                                           
    #> $code                                                                           
    #> [1] 201                                                                         
    #>                                                                                 
    #> $message                                                                        
    #> [1] "ready to go"                                                               
    #>                                                                                 
    #> attr(,"class")                                                                  
    #> [1] "callr_session_result"                                                      
    #>                                                                                 
    #> [[2]]                                                                           
    #> $code                                                                           
    #> [1] 201                                                                         
    #>                                                                                 
    #> $message                                                                        
    #> [1] "ready to go"                                                               
    #>                                                                                 
    #> attr(,"class")                                                                  
    #> [1] "callr_session_result"                                                      
    #>                                                                                 
    #> [[3]]                                                                           
    #> $code                                                                           
    #> [1] 201                                                                         
    #>                                                                                 
    #> $message                                                                        
    #> [1] "ready to go"                                                               
    #>                                                                                 
    #> attr(,"class")                                                                  
    #> [1] "callr_session_result"                                                      
    #>                                                                                 
    #> [[4]]                                                                           
    #> $code                                                                           
    #> [1] 201                                                                         
    #>                                                                                 
    #> $message                                                                        
    #> [1] "ready to go"                                                               
    #>                                                                                 
    #> attr(,"class")                                                                  
    #> [1] "callr_session_result"                                                      
    #>                                                                                 

## Running code

`r_session` objects have three methods to run R code:

- `$run()` is synchronous and omits standard output and error.
- `$run_with_output()` is synchronous and collects standard output and
  error.
- `$call()` is asynchronous and collects standard output and error.

Let’s use the 4 R sessions created above to demonstrate them.

`$run()` is the simplest:

``` r

procs$session[[1]]$run(function() glue::glue("I am process {Sys.getpid()}."))
```


    #> I am process 9365.                                                              

`$run_with_output()` has the output as well:

``` r

procs$session[[1]]$run_with_output(function() {
  message("I am process ", Sys.getpid(), ".")
  head(mtcars)
})
```


    #> $code                                                                           
    #> [1] 200                                                                         
    #>                                                                                 
    #> $message                                                                        
    #> [1] "done callr-rs-result-246d727e5ba0"                                         
    #>                                                                                 
    #> $result                                                                         
    #>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb            
    #> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4            
    #> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4            
    #> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1            
    #> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1            
    #> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2            
    #> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1            
    #>                                                                                 
    #> $stdout                                                                         
    #> [1] ""                                                                          
    #>                                                                                 
    #> $stderr                                                                         
    #> [1] "I am process 9365.\n"                                                      
    #>                                                                                 
    #> $error                                                                          
    #> NULL                                                                            
    #>                                                                                 
    #> attr(,"class")                                                                  
    #> [1] "callr_session_result"                                                      

`$call()` starts running the function, but does not wait for the result:

``` r

invisible(lapply(procs$session, function(p) {
    p$call(function() {
      Sys.sleep(runif(1) * 2)
      glue::glue("I am process {Sys.getpid()}.")
    })
}))
procs$session
```


    #> [[1]]                                                                           
    #> R SESSION, alive, busy, pid 9365.                                               
    #>                                                                                 
    #> [[2]]                                                                           
    #> R SESSION, alive, busy, pid 9367.                                               
    #>                                                                                 
    #> [[3]]                                                                           
    #> R SESSION, alive, busy, pid 9372.                                               
    #>                                                                                 
    #> [[4]]                                                                           
    #> R SESSION, alive, busy, pid 9377.                                               
    #>                                                                                 

Use [`processx::poll()`](http://processx.r-lib.org/reference/poll.md) to
wait for one or more sessions to finish their job:

``` r

pr <- processx::poll(procs$session, 5000)
pr
```


    #> [[1]]                                                                           
    #>   output    error  process                                                      
    #> "silent" "silent" "silent"                                                      
    #>                                                                                 
    #> [[2]]                                                                           
    #>   output    error  process                                                      
    #> "silent" "silent" "silent"                                                      
    #>                                                                                 
    #> [[3]]                                                                           
    #>   output    error  process                                                      
    #> "silent" "silent" "silent"                                                      
    #>                                                                                 
    #> [[4]]                                                                           
    #>   output    error  process                                                      
    #> "silent" "silent"  "ready"                                                      
    #>                                                                                 

Then you can use the `$read()` method to read out the result (or error,
if a failure happened):

``` r

for (i in seq_along(pr)) {
  if (pr[[i]][["process"]] == "ready") {
    cat("Process ", i, " is ready:\n")
    print(procs$session[[i]]$read())
  }
}
```


    #> Process  4  is ready:                                                           
    #> $code                                                                           
    #> [1] 200                                                                         
    #>                                                                                 
    #> $message                                                                        
    #> [1] "done callr-rs-result-246d20c27bd8"                                         
    #>                                                                                 
    #> $result                                                                         
    #> I am process 9377.                                                              
    #>                                                                                 
    #> $stdout                                                                         
    #> [1] ""                                                                          
    #>                                                                                 
    #> $stderr                                                                         
    #> [1] ""                                                                          
    #>                                                                                 
    #> $error                                                                          
    #> NULL                                                                            
    #>                                                                                 
    #> attr(,"class")                                                                  
    #> [1] "callr_session_result"                                                      

To wait for all processes to be ready, you can use a loop that is
similar to the one we used above to start them. You might find this
helper function useful as a starting point:

``` r

wait_for_sessions <- function(sess, timeout = 5000) {
  result <- vector("list", length(sess))
  is_busy <- function() {
    vapply(sess, function(s) s$get_state() == "busy", logical(1))
  }
  limit <- Sys.time() + as.difftime(timeout / 1000, units = "secs")
  while ((now < Sys.time()) < limit && any(busy <- is_busy())) {
    towait <- as.integer(as.double(limit - now, units = "secs") * 1000)
    pr <- processx::poll(sess[busy], towait)
    for (i in seq_along(pr)) {
      if (pr[[i]][["process"]] == "ready") {
        result[busy][[i]] <- sess[busy][[i]]$read()
      }
    }
  }
  result
}
wait_for_sessions(procs$session)
```


    #> [[1]]                                                                           
    #> $code                                                                           
    #> [1] 200                                                                         
    #>                                                                                 
    #> $message                                                                        
    #> [1] "done callr-rs-result-246d6d0b621a"                                         
    #>                                                                                 
    #> $result                                                                         
    #> I am process 9365.                                                              
    #>                                                                                 
    #> $stdout                                                                         
    #> [1] ""                                                                          
    #>                                                                                 
    #> $stderr                                                                         
    #> [1] ""                                                                          
    #>                                                                                 
    #> $error                                                                          
    #> NULL                                                                            
    #>                                                                                 
    #> attr(,"class")                                                                  
    #> [1] "callr_session_result"                                                      
    #>                                                                                 
    #> [[2]]                                                                           
    #> $code                                                                           
    #> [1] 200                                                                         
    #>                                                                                 
    #> $message                                                                        
    #> [1] "done callr-rs-result-246d357a88c"                                          
    #>                                                                                 
    #> $result                                                                         
    #> I am process 9367.                                                              
    #>                                                                                 
    #> $stdout                                                                         
    #> [1] ""                                                                          
    #>                                                                                 
    #> $stderr                                                                         
    #> [1] ""                                                                          
    #>                                                                                 
    #> $error                                                                          
    #> NULL                                                                            
    #>                                                                                 
    #> attr(,"class")                                                                  
    #> [1] "callr_session_result"                                                      
    #>                                                                                 
    #> [[3]]                                                                           
    #> $code                                                                           
    #> [1] 200                                                                         
    #>                                                                                 
    #> $message                                                                        
    #> [1] "done callr-rs-result-246d412b794a"                                         
    #>                                                                                 
    #> $result                                                                         
    #> I am process 9372.                                                              
    #>                                                                                 
    #> $stdout                                                                         
    #> [1] ""                                                                          
    #>                                                                                 
    #> $stderr                                                                         
    #> [1] ""                                                                          
    #>                                                                                 
    #> $error                                                                          
    #> NULL                                                                            
    #>                                                                                 
    #> attr(,"class")                                                                  
    #> [1] "callr_session_result"                                                      
    #>                                                                                 
    #> [[4]]                                                                           
    #> NULL                                                                            
    #>                                                                                 

Errors from a `$run()` are turned into errors in the main process:

``` r

rs <- r_session$new()
rs$run(function() library("not-a-package"))
```


    #> Error:                                                                          
    #> ! in callr subprocess.                                                          
    #> Caused by error in `library("not-a-package")`:                                  
    #> ! there is no package called ‘not-a-package’                                    
    #> Type .Last.error to see the more details.                                       

callr also adds two stack traces to the output, one for the main process
and one for the subprocess:

``` r

.Last.error
```


    #> <callr_error/rlib_error_3_0/rlib_error/error>                                   
    #> Error:                                                                          
    #> ! in callr subprocess.                                                          
    #> Caused by error in `library("not-a-package")`:                                  
    #> ! there is no package called ‘not-a-package’                                    
    #> ---                                                                             
    #> Backtrace:                                                                      
    #> 1. rs$run(function() library("not-a-package"))                                  
    #> 2. callr:::rs_run(self, private, func, args, package)                           
    #> 3. callr:::throw(res$error)                                                     
    #> ---                                                                             
    #> Subprocess backtrace:                                                           
    #> 1. base::library("not-a-package")                                               
    #> 2. base::stop(packageNotFoundError(package, lib.loc, sys.call()))               
    #> 3. global (function (e) …                                                       

Errors from a `$call()` are returned in the `error` entry of the result:

``` r

rs$call(function() library("still-not"))
rs$poll_process(2000)
rs$read()
```


    #> [1] "ready"                                                                     
    #> $code                                                                           
    #> [1] 200                                                                         
    #>                                                                                 
    #> $message                                                                        
    #> [1] "done callr-rs-result-246d27e09eef"                                         
    #>                                                                                 
    #> $result                                                                         
    #> NULL                                                                            
    #>                                                                                 
    #> $stdout                                                                         
    #> [1] ""                                                                          
    #>                                                                                 
    #> $stderr                                                                         
    #> [1] ""                                                                          
    #>                                                                                 
    #> $error                                                                          
    #> <callr_error/rlib_error_3_0/rlib_error/error>                                   
    #> Error:                                                                          
    #> ! in callr subprocess.                                                          
    #> Caused by error in `library("still-not")`:                                      
    #> ! there is no package called ‘still-not’                                        
    #> ---                                                                             
    #> Subprocess backtrace:                                                           
    #> 1. base::library("still-not")                                                   
    #> 2. base::stop(packageNotFoundError(package, lib.loc, sys.call()))               
    #> 3. global (function (e) …                                                       
    #>                                                                                 
    #> attr(,"class")                                                                  
    #> [1] "callr_session_result"                                                      

## Debugging

Debugging subprocesses is hard. `r_session` objects have a couple of
methods to help you, but it is still hard.

### Stack traces

As you have seen above, callr returns stack traces for errors, both for
the main process and the subprocess. If your packages are installed with
source references, then these include links to the source files as well.

### `.Last.error`

For errors that are re-thrown in the main process, callr sets the
`.Last.error` variable to the last error object. You can inspect that
after the error.

`.Last.error$parent` contains the error object from the subprocess. The
error object often has additional information about the error, e.g.
[`processx::run()`](http://processx.r-lib.org/reference/run.md) includes
the standard output + error if the system process exits with a
non-successful status:

``` r

rs <- r_session$new()
rs$run(function() processx::run("ls", "/not-this"))
```


    #> Error:                                                                          
    #> ! in callr subprocess.                                                          
    #> Caused by error in `processx::run("ls", "/not-this")`:                          
    #> ! System command 'ls' failed                                                    
    #> Type .Last.error to see the more details.                                       

``` r

.Last.error
```


    #> <callr_error/rlib_error_3_0/rlib_error/error>                                   
    #> Error:                                                                          
    #> ! in callr subprocess.                                                          
    #> Caused by error in `processx::run("ls", "/not-this")`:                          
    #> ! System command 'ls' failed                                                    
    #> ---                                                                             
    #> Backtrace:                                                                      
    #> 1. rs$run(function() processx::run("ls", "/not-this"))                          
    #> 2. callr:::rs_run(self, private, func, args, package)                           
    #> 3. callr:::throw(res$error)                                                     
    #> ---                                                                             
    #> Subprocess backtrace:                                                           
    #> 1. processx::run("ls", "/not-this")                                             
    #> 2. base::throw(new_process_error(res, call = sys.call(), echo = echo, …         
    #> 3. | base::signalCondition(cond)                                                
    #> 4. global (function (e) …                                                       

``` r

.Last.error$parent
```


    #> <system_command_status_error/rlib_error_3_0/rlib_error/error>                   
    #> Error in `processx::run("ls", "/not-this")`:                                    
    #> ! System command 'ls' failed                                                    
    #> ---                                                                             
    #> Exit status: 2                                                                  
    #> Stderr:                                                                         
    #> ls: cannot access '/not-this': No such file or directory                        

### Inspecting the stack traces

Another way to inspect the stack trace in the subprocess is to set the
`callr.traceback` option to `TRUE` and call the `$traceback()` method
after the error.

This option is off by default, because the stack trace sometimes
contains large objects, that take a lot of time to copy between
processes.

``` r

options(callr.traceback = TRUE)
rs <- r_session$new()
fun <- function() {
  options(warn = 2)         # convert warnings to errors
  f1 <- function() f2()
  f2 <- function() f3()
  f3 <- function() {
    vec <- 1:2
    if (vec) "success"
  }
  f1()
}
rs$run(fun)
```


    #> Error:                                                                          
    #> ! in callr subprocess.                                                          
    #> Caused by error in `if (vec) "success"`:                                        
    #> ! the condition has length > 1                                                  
    #> Type .Last.error to see the more details.                                       

``` r

rs$traceback()
```


    #> 3: f3() at #4                                                                   
    #> 2: f2() at #3                                                                   
    #> 1: f1()                                                                         

### Inspecting frames of a stack trace

If the `callr.traceback` option is `TRUE`, then callr saves the full
trace, including the frames. You can then inspect these frames with the
`$debug()` method. We can use it here to debug the previous error:

![Screen recording showing \`rs\$debug()\`. First there is a help
message with the various commands you can use: \`.where\`, \`.inspect
\<n\>\`, \`.help\`, \`.q\`, or an R expression to run. Then there is a
list of frames. Next there is an \`inspect 3\` call to get into \`frame
3\`, and next an \`ls()\` expression evaluated in that frame. It lists
\`vec\` as the only object in that frame, and then \`vec\` will show
what \`vec\` is, the \`1:2\` integer vector. At the end \`.q\` exits the
debugger.](r-session_files/figure-html/debug.svg)

### Interactive debugging

You can use the `$attach` method to start a REPL (read-eval-print loop)
that runs in the subprocess. It is best to do this when the subprocess
is idle, otherwise it is probably not responsive.

Press CTRL+C or ESC, or type `.q` and press ENTER to quit the REPL.

Here is an example:

``` r

rs <- r_session$new()
rs$run(function() { .GlobalEnv$data <- mtcars; NULL })
```


    #> NULL                                                                            

![Screen recording for an \`rs\$attach()\` example. It shows an \`RS
\<pid\>\` prompt. Then we type in \`ls()\`, which is evaluated in the
subprocess, and shows \`data\`. \`head(data)\` shows the first rows of
\`data\`, which is the \`mtcars\` data set. Finally \`.q\` quits the
debugger.](r-session_files/figure-html/attach-1.svg)

This is an experimental feature and it does not always print the output
properly, e.g. sometimes you need to press ENTER twice, but nevertheless
it can be useful at times.

## Communication protocol

The `$read()` method can return messages with the following `code`s:

- `200`: the function is done. Note that the result might still be an
  error, you need to check that the `error` entry is not `NULL`.
- `201`: the R process is ready to use. This is the first message you
  get after a successful non-blocking startup.
- `202`: attach is done. This is used internally by the `$attach()`
  method, see the section about debugging below.
- `301`: message from the subprocess. E.g. the cli package can generate
  such messages, see [the cli
  documentation](https://cli.r-lib.org/articles/semantic-cli.html#sub-processes).
- `500`: the R session exited cleanly. This means that the evaluated
  expression quit R.
- `501`: the R session crashed or was killed.
- `502`: the R session closed its end of the connection that callr uses
  for communication. This might also happen because it was killed or
  crashed.
