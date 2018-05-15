
# callr

> Call R from R

[![Linux Build Status](https://travis-ci.org/r-lib/callr.svg?branch=master)](https://travis-ci.org/r-lib/callr)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/r-lib/callr?svg=true)](https://ci.appveyor.com/project/gaborcsardi/callr)
[![](http://www.r-pkg.org/badges/version/callr)](http://www.r-pkg.org/pkg/callr)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/callr)](http://www.r-pkg.org/pkg/callr)
[![Coverage Status](https://img.shields.io/codecov/c/github/r-lib/callr/master.svg)](https://codecov.io/github/r-lib/callr?branch=master)

It is sometimes useful to perform a computation in a separate R process,
without affecting the current R process at all. This packages does exactly
that.

---

  - [Installation](#installation)
  - [Usage](#usage)
    - [Introduction](#introduction)
    - [Passing arguments](#passing-arguments)
	- [Using packages](#using-packages)
	- [Error handling](#error-handling)
	- [Standard output and error](#standard-output-and-error)
	- [Showing progress](#showing-progress)
	- [Tips](#tips)
	- [`R CMD <command>`](#r-cmd-command)
  - [License](#license)

## Installation

Install the stable version from CRAN:

```r
install.packages("callr")
```

Install the development version from GitHub:

```r
source("https://install-github.me/r-lib/callr")
```

## Usage

### Introduction

Use `r` to run an R function in a new child process. The results are
passed back seamlessly:

```r
r(function() var(iris[, 1:4]))

#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length    0.6856935  -0.0424340    1.2743154   0.5162707
#> Sepal.Width    -0.0424340   0.1899794   -0.3296564  -0.1216394
#> Petal.Length    1.2743154  -0.3296564    3.1162779   1.2956094
#> Petal.Width     0.5162707  -0.1216394    1.2956094   0.5810063
```

### Passing arguments

You can pass arguments to the function by setting `args` to the list of
arguments. This is often necessary as these arguments are explicitly
passed to the child process, whereas the evaluated function cannot
refer to variables in the parent. For example, the following does
not work:

```r
mycars <- cars
r(function() summary(mycars))

#> Error in summary(mycars) (from internal.R#90) : object 'mycars' not found
```

But this does:

```r
r(function(x) summary(x), args = list(mycars))

#>     speed           dist
#> Min.   : 4.0   Min.   :  2.00
#> 1st Qu.:12.0   1st Qu.: 26.00
#> Median :15.0   Median : 36.00
#> Mean   :15.4   Mean   : 42.98
#> 3rd Qu.:19.0   3rd Qu.: 56.00
#> Max.   :25.0   Max.   :120.00
```

Note that the arguments will be serialized and saved to a file,
so if they are large R objects, it might take a long time for the
child process to start up.

### Using packages

You can use any R package in the child process, just make sure to
refer to it explicitly with the `::` operator. For example, the following
code creates an [igraph](https://github.com/igraph/rigraph) graph
in the child, and calculates some metrics of it.

```r
r(function() { g <- igraph::sample_gnp(1000, 4/1000); igraph::diameter(g) })

#> 12
```

### Error handling

`callr` provides three ways to handle errors that happen in the
child process. The default is to forward any errors to the parent:

```r
r(function() 1 + "A")
#> Error in 1 + "A" : non-numeric argument to binary operator
```

You can catch these errors on the parent, but the context is of course
lost. To get the context, you need to specify the `error = "stack"`
option. This copies the whole stack to the parent on an error.
The stack is part of the error object thrown on the parent, and you
can catch it with `tryCatch`, and examine it. Here is an example:

```r
tryCatch(
  r(function() { f <- function() g(); g <- function() 1 + "A"; f() },
    error = "stack"),
  error = function(e) print(e$stack)
)

#> $`(function () \n{\n    f <- function() g()\n    g <- function() 1 + "A"\n    f()`
#> <environment: 0x7fc1e4b61e08>
#>
#> $`#2: f()`
#> <environment: 0x7fc1e4b62150>
#>
#> $`#2: g()`
#> <environment: 0x7fc1e4b62188>
#>
#> attr(,"error.message")
#> [1] "non-numeric argument to binary operator"
#> attr(,"class")
#> [1] "dump.frames"
```

The third possible value for `error` is `"debugger"` which starts a
debugger (see `?debugger` in the call stack returned from the child:

```r
r(function() { f <- function() g(); g <- function() 1 + "A"; f() },
  error = "debugger")

#> Message:  non-numeric argument to binary operator
#> Available environments had calls:
#> 1: (function ()
#> {
#>     f <- function() g()
#>     g <- function() 1 + "A"
#>     f()
#> 2: #1: f()
#> 3: #1: g()
#>
#> Enter an environment number, or 0 to exit  Selection:
```

### Standard output and error

By default, the standard output and error of the child is lost,
but you can request `callr` to redirect them to files, and then
inspect the files in the parent:

```r
x <- r(function() { print("hello world!"); message("hello again!") },
  stdout = "/tmp/out", stderr = "/tmp/err"
)
readLines("/tmp/out")

#> [1] "[1] \"hello world!\""

readLines("/tmp/err")

#> [1] "hello again!"
```

### Showing progress

With the `stdout` option, the standard output is collected and can
be examined once the child process finished. The `show = TRUE` options
will also show the output of the child, as it is printed, on the console
of the parent.

### Tips

1) It is good practice to create an anonymous function for the `r()` call,
instead of passing a function from a package to `r()` directly. This is
because `callr` resets the environment of the function, which prevents
some functions from working. Here is an example:
```r
r(praise::praise)

#> Error: could not find function "is_template"
```

But with an anonymous function this works fine:
```r
r(function() praise::praise())

#> [1] "You are outstanding!"
```

2) If the function you call in the other session calls `quit()` with a
non-zero status, then callr interprets that as an R crash. Zero status is
a clean exit, but callr returns `NULL`, as no results were saved:

```r
r(function() quit(status = 0))
#> NULL

r(function() quit(status = 2))
#> Error: callr failed, could not start R, exited with non-zero status, has crashed or was killed
```

### `R CMD <command>`

The `rcmd()` function calls an `R CMD` command. For example, you can
call `R CMD INSTALL`, `R CMD check` or `R CMD config` this way:

```r
rcmd("config", "CC")

#>$stdout
#>[1] "clang\n"
#>
#>$stderr
#>[1] ""
#>
#>$status
#>[1] 0
```

This returns a list with three components: the standard output, the standard
error, and the exit (status) code of the `R CMD` command.

## License

MIT © Mango Solutions, RStudio
