# Read the result object from the output file, or the error

Even if an error happens, the output file might still exist, because
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) creates the file
before evaluating its object argument. So we need to check for the error
file to decide if an error happened.

## Usage

``` r
get_result(output, options)
```

## Arguments

- output:

  List of the output object from
  [`run()`](http://processx.r-lib.org/reference/run.md) and the name of
  the result file to read. For the error file, `.error` is appended to
  this.

- options:

  The context, including all parameters.

## Value

If no error happened, the result is returned. Otherwise we handle the
error.
