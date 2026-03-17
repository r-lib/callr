# Create an error object

There are two kinds of errors, both have class `callr_error`:

1.  the first one is thrown after a timeout: `callr_timeout_error`.

2.  the second one is thrown after an R error (in the other session):
    `callr_status_error`.

## Usage

``` r
new_callr_crash_error(out, msg = NULL)
```

## Arguments

- out:

  The object returned by
  [`run()`](http://processx.r-lib.org/reference/run.md).

- msg:

  An extra message to add to the error message.
