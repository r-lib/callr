# Add a user hook to be executed before launching an R subprocess

This function allows users of `callr` to specify functions that get
invoked whenever an R session is launched. The function can modify the
environment variables and command line arguments.

## Usage

``` r
add_hook(...)
```

## Arguments

- ...:

  Named argument specifying a hook function to add, or `NULL` to delete
  the named hook.

## Value

`add_hook` is called for its side-effects.

## Details

The prototype of the hook function is `function (options)`, and it is
expected to return the modified `options`.
