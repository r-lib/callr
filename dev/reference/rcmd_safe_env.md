# `rcmd_safe_env` returns a set of environment variables that are more appropriate for [`rcmd_safe()`](https://callr.r-lib.org/dev/reference/rcmd.md). It is exported to allow manipulating these variables (e.g. add an extra one), before passing them to the [`rcmd()`](https://callr.r-lib.org/dev/reference/rcmd.md) functions.

It currently has the following variables:

- `CYGWIN="nodosfilewarning"`: On Windows, do not warn about MS-DOS
  style file names.

- `R_TESTS=""` This variable is set by `R CMD check`, and makes the
  child R process load a startup file at startup, from the current
  working directory, that is assumed to be the `/test` directory of the
  package being checked. If the current working directory is changed to
  something else (as it typically is by `testthat`, then R cannot start.
  Setting it to the empty string ensures that `callr` can be used from
  unit tests.

- `R_BROWSER="false"`: typically we don't want to start up a browser
  from the child R process.

- `R_PDFVIEWER="false"`: similarly for the PDF viewer.

## Usage

``` r
rcmd_safe_env()
```

## Value

A named character vector of environment variables.

## Details

Note that `callr` also sets the `R_ENVIRON`, `R_ENVIRON_USER`,
`R_PROFILE` and `R_PROFILE_USER` environment variables appropriately,
unless these are set by the user in the `env` argument of the `r`, etc.
calls.
