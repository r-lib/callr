# Convert and check function arguments

This function is used for all variants of `r` and `rcmd`. An argument
name is only used to refer to one kind of object, to make this possible.

## Usage

``` r
convert_and_check_my_args(options)
```

## Arguments

- options:

  List of options.

## Details

The benefit of having a single `options` object is to avoid passing
around a lot of arguments all the time.

The benefit of making this object internal (i.e. that the `r`, etc.
functions have multiple arguments instead of a single `options` list),
is that documentation and usage is more user friendly (e.g. command-
completion works in the editor.
