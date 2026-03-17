# Find supported sub-architectures for the current R installation

This function uses a heuristic, which might fail, so its result should
be taken as a best guess.

## Usage

``` r
supported_archs()
```

## Value

Character vector of supported architectures. If the current R build is
not a multi-architecture build, then an empty string scalar is returned.

## Examples

``` r
supported_archs()
#> [1] ""
```
