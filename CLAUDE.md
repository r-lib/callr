# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Commands

Load the package:

``` r

uncovr::reload()
```

Run the test suite:

``` r

uncovr::test()
```

Run a single test file:

``` r

uncovr::test(filter = "r-session")
```

Regenerate documentation (man/ files):

``` r

uncovr::document()
```

Run full package check:

``` r

withr::with_envvar(c(NOT_CRAN = "true", DISPLAY = NA), rcmdcheck::rcmdcheck())
```

## Architecture

callr lets R code call R functions in separate R processes. There are
three execution models and three command types:

**Execution models:** - **Synchronous one-off**
([`r()`](https://callr.r-lib.org/reference/r.md),
[`rcmd()`](https://callr.r-lib.org/reference/rcmd.md),
[`rscript()`](https://callr.r-lib.org/reference/rscript.md)): spawns a
process, waits for completion, returns result - **Asynchronous one-off**
([`r_bg()`](https://callr.r-lib.org/reference/r_bg.md),
[`rcmd_bg()`](https://callr.r-lib.org/reference/rcmd_bg.md)): spawns a
process, returns an R6 object immediately; caller polls/waits -
**Persistent session** (`r_session$new()`): a reusable R process that
accepts multiple sequential function calls over its lifetime

**Command types:** - R function evaluation —
[`r()`](https://callr.r-lib.org/reference/r.md),
[`r_bg()`](https://callr.r-lib.org/reference/r_bg.md), `r_session`:
serialize a closure to RDS, run it in a subprocess, deserialize the
result - R CMD commands —
[`rcmd()`](https://callr.r-lib.org/reference/rcmd.md),
[`rcmd_bg()`](https://callr.r-lib.org/reference/rcmd_bg.md): run
`R CMD <subcommand>` with arguments - R scripts —
[`rscript()`](https://callr.r-lib.org/reference/rscript.md): run a `.R`
file via `Rscript`

**Key source files:**

| File | Role |
|----|----|
| [R/eval.R](https://callr.r-lib.org/R/eval.R) | [`r()`](https://callr.r-lib.org/reference/r.md) — main synchronous entry point |
| [R/eval-bg.R](https://callr.r-lib.org/R/eval-bg.R) | [`r_bg()`](https://callr.r-lib.org/reference/r_bg.md) — async entry point |
| [R/r-process.R](https://callr.r-lib.org/R/r-process.R) | `r_process` R6 class (extends [`processx::process`](http://processx.r-lib.org/reference/process.md)) used by [`r_bg()`](https://callr.r-lib.org/reference/r_bg.md) |
| [R/r-session.R](https://callr.r-lib.org/R/r-session.R) | `r_session` R6 class — persistent subprocess with a message loop |
| [R/rcmd.R](https://callr.r-lib.org/R/rcmd.R) / [R/rcmd-bg.R](https://callr.r-lib.org/R/rcmd-bg.R) | [`rcmd()`](https://callr.r-lib.org/reference/rcmd.md) / [`rcmd_bg()`](https://callr.r-lib.org/reference/rcmd_bg.md) entry points |
| [R/script.R](https://callr.r-lib.org/R/script.R) | Generates the R script that runs inside the subprocess |
| [R/setup.R](https://callr.r-lib.org/R/setup.R) | Serializes the function + args to a temp RDS file for transport |
| [R/result.R](https://callr.r-lib.org/R/result.R) | Deserializes the result RDS written by the subprocess |
| [R/error.R](https://callr.r-lib.org/R/error.R) | Error classes (`callr_error`, `callr_timeout_error`, `callr_status_error`) and propagation |
| [R/options.R](https://callr.r-lib.org/R/options.R) | Builds the option lists consumed by `r_process`, `rcmd_process`, `rscript_process` |
| [R/hook.R](https://callr.r-lib.org/R/hook.R) | Hook system run inside the subprocess before user code (library paths, repos, etc.) |
| [R/package.R](https://callr.r-lib.org/R/package.R) | Package init, loads the processx C client library (`client.so`/`.dll`) |

**Subprocess communication:** The parent process writes a function +
arguments to a temp `.rds` file; the generated script (from `script.R`)
reads and evaluates it, then writes the return value (or error) to
another `.rds` file. `r_session` uses a persistent connection via the
processx C client for lower-overhead repeated calls.

**Safety defaults:** By default,
[`r()`](https://callr.r-lib.org/reference/r.md) and friends run with
`--no-save --no-restore`, no user profile, and a forced CRAN mirror —
see [`r_safe()`](https://callr.r-lib.org/reference/r.md) in
[R/presets.R](https://callr.r-lib.org/R/presets.R).

## Testing

Tests live in `tests/testthat/`. Snapshot files are in
`tests/testthat/_snaps/`. A small test package (`csomag/`) lives in
`tests/testthat/fixtures/` and is used for library-path and installation
tests.

Many tests are skipped on CRAN (`skip_on_cran()`). The `NOT_CRAN`
environment variable controls this during local development.
