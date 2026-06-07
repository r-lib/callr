# Package index

## Introduction

- [`callr`](https://callr.r-lib.org/reference/callr-package.md)
  [`callr-package`](https://callr.r-lib.org/reference/callr-package.md)
  : Call R from R

## Run R

- [`r()`](https://callr.r-lib.org/reference/r.md)
  [`r_safe()`](https://callr.r-lib.org/reference/r.md) : Evaluate an
  expression in another R session
- [`r_copycat()`](https://callr.r-lib.org/reference/r_copycat.md) : Run
  an R process that mimics the current R process
- [`r_vanilla()`](https://callr.r-lib.org/reference/r_vanilla.md) : Run
  an R child process, with no configuration

## Run R in the background

- [`r_bg()`](https://callr.r-lib.org/reference/r_bg.md) : Evaluate an
  expression in another R session, in the background
- [`r_process`](https://callr.r-lib.org/reference/r_process.md) :
  External R Process
- [`r_process_options()`](https://callr.r-lib.org/reference/r_process_options.md)
  : Create options for an r_process object

## Persistent background R session

- [`r_session`](https://callr.r-lib.org/reference/r_session.md) :
  External R Session
- [`r_session_options()`](https://callr.r-lib.org/reference/r_session_options.md)
  : Create options for an r_session object
- [`r_session_debug`](https://callr.r-lib.org/reference/r_session_debug.md)
  : Interactive debugging of persistent R sessions

## Run R CMD

- [`rcmd()`](https://callr.r-lib.org/reference/rcmd.md)
  [`rcmd_safe()`](https://callr.r-lib.org/reference/rcmd.md) :

  Run an `R CMD` command

- [`rcmd_copycat()`](https://callr.r-lib.org/reference/rcmd_copycat.md)
  :

  Call and `R CMD` command, while mimicking the current R session

- [`rcmd_safe_env()`](https://callr.r-lib.org/reference/rcmd_safe_env.md)
  :

  `rcmd_safe_env` returns a set of environment variables that are more
  appropriate for
  [`rcmd_safe()`](https://callr.r-lib.org/reference/rcmd.md). It is
  exported to allow manipulating these variables (e.g. add an extra
  one), before passing them to the
  [`rcmd()`](https://callr.r-lib.org/reference/rcmd.md) functions.

## Run R CMD in the background

- [`rcmd_bg()`](https://callr.r-lib.org/reference/rcmd_bg.md) :

  Run an `R CMD` command in the background

- [`rcmd_process`](https://callr.r-lib.org/reference/rcmd_process.md) :

  External `R CMD` Process

- [`rcmd_process_options()`](https://callr.r-lib.org/reference/rcmd_process_options.md)
  : Create options for an rcmd_process object

## Run Rscript

- [`rscript()`](https://callr.r-lib.org/reference/rscript.md) : Run an R
  script

## Run Rscript in the background

- [`rscript_process`](https://callr.r-lib.org/reference/rscript_process.md)
  :

  External `Rscript` process

- [`rscript_process_options()`](https://callr.r-lib.org/reference/rscript_process_options.md)
  : Create options for an rscript_process object

## Miscellaneous utilities

- [`default_repos()`](https://callr.r-lib.org/reference/default_repos.md)
  :

  Default value for the `repos` option in callr subprocesses

- [`supported_archs()`](https://callr.r-lib.org/reference/supported_archs.md)
  : Find supported sub-architectures for the current R installation

- [`add_hook()`](https://callr.r-lib.org/reference/add_hook.md) : Add a
  user hook to be executed before launching an R subprocess

## Re-exported functions

- [`reexports`](https://callr.r-lib.org/reference/reexports.md)
  [`run`](https://callr.r-lib.org/reference/reexports.md)
  [`process`](https://callr.r-lib.org/reference/reexports.md)
  [`poll`](https://callr.r-lib.org/reference/reexports.md) : Objects
  exported from other packages
