# Changelog

## callr (development version)

- [`r()`](https://callr.r-lib.org/dev/reference/r.md),
  [`rcmd()`](https://callr.r-lib.org/dev/reference/rcmd.md),
  [`rscript()`](https://callr.r-lib.org/dev/reference/rscript.md) and
  `rscript_process` now accept `pty = TRUE` to run the child R process
  in a pseudo-terminal.

- callr now supports
  [`carrier::crate()`](https://rdrr.io/pkg/carrier/man/crate.html).
  `package = NULL`, the new default, handles functions created with
  [`carrier::crate()`](https://rdrr.io/pkg/carrier/man/crate.html)
  automatically ([\#249](https://github.com/r-lib/callr/issues/249)).

- The `CALLR_TMPDIR` environment variable can now be set to override the
  directory callr uses for its auxiliary temporary files. When unset,
  callr keeps using [`tempdir()`](https://rdrr.io/r/base/tempfile.html)
  ([\#172](https://github.com/r-lib/callr/issues/172)).

- [`r_bg()`](https://callr.r-lib.org/dev/reference/r_bg.md) (and
  [`r()`](https://callr.r-lib.org/dev/reference/r.md)) subprocesses now
  exit with a non-zero status when the evaluated expression throws an
  error or is interrupted, instead of always exiting with status 0
  ([\#291](https://github.com/r-lib/callr/issues/291)).

- [`r()`](https://callr.r-lib.org/dev/reference/r.md),
  [`r_bg()`](https://callr.r-lib.org/dev/reference/r_bg.md),
  `r_session$new()` and friends now accept `libpath = NULL`, which
  leaves the subprocess library path alone, so the subprocess uses the
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) of a fresh R
  session (e.g. as set up by a project `.Rprofile`) instead of
  inheriting the parent’s library path
  ([\#255](https://github.com/r-lib/callr/issues/255)).

- callr is now instrumented with
  [OpenTelemetry](https://opentelemetry.io/). When an OpenTelemetry SDK
  (such as otelsdk) is loaded and configured, callr emits spans for
  [`r()`](https://callr.r-lib.org/dev/reference/r.md),
  [`rcmd()`](https://callr.r-lib.org/dev/reference/rcmd.md),
  [`rscript()`](https://callr.r-lib.org/dev/reference/rscript.md),
  `r_process` and `r_session` (including per-call `$call`, `$read` and
  `$close` spans), and propagates the W3C `traceparent` header into the
  subprocess so spans created inside the child are parented to the
  parent’s span. See the ‘OpenTelemetry’ article for details
  ([\#302](https://github.com/r-lib/callr/issues/302)).

- [`r()`](https://callr.r-lib.org/dev/reference/r.md),
  [`rcmd()`](https://callr.r-lib.org/dev/reference/rcmd.md),
  [`rscript()`](https://callr.r-lib.org/dev/reference/rscript.md) and
  friends no longer hang on Windows when called with `stdout = "|"` (or
  any value of `stdout`/`stderr` other than a file path or `NULL`)
  ([\#313](https://github.com/r-lib/callr/issues/313)).

## callr 3.7.6

CRAN release: 2024-03-25

- If the `CALLR_NO_TEMP_DLLS=true` env var is set then callr does not
  copy the dll the client DLL files from, in the subprocess. By default
  callr copies the DLL file that drives the callr subprocess into a
  temporary directory and loads it from there
  ([\#273](https://github.com/r-lib/callr/issues/273)).

## callr 3.7.5

CRAN release: 2024-02-19

- No changes.

## callr 3.7.4

CRAN release: 2024-02-19

- The `r_session$get_running_time()` method now returns the correct
  values, as documented
  ([\#241](https://github.com/r-lib/callr/issues/241),
  [@djnavarro](https://github.com/djnavarro)).

- callr now uses fully qualified function calls in the subprocess to
  avoid interference with functions defined in the global environment.
  I.e. [`base::stderr()`](https://rdrr.io/r/base/showConnections.html)
  instead of [`stderr()`](https://rdrr.io/r/base/showConnections.html).
  Closes [\#246](https://github.com/r-lib/callr/issues/246).

## callr 3.7.3

CRAN release: 2022-11-02

- Errors from callr now include the standard output (in `$stdout`) and
  standard error (in `stderr`) again. The standard output and error are
  also printed on error in non-interactive sessions, and a note is
  printed about them in interactive sessions.

## callr 3.7.2

CRAN release: 2022-08-22

- New function
  [`add_hook()`](https://callr.r-lib.org/dev/reference/add_hook.md) to
  hook into the callr process startup and options. This is for experts
  and it is also currently experimental
  ([\#203](https://github.com/r-lib/callr/issues/203),
  [@klmr](https://github.com/klmr)).

## callr 3.7.1

CRAN release: 2022-07-13

- When copying existing startup files, an additional newline is appended
  to protect against a missing newline at the end of the file. This
  would cause R ignore that line
  ([\#205](https://github.com/r-lib/callr/issues/205)).

- Serialization of objects passed between sessions now uses
  `compress=FALSE` by default. The default can be changed by setting the
  `callr.compress_transport` option
  ([\#223](https://github.com/r-lib/callr/issues/223),
  [@dfalbel](https://github.com/dfalbel)).

- We have revamped callr’s error objects, with lots of improvements to
  the output.

## callr 3.7.0

CRAN release: 2021-04-20

- Reporting errors is much faster now
  ([\#185](https://github.com/r-lib/callr/issues/185)).

- The `user_profile` option of
  [`r_vanilla()`](https://callr.r-lib.org/dev/reference/r_vanilla.md)
  defaults to `FALSE` now
  ([\#194](https://github.com/r-lib/callr/issues/194)).

- It is now possible to set R environment variables (`R_ENVIRON_USER`,
  `R_PROFILE_USER`, etc.) via the `env` argument
  ([\#193](https://github.com/r-lib/callr/issues/193)).

## callr 3.6.0

CRAN release: 2021-03-28

- callr now supports starting an R process with a different
  architecture, so on Windows 64-bit R can start a 32-bit R background
  process, and vice-versa
  ([\#95](https://github.com/r-lib/callr/issues/95)).

- callr now handles symbolic arguments properly, and does not evaluate
  them. E.g. `callr::r(function(x) x, list(quote(foobar)))` works now
  ([\#175](https://github.com/r-lib/callr/issues/175)).

- [`callr::r_session`](https://callr.r-lib.org/dev/reference/r_session.md)
  does not leak file descriptors now in the sub-process
  ([\#184](https://github.com/r-lib/callr/issues/184)).

## callr 3.5.1

CRAN release: 2020-10-13

- [`callr::r_session`](https://callr.r-lib.org/dev/reference/r_session.md)
  now handles large messages from the subprocess well
  ([\#168](https://github.com/r-lib/callr/issues/168)).

## callr 3.5.0

CRAN release: 2020-10-08

- callr can now pass the environment of the function to the subprocess,
  optionally. This makes it easier to call an internal function of a
  package in a subprocess. See the `package` argument of
  [`r()`](https://callr.r-lib.org/dev/reference/r.md),
  [`r_bg()`](https://callr.r-lib.org/dev/reference/r_bg.md),
  `r_session$run()`, etc.
  ([\#147](https://github.com/r-lib/callr/issues/147)).

## callr 3.4.4

CRAN release: 2020-09-07

- An `r_session` now exits if the load hook errors. This generates an
  error if the session is started with `wait = TRUE`. For `wait = FALSE`
  the first `$read()` operation will return with an error
  ([\#162](https://github.com/r-lib/callr/issues/162)).

## callr 3.4.3

CRAN release: 2020-03-28

- [`default_repos()`](https://callr.r-lib.org/dev/reference/default_repos.md)
  now returns a list if `getOption("repos")` is a list, and a vector
  otherwise, on R 4.x.y as well.

## callr 3.4.2

CRAN release: 2020-02-12

- Improved error messages. Error messages are now fully printed after an
  error. In non-interactive sessions, the stack trace is printed as
  well.

## callr 3.4.1

CRAN release: 2020-01-24

- callr is now more careful when loading the local `.Rprofile` in the
  subprocess. This fixes issues with packrat and renv that use
  `.Rprofile` for setup
  ([\#139](https://github.com/r-lib/callr/issues/139)).

- callr functions fail early if environment file is missing
  ([\#123](https://github.com/r-lib/callr/issues/123),
  [@jdblischak](https://github.com/jdblischak))

## callr 3.4.0

CRAN release: 2019-12-09

- All callr functions and background processes properly clean up
  temporary files now
  ([\#104](https://github.com/r-lib/callr/issues/104)).

- callr now uses a more principled setup for the library path, and
  restores the related environment variables in the child process. This
  is a **breaking change** if you relied on having the library set in a
  [`system()`](https://rdrr.io/r/base/system.html) subprocess of the
  callr subprocess ([\#114](https://github.com/r-lib/callr/issues/114)).

- Better printing of `rlang_error`s that happened in the subprocess.

- The stacking of error objects is slightly different now, as we keep
  the unmodified error from the subprocess in `$parent$error`.

- callr now loads `.Rprofile` files from the current working directory
  by default. This works better with packrat, renv, and other software
  that relies on a local profile for initialization
  ([\#131](https://github.com/r-lib/callr/issues/131)).

## callr 3.3.2

CRAN release: 2019-09-22

No user visible changes in this version.

## callr 3.3.1

CRAN release: 2019-07-18

- `r_session` now avoids creating `data` and `env` objects in the global
  environment of the subprocess.

- New `$debug()` method for `r_session` to inspect the dumped frames in
  the subprocess, after an error.

## callr 3.3.0

CRAN release: 2019-07-04

- callr now sets the `.Last.error` variable for every uncaught callr
  error to the error condition, and also sets `.Last.error.trace` to its
  stack trace. If the error originates in the subprocess, then
  `.Last.error` is a hierarchical error object, and `.Last.error.trace`
  merges the traces from the two processes. See the `README.md` for an
  example.

- New `$traceback()` method for `r_session`, to run
  [`traceback()`](https://rdrr.io/r/base/traceback.html) in the
  subprocess, after an error.

- A callr subprocess now does not load any R packages by default.

- New vignette, that showcases `r_session`.

## callr 3.2.0

CRAN release: 2019-03-15

- [`r()`](https://callr.r-lib.org/dev/reference/r.md),
  [`rcmd()`](https://callr.r-lib.org/dev/reference/rcmd.md) and
  [`rscript()`](https://callr.r-lib.org/dev/reference/rscript.md) can
  now redirect the standard error of the subprocess its standard output.
  This allows to keep them correctly interleaved. For this, you need to
  either set the `stderr` argument to the special string `"2>&1"`, or to
  the same output file as specified for `stdout`.

- [`r()`](https://callr.r-lib.org/dev/reference/r.md),
  [`rcmd()`](https://callr.r-lib.org/dev/reference/rcmd.md) and
  [`rscript()`](https://callr.r-lib.org/dev/reference/rscript.md) now
  pass `...` arguments to
  [`processx::run()`](http://processx.r-lib.org/reference/run.md).
  [`r_bg()`](https://callr.r-lib.org/dev/reference/r_bg.md) and
  [`rcmd_bg()`](https://callr.r-lib.org/dev/reference/rcmd_bg.md) pass
  `...` arguments to the
  [`processx::process`](http://processx.r-lib.org/reference/process.md)
  constructor. For `r_process`, `rcmd_process` and `rscript_process`
  extra arguments can be specified as `options$extra`, these are also
  passed to the
  [`processx::process`](http://processx.r-lib.org/reference/process.md)
  constructor ([\#100](https://github.com/r-lib/callr/issues/100)).

## callr 3.1.1

CRAN release: 2018-12-21

- [`r()`](https://callr.r-lib.org/dev/reference/r.md),
  [`r_bg()`](https://callr.r-lib.org/dev/reference/r_bg.md), etc. now
  handle messages from the cliapp package properly. They used to make
  the R session exit.

- Better default for the `repos` option in callr subprocesses. callr no
  longer creates duplicate “CRAN” entries. By default the new
  [`default_repos()`](https://callr.r-lib.org/dev/reference/default_repos.md)
  function is used to set `repos` in the subprocess.

## callr 3.1.0

CRAN release: 2018-12-10

- New [`rscript()`](https://callr.r-lib.org/dev/reference/rscript.md)
  function and `rscript_process` class to execute R scripts via
  `Rscript` ([\#40](https://github.com/r-lib/callr/issues/40),
  [\#81](https://github.com/r-lib/callr/issues/81)).

- Library paths are now correctly set up for
  [`system()`](https://rdrr.io/r/base/system.html) (and similar) calls
  from the callr subprocesses
  ([\#83](https://github.com/r-lib/callr/issues/83),
  [\#84](https://github.com/r-lib/callr/issues/84)).

- Pass `options("repos")` to the child process as is, without checking.
  Closes [\#82](https://github.com/r-lib/callr/issues/82).

- `r_session$run_with_output()` now returns an S3 object with class
  `callr_session_result`.

- `r_session$run*()` handle interrupts properly. It tries to interrupt
  the background process fist, kills it if it is not interruptible, and
  then re-throws the interrupt condition, going back to the top level
  prompt if the re-thrown condition is uncaught.

## callr 3.0.0

CRAN release: 2018-08-24

- New `r_session` class: a background R session you can send commands to
  ([\#56](https://github.com/r-lib/callr/issues/56)).

- Rewrote passing the library path to the subprocess
  ([\#73](https://github.com/r-lib/callr/issues/73),
  [\#75](https://github.com/r-lib/callr/issues/75))

- Retain names of the `repos` option
  ([\#67](https://github.com/r-lib/callr/issues/67),
  [@jennybc](https://github.com/jennybc))

## callr 2.0.4

CRAN release: 2018-05-15

- pkgdown web site at <https://callr.r-lib.org>
  ([\#52](https://github.com/r-lib/callr/issues/52),
  [\#53](https://github.com/r-lib/callr/issues/53)).

- callr users `.Renviron` files now (and `R_ENVIRON_USER` as well), but
  overrides the library path, as requested in
  [`r()`](https://callr.r-lib.org/dev/reference/r.md), etc.
  ([\#30](https://github.com/r-lib/callr/issues/30)).

- callr now handles the case when the subprocess calls
  [`quit()`](https://rdrr.io/r/base/quit.html).

- callr now uses the processx package, instead of embedded code, to
  create and control processes.

## callr 2.0.3

CRAN release: 2018-04-11

- The default behavior on error can be set now with the `callr.error`
  option.

- Better error message if the child R process crashes or gets killed.
  ([\#41](https://github.com/r-lib/callr/issues/41))

- `r_bg` and `rcmd_bg` now have the `supervise` option
  ([\#45](https://github.com/r-lib/callr/issues/45)).

## callr 2.0.2

CRAN release: 2018-02-11

- Fix a bug with R-devel, caused by the change on 2018-02-08:
  <https://github.com/wch/r-source/commit/924582943706100e88a11d6bb0585d25779c91f5>
  [\#37](https://github.com/r-lib/callr/issues/37),
  [\#38](https://github.com/r-lib/callr/issues/38)

- Fix a race condition on Windows, when creating named pipes for
  `stdout` or `stderr`. The client sometimes didn’t wait for the server,
  and callr failed with ERROR_PIPE_BUSY (231, All pipe instances are
  busy).

## callr 2.0.1

CRAN release: 2018-01-30

- Fix compilation issues on Solaris

- Fix a test failure on macOS

## callr 2.0.0

CRAN release: 2018-01-28

- Run R or `R CMD` in the background, see
  [`r_bg()`](https://callr.r-lib.org/dev/reference/r_bg.md),
  [`rcmd_bg()`](https://callr.r-lib.org/dev/reference/rcmd_bg.md), and
  also `r_process` and `rcmd_process`

- The defaults for [`r()`](https://callr.r-lib.org/dev/reference/r.md)
  are safer now, the match the defaults of
  [`r_safe()`](https://callr.r-lib.org/dev/reference/r.md).
  [`r_safe()`](https://callr.r-lib.org/dev/reference/r.md) is kept for
  compatibility.
  [`r_copycat()`](https://callr.r-lib.org/dev/reference/r_copycat.md)
  has the old [`r()`](https://callr.r-lib.org/dev/reference/r.md)
  defaults.

- The defaults for
  [`rcmd()`](https://callr.r-lib.org/dev/reference/rcmd.md) are safer
  now, the match the defaults of
  [`rcmd_safe()`](https://callr.r-lib.org/dev/reference/rcmd.md).
  [`rcmd_safe()`](https://callr.r-lib.org/dev/reference/rcmd.md) is kept
  for compatibility.
  [`rcmd_copycat()`](https://callr.r-lib.org/dev/reference/rcmd_copycat.md)
  has the old [`rcmd()`](https://callr.r-lib.org/dev/reference/rcmd.md)
  defaults.

- Support block callbacks, in addition to line callbacks. Block
  callbacks are called for arbitrary chunks of output, even without a
  newline

- Add `spinner` argument to show a spinner in
  [`r()`](https://callr.r-lib.org/dev/reference/r.md) or
  [`rcmd()`](https://callr.r-lib.org/dev/reference/rcmd.md)

- Support timeouts, via the `timeout` argument

- Fix bug when `stdout` and `stderr` are redirected to the same file

- [`rcmd_safe_env()`](https://callr.r-lib.org/dev/reference/rcmd_safe_env.md)
  to allow extending the environment variables set in safe mode

- [`rcmd()`](https://callr.r-lib.org/dev/reference/rcmd.md) gets a
  `fail_on_status` argument

- [`rcmd()`](https://callr.r-lib.org/dev/reference/rcmd.md) gets an
  `echo` argument to potentially show the command to be run on the
  screen ([\#15](https://github.com/r-lib/callr/issues/15))

- [`rcmd()`](https://callr.r-lib.org/dev/reference/rcmd.md) gets a `wd`
  argument to set the working directory

## callr 1.0.0

CRAN release: 2016-06-18

First public release.
