
# callr 3.0.0

* New `r_session` class: a background R session you can send commands to
  (#56).

* Rewrote passing the library path to the subprocess (#73, #75)

* Retain names of the `repos` option (#67, @jennybc)

# callr 2.0.4

* pkgdown web site at https://callr.r-lib.org  (#52, #53).

* callr users `.Renviron` files now (and `R_ENVIRON_USER` as well),
  but overrides the library path, as requested in `r()`, etc. (#30).

* callr now handles the case when the subprocess calls `quit()`.

* callr now uses the processx package, instead of embedded code,
  to create and control processes.

# callr 2.0.3

* The default behavior on error can be set now with the `callr.error`
option.

* Better error message if the child R process crashes or gets killed. (#41)

* `r_bg` and `rcmd_bg` now have the `supervise` option (#45).

# callr 2.0.2

* Fix a bug with R-devel, caused by the change on 2018-02-08:
  https://github.com/wch/r-source/commit/924582943706100e88a11d6bb0585d25779c91f5
  #37, #38

* Fix a race condition on Windows, when creating named pipes for stdout
  or stderr. The client sometimes didn't wait for the server, and callr
  failed with ERROR_PIPE_BUSY (231, All pipe instances are busy).

# callr 2.0.1

* Fix compilation issues on CRAN's Solaris machine

* Fix a test failure on CRAN's macOS machine

# callr 2.0.0

* Run R or R CMD * in the background, see `r_bg()`, `rcmd_bg()`,
  and also `r_process` and `rcmd_process`

* The defaults for `r()` are safer now, the match the defaults of
  `r_safe()`. `r_safe()` is kept for compatibility. `r_copycat()` has the
  old `r()` defaults.

* The defaults for `rcmd()` are safer now, the match the defaults of
`rcmd_safe()`. `rcmd_safe()` is kept for compatibility. `rcmd_copycat()`
  has the old `rcmd()` defaults.

* Support block callbacks, in addition to line callbacks. Block callbacks
  are called for arbitrary chunks of output, even without a newline

* Add `spinner` argument to show a spinner in `r()` or `rcmd()`

* Support timeouts, via the `timeout` argument

* Fix bug when stdout and stderr are redirected to the same file

* `rcmd_safe_env()` to allow extending the environment variables set in
  safe mode

* `rcmd()` gets a `fail_on_status` argument

* `rcmd()` gets an `echo` argument to potentially show the command to be
  run on the screen (#15)

* `rcmd()` gets a `wd` argument to set the working directory

# callr 1.0.0

First public release.
