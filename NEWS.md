
# 2.0.1

* Fix compilation issues on CRAN's Solarix machine

* Fix a test failure on CRAN's macOS machine

# 2.0.0

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

# 1.0.0

First public release.
