
# 1.0.0.9000

* Use the `processx` package to start R processes

* Run R or R CMD * in the background, see `r_bg()`, `r_bg_safe()`,
  `rcmd_bg()`, `rcmd_bg_safe()`, and also `r_process` and `rcmd_process`

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
