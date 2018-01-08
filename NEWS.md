
# 1.1.0

* `callr` suppresses `R_ENVIRON_USER` now.

* The defaults for `r()` are safer now, the match the defaults of
  `r_safe()`. `r_safe()` is kept for compatibility. `r_copycat()`
  has the old `r()` defaults.

* The defaults for `rcmd()` are safer now, the match the defaults of
  `rcmd_safe()`. `rcmd_safe()` is kept for compatibility. `rcmd_copycat()`
  has the old `rcmd()` defaults.

* Command output is converted to UTF-8

* Print stderr on `fail_on_status` (#16)

* Fix bug when stdout and stderr are redirected to the same file

* `rcmd_safe_env()` to allow extending the environment variables set in
  safe mode

* `rcmd()` gets a `fail_on_status` argument

* `rcmd()` result contains the invoked command now

* `rcmd()` gets an `echo` argument to potentially show the command to be
  run on the screen (#15)

* `rcmd()` gets a `wd` argument to set the working directory

# 1.0.0

First public release.
