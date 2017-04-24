
#' @importFrom processx run

run_r <- function(options) {

  oldwd <- getwd()
  setwd(options$wd)
  on.exit(setwd(oldwd), add = TRUE)

  with(
    options,
    with_envvar(
      env,
      run(
        bin, args = real_cmdargs, stdout_line_callback = real_callback,
        stderr_line_callback = real_callback,
        stdout_callback = real_block_callback,
        stderr_callback = real_block_callback, echo_cmd = echo,
        echo = show, spinner = spinner, error_on_status = fail_on_status,
        timeout = timeout
      )
    )
  )
}

run_r_bg <- function(options) {

  oldwd <- getwd()
  setwd(options$wd)
  on.exit(setwd(oldwd), add = TRUE)

  with(
    options,
    with_envvar(
      env,
      process$new(bin, real_cmdargs, stdout = stdout, stderr = stderr)
    )
  )
}
