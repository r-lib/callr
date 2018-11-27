
run_r <- function(options) {

  oldwd <- getwd()
  setwd(options$wd)
  on.exit(setwd(oldwd), add = TRUE)

  res <- with(
    options,
    with_envvar(
      env,
      processx::run(
        bin, args = real_cmdargs,
        stdout_line_callback = real_callback(stdout),
        stderr_line_callback = real_callback(stderr),
        stdout_callback = real_block_callback,
        stderr_callback = real_block_callback, echo_cmd = echo,
        echo = show, spinner = spinner, error_on_status = fail_on_status,
        timeout = timeout
      )
    )
  )

  res$command <- c(options$bin, options$real_cmdargs)
  res
}
