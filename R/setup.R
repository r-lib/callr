
setup_script_files <- function(options) {
  within(options, {
    func_file   <- save_function_to_temp(options)
    result_file <- tempfile()
    script_file <- make_vanilla_script(func_file, result_file, options$error)
    tmp_files <- c(tmp_files, func_file, script_file, result_file)
  })
}

save_function_to_temp <- function(options) {
  tmp <- tempfile()
  environment(options$func) <- .GlobalEnv
  saveRDS(list(options$func, options$args), file = tmp)
  tmp
}

setup_context <- function(options) {

  ## Avoid R CMD check warning...
  repos <- libpath <- system_profile <- user_profile <- NULL

  within(options, {
    ## profiles
    profile <- make_profile(repos)
    tmp_files <- c(tmp_files, profile)

    ## Temporary library path
    lib <- paste(libpath, collapse = .Platform$path.sep)

    ## Workaround, R ignores "", need to set to non-existant file
    if (lib == "") lib <- tempfile()

    ## LIB and PROFILE env vars
    if (is.na(env["R_LIBS"])) env["R_LIBS"] <- lib
    if (is.na(env["R_LIBS_USER"])) env["R_LIBS_USER"] <- lib
    if (is.na(env["R_LIBS_SITE"])) env["R_LIBS_SITE"] <- lib
    if (!system_profile) env["R_PROFILE"] <- profile
    if (!user_profile) env["R_PROFILE_USER"] <- profile
  })
}


make_profile <- function(repos) {
  profile <- tempfile()
  cat("options(repos=", deparse(repos), ")\n", sep = "", file = profile)
  profile
}

setup_callbacks <- function(options) {

  ## We cannot easily use `within` here, because the
  ## functions we create will have the wrong environment

  cb <- options$callback
  block_cb <- options$block_callback

  ## This is cumbersome, because we cannot easily set a named list
  ## element to NULL
  options <- append(
    options,
    list("real_block_callback" =
           if (!is.null(block_cb)) function(x, proc) block_cb(x))
  )

  callback_factory <- function(stream) {
    ## Need to evaluate it when the callback is created
    force(stream)

    ## In case there is no output, we create an empty file here
    if (!is.null(stream)) cat("", file = stream)

    if (!is.null(cb)) {
      function(x, proc) {
        if (!is.null(stream)) cat(x, file = stream, sep = "\n", append = TRUE)
        cb(x)
      }

    } else {
      function(x, proc) {
        if (!is.null(stream)) cat(x, file = stream, sep = "\n", append = TRUE)
      }
    }
  }

  options <- append(options, list("real_callback" = callback_factory))
  options
}

setup_r_binary_and_args <- function(options) {
  exec <- if (os_platform() == "windows") "Rterm" else "R"
  options$bin <- file.path(R.home("bin"), exec)
  options$real_cmdargs <- c(options$cmdargs, "-f", options$script_file)
  options
}

setup_rcmd_binary_and_args <- function(options) {

  if(os_platform() == "windows") {
    options$bin <- file.path(R.home("bin"), "Rcmd.exe")
    options$real_cmdargs <- c(options$cmd, options$cmdargs)

  } else {
    options$bin <- file.path(R.home("bin"), "R")
    options$real_cmdargs <- c("CMD", options$cmd, options$cmdargs)
  }

  options
}
