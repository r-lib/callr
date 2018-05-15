
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
    profile <- make_profile(system_profile, user_profile, repos, libpath)
    tmp_files <- c(tmp_files, profile)

    ## Lib path is set in the profile
    empty <- tempfile()
    cat("", file =  empty)
    tmp_files  <- c(tmp_files, empty)
    if (is.na(env["R_LIBS"])) env["R_LIBS"] <- empty
    if (is.na(env["R_LIBS_USER"])) env["R_LIBS_USER"] <- empty
    if (is.na(env["R_LIBS_SITE"])) env["R_LIBS_SITE"] <- empty
    if (is.na(env["R_PROFILE"])) env["R_PROFILE"] <- empty
    if (is.na(env["R_PROFILE_USER"])) env["R_PROFILE_USER"] <- profile
  })
}


## We need to combine all profiles into a single one. The combined profile
## might include the system and user profile, depending on options.
## It always includes the `repos` and `.libPaths()` settingd.
##
## We set  lib path here as well, in case it was set in .Renviron, etc.,
## because the supplied libpath should take precedence over .Renviron.

make_profile <- function(system, user, repos, libpath) {
  profile <- tempfile()

  ## Create file
  cat("", file = profile)

  ## Add profiles
  if (system) {
    sys <- Sys.getenv("R_PROFILE",
                      file.path(R.home("etc"), "Rprofile.site"))
    sys <- path.expand(sys)
    if (file.exists(sys)) file.append(profile, sys)
  }

  if (user) {
    user <- Sys.getenv("R_PROFILE_USER", NA_character_)
    local <- ".Rprofile"
    home  <- path.expand("~/.Rprofile")
    if (is.na(user) && file.exists(local)) user <- local
    if (is.na(user) && file.exists(home)) user <- home
    if (!is.na(user) && file.exists(user)) file.append(profile, user)
  }

  ## Override repos and library path, as requested
  cat("options(repos=", deparse(repos), ")\n", sep = "", file = profile,
      append  = TRUE)
  if (!is.null(libpath)) {
    cat(".libPaths(", deparse(libpath), ")\n", sep = "", file = profile,
        append = TRUE)
  }

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
