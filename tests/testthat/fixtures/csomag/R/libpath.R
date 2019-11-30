
file_from_env <- function(var) {
  if (file.exists(f <- Sys.getenv(var))) readLines(f)
}

if (Sys.getenv("CALLR_DUMP_HERE") != "") {
  saveRDS(
    list(
      env = Sys.getenv(),
      libpaths = .libPaths(),
      search = search(),
      R_ENVIRON = file_from_env("R_ENVIRON"),
      R_ENVIRON_USER = file_from_env("R_ENVIRON_USER"),
      R_PROFILE = file_from_env("R_PROFILE"),
      R_PROFILE_USER = file_from_env("R_PROFILE_USER")
    ),
    file = Sys.getenv("CALLR_DUMP_HERE")
  )
}
