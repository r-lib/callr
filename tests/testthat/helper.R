
skip_other_platforms <- function(platform) {
  if (os_type() != platform) skip(paste("only run it on", platform))
}

try_silently <- function(expr) {
  tryCatch(
    expr,
    error = function(x) "error",
    warning = function(x) "warning",
    message = function(x) "message"
  )
}

get_tool <- function(prog) {
  if (os_type() == "windows") prog <- paste0(prog, ".exe")
  exe <- system.file(package = "callr", "bin", .Platform$r_arch, prog)
  if (exe == "") {
    pkgpath <- system.file(package = "callr")
    if (basename(pkgpath) == "inst") pkgpath <- dirname(pkgpath)
    exe <- file.path(pkgpath, "src", "tools", prog)
    if (!file.exists(exe)) return("")
  }
  exe
}

get_pid_by_name <- function(name) {
  if (os_type() == "windows") {
    get_pid_by_name_windows(name)
  } else if (is_linux()) {
    get_pid_by_name_linux(name)
  } else {
    get_pid_by_name_unix(name)
  }
}

get_pid_by_name_windows <- function(name) {
  ## TODO
}

## Linux does not exclude the ancestors of the pgrep process
## from the list, so we have to do that manually. We remove every
## process that contains 'pgrep' in its command line, which is
## not the proper solution, but for testing it will do.
##
## Unfortunately Ubuntu 12.04 pgrep does not have a -a switch,
## so we cannot just output the full command line and then filter
## it in R. So we first run pgrep to get all matching process ids
## (without their command lines), and then use ps to list the processes
## again. At this time the first pgrep process is not running any
## more, but another process might have its id, so we filter again the
## result for 'name'

get_pid_by_name_linux <- function(name) {
  ## TODO
}
