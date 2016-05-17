
safe_system <- function(command, args, callback = NULL) {

  if (!is.null(callback)) {
    safe_system_callback(command, args, callback)

  } else {
    safe_system_sync(command, args)
  }
}

safe_system_sync <- function(command, args) {

  out <- tempfile()
  err <- tempfile()
  on.exit(unlink(out), add = TRUE)
  on.exit(unlink(err), add = TRUE)

  ## We suppress warnings, they are given if the command
  ## exits with a non-zero status
  suppressWarnings(
    res <- system2(command, args = args, stdout = out, stderr = err)
  )

  list(
    stdout = win2unix(read_char(out)),
    stderr = win2unix(read_char(err)),
    status = res
  )
}

safe_system_callback <- function(command, args, callback) {

  stopifnot(is.function(callback))

  err <- tempfile()
  on.exit(unlink(err), add = TRUE)

  ## Need to redirect error to a file, we can only get stdout
  ## from the pipe
  commandline <- paste(command, paste(args, collapse = " "))

  pip <- pipe(paste(commandline, "2>", err))
  open(pip)
  on.exit(try(close(pip), silent = TRUE), add = TRUE)

  ## readLines blocks until there is sg to read.
  ## When eof, it returns an empty character vector
  output <- character(0)
  line <- readLines(pip, n = 1, warn = FALSE)
  while (length(line)) {
    output <- c(output, line)
    callback(line)
    line <- readLines(pip, n = 1, warn = FALSE)
  }

  res <- close(pip)

  list(
    stdout = paste(output, collapse = "\n"),
    stderr = read_char(err),
    status = res
  )
}
