
load_client_lib <- function(sofile = NULL) {
  ext <- .Platform$dynlib.ext
  if (is.null(sofile)) {
    arch <- .Platform$r_arch
    sofile <- system.file(
      "libs", arch, paste0("client", ext),
      package = "processx")

    # Try this as well, this is for devtools/pkgload
    if (sofile == "") {
      sofile <- system.file(
        "src", paste0("client", ext),
        package = "processx")
    }

    # stop() here and not throw(), because this function should be standalone
    if (sofile == "") stop("Cannot find client file")
  }

  tmpsofile <- tempfile(fileext = ext)
  file.copy(sofile, tmpsofile)
  tmpsofile <- normalizePath(tmpsofile)

  lib <- dyn.load(tmpsofile)
  on.exit(dyn.unload(tmpsofile))

  sym_encode <- getNativeSymbolInfo("processx_base64_encode", lib)
  sym_decode <- getNativeSymbolInfo("processx_base64_decode", lib)
  sym_disinh <- getNativeSymbolInfo("processx_disable_inheritance", lib)
  sym_write  <- getNativeSymbolInfo("processx_write", lib)
  sym_setout <- getNativeSymbolInfo("processx_set_stdout", lib)
  sym_seterr <- getNativeSymbolInfo("processx_set_stderr", lib)
  sym_setoutf <- getNativeSymbolInfo("processx_set_stdout_to_file", lib)
  sym_seterrf <- getNativeSymbolInfo("processx_set_stderr_to_file", lib)

  env <- new.env(parent = emptyenv())
  env$.path <- tmpsofile

  mycall <- .Call

  env$base64_encode <- function(x) rawToChar(mycall(sym_encode, x))
  env$base64_decode <- function(x) {
    if (is.character(x)) {
      x <- charToRaw(paste(gsub("\\s+", "", x), collapse = ""))
    }
    mycall(sym_decode, x)
  }

  env$disable_fd_inheritance <- function() mycall(sym_disinh)

  env$write_fd <- function(fd, data) {
    if (is.character(data)) data <- charToRaw(paste0(data, collapse = ""))
    len <- length(data)
    repeat {
      written <- mycall(sym_write, fd, data)
      len <- len - written
      if (len == 0) break
      if (written) data <- data[-(1:written)]
      Sys.sleep(.1)
    }
  }

  env$set_stdout <- function(fd, drop = TRUE) {
    mycall(sym_setout, as.integer(fd), as.logical(drop))
  }

  env$set_stderr <- function(fd, drop = TRUE) {
    mycall(sym_seterr, as.integer(fd), as.logical(drop))
  }

  env$set_stdout_file <- function(path) {
    mycall(sym_setoutf, as.character(path)[1])
  }

  env$set_stderr_file <- function(path) {
    mycall(sym_seterrf, as.character(path)[1])
  }

  env$.finalize <- function() {
    dyn.unload(env$.path)
    rm(list = ls(env, all.names = TRUE), envir = env)
  }

  penv <- environment()
  parent.env(penv) <- baseenv()

  reg.finalizer(
    env,
    function(e) if (".finalize" %in% names(e)) e$.finalize(),
    onexit = TRUE)

  ## Clear the cleanup method
  on.exit(NULL)
  env
}
