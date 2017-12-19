
.onLoad <- function(libname, pkgname) {
  supervisor_reset()
  debugme::debugme()                    # nocov
}

.onUnload <- function(libpath) {
  if (os_type() != "windows") .Call(c_callr__killem_all)
  supervisor_reset()
}
