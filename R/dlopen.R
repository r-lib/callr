load_r <- function() {
  rlib <- file.path(R.home("lib"), "libR.dylib")
  dir.create(tmp <- tempfile())
  path <- file.path(tmp, "libR.dylib")
  file.copy(rlib, path)
  .Call(c_load_r, path)
}

init_r <- function(R) {
  .Call(c_init_r, R)
  invisible()
}

eval_r <- function(R, expr) {
  .Call(c_eval_r, R, expr)
  invisible()
}
