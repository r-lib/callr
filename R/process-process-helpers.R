
process__exists <- function(pid) {
  .Call(c_callr__process_exists, pid)
}
