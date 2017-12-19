
process__exists <- function(pid) {
  .Call(c_processx__process_exists, pid)
}
