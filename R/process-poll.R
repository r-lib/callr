
#' Poll for process I/O or termination
#'
#' Wait until one of the specified processes produce standard output
#' or error, terminates, or a timeout occurs.
#'
#' @section Explanation of the return values:
#' * `nopipe` means that the stdout or stderr from this process was not
#'   captured.
#' * `ready` means that stdout or stderr from this process are ready to
#'   read from. Note that end-of-file on these outputs also triggers
#'   `ready`.
#' * timeout`: the processes are not ready to read from and a timeout
#'   happened.
#' * `closed`: the connection was already closed, before the polling
#'   started.
#' * `silent`: the connection is not ready to read from, but another
#'   connection was.
#'
#' @section Known issues:
#'
#' `poll()` cannot wait on the termination of a process directly. It is
#' only signalled through the closed stdout and stderr pipes. This means
#' that if both stdout and stderr are ignored or closed for a process,
#' then you will not be notified when it exits. If you want to wait for
#' just a single process to end, it can be done with the `$wait()` method.
#'
#' @param processes A list of `process` objects to wait on. If this is a
#'   named list, then the returned list will have the same names. This
#'   simplifies the identification of the processes. If an empty list,
#'   then the
#' @param ms Integer scalar, a timeout for the polling, in milliseconds.
#'   Supply -1 for an infitite timeout, and 0 for not waiting at all.
#' @return A list of character vectors of length two. There is one list
#'   element for each process, in the same order as in the input list.
#'   The character vectors' elements are named `output` and `error` and
#'   their possible values are: `nopipe`, `ready`, `timeout`, `closed`,
#'   `silent`. See details about these below.
#'
#' @export
#' @examples
#' ## Different commands to run for windows and unix
#' \dontrun{
#' cmd1 <- switch(
#'   .Platform$OS.type,
#'   "unix" = c("sh", "-c", "sleep 1; ls"),
#'   c("cmd", "/c", "ping -n 2 127.0.0.1 && dir /b")
#' )
#' cmd2 <- switch(
#'   .Platform$OS.type,
#'   "unix" = c("sh", "-c", "sleep 2; ls 1>&2"),
#'   c("cmd", "/c", "ping -n 2 127.0.0.1 && dir /b 1>&2")
#' )
#'
#' ## Run them. p1 writes to stdout, p2 to stderr, after some sleep
#' p1 <- process$new(cmd1[1], cmd1[-1], stdout = "|")
#' p2 <- process$new(cmd2[1], cmd2[-1], stderr = "|")
#'
#' ## Nothing to read initially
#' poll(list(p1 = p1, p2 = p2), 0)
#'
#' ## Wait until p1 finishes. Now p1 has some output
#' p1$wait()
#' poll(list(p1 = p1, p2 = p2), -1)
#'
#' ## Close p1's connection, p2 will have output on stderr, eventually
#' close(p1$get_output_connection())
#' poll(list(p1 = p1, p2 = p2), -1)
#'
#' ## Close p2's connection as well, no nothing to poll
#' close(p2$get_error_connection())
#' poll(list(p1 = p1, p2 = p2), 0)
#' }

poll <- function(processes, ms) {
  assert_that(is_list_of_processes(processes))
  assert_that(is_integerish_scalar(ms))
  if (length(processes) == 0) {
    return(structure(list(), names = names(processes)))
  }
  statuses <- lapply(processes, function(p) {
    p$.__enclos_env__$private$status
  })

  res <- lapply(
    .Call(c_callr_poll, statuses, as.integer(ms)),
    function(x) structure(poll_codes[x], names = c("output", "error"))
  )

  structure(res, names = names(processes))
}
