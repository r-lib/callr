
#include "../callr.h"

#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>


callr_connection_t* callr__create_connection(
  int fd, const char *membername,
  SEXP private,
  const char *encoding) {

  callr_connection_t *con;
  SEXP res;

  con = callr_c_connection_create(fd, CALLR_FILE_TYPE_ASYNCPIPE,
				     encoding, &res);

  defineVar(install(membername), res, private);

  return con;
}

void callr__create_connections(callr_handle_t *handle, SEXP private,
				  const char *encoding) {
  handle->pipes[0] = handle->pipes[1] = handle->pipes[2] = 0;

  if (handle->fd1 >= 0) {
    handle->pipes[1] = callr__create_connection(
      handle->fd1,
      "stdout_pipe",
      private,
      encoding);
  }

  if (handle->fd2 >= 0) {
    handle->pipes[2] = callr__create_connection(
      handle->fd2,
      "stderr_pipe",
      private,
      encoding);
  }
}
