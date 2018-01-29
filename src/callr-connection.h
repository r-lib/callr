
#ifndef CALLR_CONNECTION_H
#define CALLR_CONNECTION_H

#include <Rinternals.h>
#include <R_ext/Riconv.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

/* --------------------------------------------------------------------- */
/* Data types                                                            */
/* --------------------------------------------------------------------- */

#ifdef _WIN32
typedef HANDLE callr_file_handle_t;
typedef struct {
  HANDLE handle;
  BOOLEAN async;
  OVERLAPPED overlapped;
  BOOLEAN read_pending;
} callr_i_connection_t;
#else
typedef int callr_file_handle_t;
typedef int callr_i_connection_t;
#endif

typedef enum {
  CALLR_FILE_TYPE_FILE = 1,	/* regular file, blocking IO */
  CALLR_FILE_TYPE_ASYNCFILE,	/* regular file, async IO (well, win only) */
  CALLR_FILE_TYPE_PIPE,	/* pipe, blocking IO */
  CALLR_FILE_TYPE_ASYNCPIPE	/* pipe, async IO */
} callr_file_type_t;

typedef struct callr_connection_s {
  callr_file_type_t type;

  int is_closed_;
  int is_eof_;			/* the UTF8 buffer */
  int is_eof_raw_;		/* the raw file */

  char *encoding;
  void *iconv_ctx;

  callr_i_connection_t handle;

  char* buffer;
  size_t buffer_allocated_size;
  size_t buffer_data_size;

  char *utf8;
  size_t utf8_allocated_size;
  size_t utf8_data_size;

} callr_connection_t;

/* Generic poll method
 *
 * @param object The thing to poll.
 * @param status Currently not used.
 * @param handle A handle can be returned here, to `poll` or wait on.
 *   If this is not needed, set it to NULL.
 * @param timeout A timeout value can be returned here, for the next
 *   poll. If this is not needed, set it to NULL.
 * @return The result of the pre-polling. PXCLOSED, PXREADY or PXSILENT.
 *   PXREADY: data is readily available, at least one character.
 *     (But maybe not a full line.)
 *   PXSILENT: we don't know if data is available, we need to check the
 *     operating system via `poll` or `WaitForStatus`.
 */

typedef int (*callr_connection_poll_func_t)(
  void *object,
  int status,
  callr_file_handle_t *handle,
  int *again);

/* Data structure for a pollable object
 *
 * @member poll_func The function to call on the object, before
 *   the poll/wait system call. The pollable object might have data
 *   available without immediately, without poll/wait. If not, it
 *   will return the file descriptor or HANDLE to poll.
 * @member object The object to pass to `poll_func`.
 * @member free Whether to call `free()` on `object` when finalizing
 *   `callr_pollable_t` objects.
 * @member event The result of the polling is stored here. Possible values:
 *   `PXSILENT` (no data), `PXREADY` (data), `PXTIMEOUT` (timeout).
 */

typedef struct callr_pollable_s {
  callr_connection_poll_func_t poll_func;
  void *object;
  int free;
  int event;
} callr_pollable_t;

/* --------------------------------------------------------------------- */
/* API from R                                                            */
/* --------------------------------------------------------------------- */

/* Create connection from fd / HANDLE */
SEXP callr_connection_create(SEXP handle, SEXP encoding);

/* Read characters in a given encoding from the connection. */
SEXP callr_connection_read_chars(SEXP con, SEXP nchars);

/* Read lines of characters from the connection. */
SEXP callr_connection_read_lines(SEXP con, SEXP nlines);

/* Check if the connection has ended. */
SEXP callr_connection_is_eof(SEXP con);

/* Close the connection. */
SEXP callr_connection_close(SEXP con);
SEXP callr_is_closed(SEXP con);

/* Poll connections and other pollable handles */
SEXP callr_connection_poll(SEXP pollables, SEXP timeout);

/* --------------------------------------------------------------------- */
/* API from C                                                            */
/* --------------------------------------------------------------------- */

/* Create connection object */
callr_connection_t *callr_c_connection_create(
  callr_file_handle_t os_handle,
  callr_file_type_t type,
  const char *encoding,
  SEXP *r_connection);

/* Destroy connection object. We need this for the C API */
void callr_c_connection_destroy(callr_connection_t *ccon);

/* Read characters */
ssize_t callr_c_connection_read_chars(
  callr_connection_t *con,
  void *buffer,
  size_t nbyte);

/* Read lines of characters */
ssize_t callr_c_connection_read_line(
  callr_connection_t *ccon,
  char **linep,
  size_t *linecapp);

/* Check if the connection has ended */
int callr_c_connection_is_eof(
  callr_connection_t *con);

/* Close */
void callr_c_connection_close(
  callr_connection_t *con);
int callr_c_connection_is_closed(
  callr_connection_t *con);

/* Poll connections and other pollable handles */
int callr_c_connection_poll(
  callr_pollable_t pollables[],
  size_t npollables, int timeout);

/* Helper function to create pollable handles*/
int callr_c_pollable_from_connection(
  callr_pollable_t *pollable,
  callr_connection_t *ccon);

/* --------------------------------------------------------------------- */
/* Internals                                                             */
/* --------------------------------------------------------------------- */

#ifndef _WIN32
typedef unsigned long DWORD;
#endif

#define CALLR_ERROR(m,c) callr__error((m),(c),__FILE__,__LINE__)
void callr__error(const char *message, DWORD errorcode,
		     const char *file, int line);

#endif
