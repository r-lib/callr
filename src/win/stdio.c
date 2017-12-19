
#include <R.h>

#include "../callr.h"

/*
 * The `child_stdio_buffer` buffer has the following layout:
 *   int number_of_fds
 *   unsigned char crt_flags[number_of_fds]
 *   HANDLE os_handle[number_of_fds]
 */
#define CHILD_STDIO_SIZE(count)                     \
    (sizeof(int) +                                  \
     sizeof(unsigned char) * (count) +              \
     sizeof(uintptr_t) * (count))

#define CHILD_STDIO_COUNT(buffer)                   \
    *((unsigned int*) (buffer))

#define CHILD_STDIO_CRT_FLAGS(buffer, fd)           \
    *((unsigned char*) (buffer) + sizeof(int) + fd)

#define CHILD_STDIO_HANDLE(buffer, fd)              \
    *((HANDLE*) ((unsigned char*) (buffer) +        \
                 sizeof(int) +                      \
                 sizeof(unsigned char) *            \
                 CHILD_STDIO_COUNT((buffer)) +      \
                 sizeof(HANDLE) * (fd)))

/* CRT file descriptor mode flags */
#define FOPEN       0x01
#define FEOFLAG     0x02
#define FCRLF       0x04
#define FPIPE       0x08
#define FNOINHERIT  0x10
#define FAPPEND     0x20
#define FDEV        0x40
#define FTEXT       0x80

static int callr__create_nul_handle(HANDLE *handle_ptr, DWORD access) {
  HANDLE handle;
  SECURITY_ATTRIBUTES sa;

  sa.nLength = sizeof(sa);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;

  handle = CreateFileW(
    /* lpFilename =            */ L"NUL",
    /* dwDesiredAccess=        */ access,
    /* dwShareMode =           */ FILE_SHARE_READ | FILE_SHARE_WRITE,
    /* lpSecurityAttributes =  */ &sa,
    /* dwCreationDisposition = */ OPEN_EXISTING,
    /* dwFlagsAndAttributes =  */ 0,
    /* hTemplateFile =         */ NULL);
  if (handle == INVALID_HANDLE_VALUE) { return GetLastError(); }

  *handle_ptr = handle;
  return 0;
}

static int callr__create_output_handle(HANDLE *handle_ptr, const char *file,
					  DWORD access) {
  HANDLE handle;
  SECURITY_ATTRIBUTES sa;
  int err;

  sa.nLength = sizeof(sa);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;
  WCHAR *filew;

  err = callr__utf8_to_utf16_alloc(file, &filew);
  if (err) return(err);

  handle = CreateFileW(
    /* lpFilename =            */ filew,
    /* dwDesiredAccess=        */ access,
    /* dwShareMode =           */ FILE_SHARE_READ | FILE_SHARE_WRITE,
    /* lpSecurityAttributes =  */ &sa,
    /* dwCreationDisposition = */ CREATE_ALWAYS,
    /* dwFlagsAndAttributes =  */ 0,
    /* hTemplateFile =         */ NULL);
  if (handle == INVALID_HANDLE_VALUE) { return GetLastError(); }

  /* We will append, so set pointer to end of file */
  SetFilePointer(handle, 0, NULL, FILE_END);

  *handle_ptr = handle;
  return 0;
}

static void callr__unique_pipe_name(char* ptr, char* name, size_t size) {
  snprintf(name, size, "\\\\?\\pipe\\px\\%p-%lu", ptr, GetCurrentProcessId());
}

int callr__create_pipe(void *id, HANDLE* parent_pipe_ptr, HANDLE* child_pipe_ptr) {

  char pipe_name[40];
  HANDLE hOutputRead = INVALID_HANDLE_VALUE;
  HANDLE hOutputWrite = INVALID_HANDLE_VALUE;
  SECURITY_ATTRIBUTES sa;
  DWORD err;
  char *errmessage = "";

  sa.nLength = sizeof(sa);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;

  callr__unique_pipe_name(id, pipe_name, sizeof(pipe_name));

  hOutputRead = CreateNamedPipeA(
    pipe_name,
    PIPE_ACCESS_OUTBOUND | PIPE_ACCESS_INBOUND |
      FILE_FLAG_OVERLAPPED | FILE_FLAG_FIRST_PIPE_INSTANCE,
    PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT,
    1,
    65536,
    65536,
    0,
    NULL);
  if (hOutputRead == INVALID_HANDLE_VALUE) {
    err = GetLastError();
    errmessage = "creating read pipe";
    goto error;
  }

  hOutputWrite = CreateFileA(
    pipe_name,
    GENERIC_WRITE,
    0,
    &sa,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    NULL);
  if (hOutputWrite == INVALID_HANDLE_VALUE) {
    err = GetLastError();
    errmessage = "creating write pipe";
    goto error;
  }

  *parent_pipe_ptr = hOutputRead;
  *child_pipe_ptr  = hOutputWrite;

  return 0;

 error:
  if (hOutputRead != INVALID_HANDLE_VALUE) CloseHandle(hOutputRead);
  if (hOutputWrite != INVALID_HANDLE_VALUE) CloseHandle(hOutputWrite);
  CALLR_ERROR(errmessage, err);
  return 0;			/* never reached */
}



callr_connection_t * callr__create_connection(
  HANDLE pipe_handle, const char *membername, SEXP private,
  const char *encoding) {

  callr_connection_t *con;
  SEXP res;

  con = callr_c_connection_create(pipe_handle, CALLR_FILE_TYPE_ASYNCPIPE,
				     encoding, &res);

  defineVar(install(membername), res, private);

  return con;
}

int callr__stdio_create(callr_handle_t *handle,
			   const char *std_out, const char *std_err,
			   BYTE** buffer_ptr, SEXP private,
			   const char *encoding) {
  BYTE* buffer;
  int count, i;
  int err;

  HANDLE pipe_handle[3] = { 0, 0, 0 };
  count = 3;

  buffer = malloc(CHILD_STDIO_SIZE(count));
  if (!buffer) { error("Out of memory"); }

  CHILD_STDIO_COUNT(buffer) = count;
  for (i = 0; i < count; i++) {
    CHILD_STDIO_CRT_FLAGS(buffer, i) = 0;
    CHILD_STDIO_HANDLE(buffer, i) = INVALID_HANDLE_VALUE;
  }

  for (i = 0; i < count; i++) {
    DWORD access = (i == 0) ? FILE_GENERIC_READ :
      FILE_GENERIC_WRITE | FILE_READ_ATTRIBUTES;
    const char *output = i == 0 ? 0 : (i == 1 ? std_out : std_err);

    handle->pipes[i] = 0;

    if (!output) {
      /* ignored output */
      err = callr__create_nul_handle(&CHILD_STDIO_HANDLE(buffer, i), access);
      if (err) { goto error; }
      CHILD_STDIO_CRT_FLAGS(buffer, i) = FOPEN | FDEV;

    } else if (strcmp("|", output)) {
      /* output to file */
      err = callr__create_output_handle(&CHILD_STDIO_HANDLE(buffer, i),
					   output, access);
      if (err) { goto error; }
      CHILD_STDIO_CRT_FLAGS(buffer, i) = FOPEN | FDEV;

    } else {
      /* piped output */
      callr_connection_t *con = 0;
      const char *r_pipe_name = i == 1 ? "stdout_pipe" : "stderr_pipe";
      GetRNGstate();
      err = callr__create_pipe(handle + (int)(unif_rand() * 65000),
				  &pipe_handle[i], &CHILD_STDIO_HANDLE(buffer, i));
      PutRNGstate();
      if (err) goto error;
      CHILD_STDIO_CRT_FLAGS(buffer, i) = FOPEN | FPIPE;
      con = callr__create_connection(pipe_handle[i], r_pipe_name,
					private, encoding);
      handle->pipes[i] = con;
    }
  }

  *buffer_ptr  = buffer;
  return 0;

 error:
  free(buffer);
  for (i = 0; i < count; i++) {
    if (pipe_handle[i]) CloseHandle(pipe_handle[i]);
    if (handle->pipes[i]) free(handle->pipes[i]);
  }
  return err;
}

void callr__stdio_destroy(BYTE* buffer) {
  int i, count;

  count = CHILD_STDIO_COUNT(buffer);
  for (i = 0; i < count; i++) {
    HANDLE handle = CHILD_STDIO_HANDLE(buffer, i);
    if (handle != INVALID_HANDLE_VALUE) {
      CloseHandle(handle);
    }
  }

  free(buffer);
}

WORD callr__stdio_size(BYTE* buffer) {
  return (WORD) CHILD_STDIO_SIZE(CHILD_STDIO_COUNT((buffer)));
}

HANDLE callr__stdio_handle(BYTE* buffer, int fd) {
  return CHILD_STDIO_HANDLE(buffer, fd);
}
