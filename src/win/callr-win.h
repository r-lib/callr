
#ifndef R_CALLR_WIN_H
#define R_CALLR_WIN_H

#include <windows.h>

typedef struct callr_handle_s {
  int exitcode;
  int collected;	 /* Whether exit code was collected already */
  HANDLE hProcess;
  DWORD  dwProcessId;
  BYTE *child_stdio_buffer;
  HANDLE waitObject;
  callr_connection_t *pipes[3];
  int cleanup;
} callr_handle_t;

extern HANDLE callr__iocp;

int callr__utf8_to_utf16_alloc(const char* s, WCHAR** ws_ptr);

int callr__stdio_create(callr_handle_t *handle,
			   const char *std_out, const char *std_err,
			   BYTE** buffer_ptr, SEXP privatex,
			   const char *encoding);
WORD callr__stdio_size(BYTE* buffer);
HANDLE callr__stdio_handle(BYTE* buffer, int fd);
void callr__stdio_destroy(BYTE* buffer);

void callr__handle_destroy(callr_handle_t *handle);

void callr__cleanup_child_tree(DWORD pid);

#define CALLR_ERROR(m,c) callr__error((m),(c),__FILE__,__LINE__)
void callr__error(const char *message, DWORD errorcode, const char *file, int line);

#endif
