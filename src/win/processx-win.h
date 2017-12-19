
#ifndef R_PROCESSX_WIN_H
#define R_PROCESSX_WIN_H

#include <windows.h>

typedef struct processx_handle_s {
  int exitcode;
  int collected;	 /* Whether exit code was collected already */
  HANDLE hProcess;
  DWORD  dwProcessId;
  BYTE *child_stdio_buffer;
  HANDLE waitObject;
  processx_connection_t *pipes[3];
  int cleanup;
} processx_handle_t;

extern HANDLE processx__iocp;

int processx__utf8_to_utf16_alloc(const char* s, WCHAR** ws_ptr);

int processx__stdio_create(processx_handle_t *handle,
			   const char *std_out, const char *std_err,
			   BYTE** buffer_ptr, SEXP privatex,
			   const char *encoding);
WORD processx__stdio_size(BYTE* buffer);
HANDLE processx__stdio_handle(BYTE* buffer, int fd);
void processx__stdio_destroy(BYTE* buffer);

void processx__handle_destroy(processx_handle_t *handle);

void processx__cleanup_child_tree(DWORD pid);

#define PROCESSX_ERROR(m,c) processx__error((m),(c),__FILE__,__LINE__)
void processx__error(const char *message, DWORD errorcode, const char *file, int line);

#endif
