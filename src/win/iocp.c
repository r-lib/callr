
#include "../callr.h"

HANDLE callr__connection_iocp = NULL;

HANDLE callr__get_default_iocp() {

  if (! callr__connection_iocp) {
    callr__connection_iocp = CreateIoCompletionPort(
    /* FileHandle = */                 INVALID_HANDLE_VALUE,
    /* ExistingCompletionPort = */     NULL,
    /* CompletionKey = */              0,
    /* NumberOfConcurrentThreads =  */ 0);

    if (! callr__connection_iocp) {
      CALLR_ERROR("cannot create default IOCP", GetLastError());
    }
  }

  return callr__connection_iocp;
}
