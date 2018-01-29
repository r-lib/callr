
#include "callr.h"

SEXP callr_poll(SEXP statuses, SEXP ms) {
  int cms = INTEGER(ms)[0];
  int i, num_proc = LENGTH(statuses);
  callr_pollable_t *pollables;
  SEXP result;

  pollables = (callr_pollable_t*)
    R_alloc(num_proc * 2, sizeof(callr_pollable_t));

  result = PROTECT(allocVector(VECSXP, num_proc));
  for (i = 0; i < num_proc; i++) {
    SEXP status = VECTOR_ELT(statuses, i);
    callr_handle_t *handle = R_ExternalPtrAddr(status);
    callr_c_pollable_from_connection(&pollables[i*2], handle->pipes[1]);
    callr_c_pollable_from_connection(&pollables[i*2+1], handle->pipes[2]);
    SET_VECTOR_ELT(result, i, allocVector(INTSXP, 2));
  }

  callr_c_connection_poll(pollables, num_proc * 2, cms);

  for (i = 0; i < num_proc; i++) {
    INTEGER(VECTOR_ELT(result, i))[0] = pollables[i*2].event;
    INTEGER(VECTOR_ELT(result, i))[1] = pollables[i*2+1].event;
  }

  UNPROTECT(1);
  return result;
}
