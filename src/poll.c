
#include "processx.h"

SEXP processx_poll(SEXP statuses, SEXP ms) {
  int cms = INTEGER(ms)[0];
  int i, num_proc = LENGTH(statuses);
  processx_pollable_t *pollables;
  SEXP result;

  pollables = (processx_pollable_t*)
    R_alloc(num_proc * 2, sizeof(processx_pollable_t));

  result = PROTECT(allocVector(VECSXP, num_proc));
  for (i = 0; i < num_proc; i++) {
    SEXP status = VECTOR_ELT(statuses, i);
    processx_handle_t *handle = R_ExternalPtrAddr(status);
    processx_c_pollable_from_connection(&pollables[i*2], handle->pipes[1]);
    processx_c_pollable_from_connection(&pollables[i*2+1], handle->pipes[2]);
    SET_VECTOR_ELT(result, i, allocVector(INTSXP, 2));
  }

  processx_c_connection_poll(pollables, num_proc * 2, cms);

  for (i = 0; i < num_proc; i++) {
    INTEGER(VECTOR_ELT(result, i))[0] = pollables[i*2].event;
    INTEGER(VECTOR_ELT(result, i))[1] = pollables[i*2+1].event;
  }

  UNPROTECT(1);
  return result;
}
