
#include <R_ext/Rdynload.h>
#include <R.h>

#include "callr.h"

void R_init_callr_win();
void R_init_callr_unix();
SEXP callr__killem_all();
SEXP run_testthat_tests();

static const R_CallMethodDef callMethods[]  = {
  { "callr_exec",               (DL_FUNC) &callr_exec,               9 },
  { "callr_wait",               (DL_FUNC) &callr_wait,               2 },
  { "callr_is_alive",           (DL_FUNC) &callr_is_alive,           1 },
  { "callr_get_exit_status",    (DL_FUNC) &callr_get_exit_status,    1 },
  { "callr_signal",             (DL_FUNC) &callr_signal,             2 },
  { "callr_kill",               (DL_FUNC) &callr_kill,               2 },
  { "callr_get_pid",            (DL_FUNC) &callr_get_pid,            1 },
  { "callr_poll",               (DL_FUNC) &callr_poll,               2 },
  { "callr__process_exists",    (DL_FUNC) &callr__process_exists,    1 },
  { "callr__killem_all",        (DL_FUNC) &callr__killem_all,        0 },
  { "callr_is_named_pipe_open", (DL_FUNC) &callr_is_named_pipe_open, 1 },
  { "callr_close_named_pipe",   (DL_FUNC) &callr_close_named_pipe,   1 },
  { "callr_create_named_pipe",  (DL_FUNC) &callr_create_named_pipe,  2 },
  { "callr_write_named_pipe",   (DL_FUNC) &callr_write_named_pipe,   2 },
  { "callr__disconnect_process_handle",
    (DL_FUNC) &callr__disconnect_process_handle, 1 },

  { "callr_connection_create",     (DL_FUNC) &callr_connection_create,     2 },
  { "callr_connection_read_chars", (DL_FUNC) &callr_connection_read_chars, 2 },
  { "callr_connection_read_lines", (DL_FUNC) &callr_connection_read_lines, 2 },
  { "callr_connection_is_eof",     (DL_FUNC) &callr_connection_is_eof,     1 },
  { "callr_connection_close",      (DL_FUNC) &callr_connection_close,      1 },
  { "callr_connection_poll",       (DL_FUNC) &callr_connection_poll,       2 },

  { "run_testthat_tests", (DL_FUNC) &run_testthat_tests, 0 },

  { NULL, NULL, 0 }
};

void R_init_callr(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
#ifdef _WIN32
  R_init_callr_win();
#else
  R_init_callr_unix();
#endif
}
