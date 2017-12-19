// On non-windows platforms, we still need the C interfaces, but they simply
// give errors.

#ifndef _WIN32

#include <Rdefines.h>

SEXP callr_is_named_pipe_open(SEXP pipe_ext) {
    error("callr_is_named_pipe_open only valid on Windows.");
    return R_NilValue;
}

SEXP callr_close_named_pipe(SEXP pipe_ext) {
    error("callr_close_named_pipe only valid on Windows.");
    return R_NilValue;
}

SEXP callr_create_named_pipe(SEXP name, SEXP mode) {
    error("callr_create_named_pipe only valid on Windows.");
    return R_NilValue;
}

SEXP callr_write_named_pipe(SEXP pipe_ext, SEXP text) {
    error("callr_write_named_pipe only valid on Windows.");
    return R_NilValue;
}

#endif
