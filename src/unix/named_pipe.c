// On non-windows platforms, we still need the C interfaces, but they simply
// give errors.

#ifndef _WIN32

#include <Rdefines.h>

SEXP processx_is_named_pipe_open(SEXP pipe_ext) {
    error("processx_is_named_pipe_open only valid on Windows.");
    return R_NilValue;
}

SEXP processx_close_named_pipe(SEXP pipe_ext) {
    error("processx_close_named_pipe only valid on Windows.");
    return R_NilValue;
}

SEXP processx_create_named_pipe(SEXP name, SEXP mode) {
    error("processx_create_named_pipe only valid on Windows.");
    return R_NilValue;
}

SEXP processx_write_named_pipe(SEXP pipe_ext, SEXP text) {
    error("processx_write_named_pipe only valid on Windows.");
    return R_NilValue;
}

#endif
