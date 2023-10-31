#include <dlfcn.h>
#include <pthread.h>

#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

#ifdef __linux__

void linux_init(void);

void *xdlsym(void* handle, const char* symbol);
void *xdlopen(const char *filename, int flags);

SEXP rdlsym(SEXP lib, SEXP name, SEXP recurse) {
  void *clib = R_ExternalPtrAddr(lib);
  const char *cname = CHAR(STRING_ELT(name, 0));
  void *sym = xdlsym(clib, cname);
  if (!sym) error("Cannot find dynamic symbol %s", cname);
  SEXP xptr = R_MakeExternalPtr(sym, R_NilValue, R_NilValue);
  return xptr;
}

SEXP rdlopen(SEXP path) {
  const char *cpath = CHAR(STRING_ELT(path, 0));
  void *ret = xdlopen(cpath, RTLD_NOW | RTLD_LOCAL);
  if (!ret) error("Cannot load shared library %s", cpath);
  SEXP xptr = R_MakeExternalPtr(ret, R_NilValue, R_NilValue);
  return xptr;
}

struct r_interp {
  const char *path;
  void *lib;
};

void *thr_load_r(void *arg) {
  struct r_interp * R = (struct r_interp*) arg;
  // initialize thread local data
  linux_init();
  R->lib = xdlopen(R->path, RTLD_NOW);
  return 0;
}

SEXP load_r(SEXP path) {
  const char *cpath = CHAR(STRING_ELT(path, 0));
  pthread_t *thread = malloc(sizeof(pthread_t));
  struct r_interp R = { cpath, NULL };
  pthread_create(thread, NULL, thr_load_r, &R);
  pthread_join(*thread, NULL);
  return R_NilValue;
}

static const R_CallMethodDef callMethods[]  = {
  { "rdlsym",    (DL_FUNC) rdlsym,    3 },
  { "rdlopen",   (DL_FUNC) rdlopen,   1 },
  { "load_r",    (DL_FUNC) load_r,    1 },
  { NULL, NULL, 0 }
};

#endif


#ifdef _WIN32

static const R_CallMethodDef callMethods[]  = {
  { NULL, NULL, 0 }
};

#endif

#ifdef __MACH__

void *c_dlsym(
  void *lib,
  const char *name,
  int add_underscore,
  int recurse
);

void *c_dlopen(
  const char *file,
  int flags
);

SEXP rdlopen(SEXP path) {
  const char *cpath = CHAR(STRING_ELT(path, 0));
  void *ret = c_dlopen(cpath, RTLD_NOW | RTLD_LOCAL);
  if (!ret) error("Cannot load shared library %s", cpath);
  SEXP xptr = R_MakeExternalPtr(ret, R_NilValue, R_NilValue);
  return xptr;
}

SEXP rdlsym(SEXP lib, SEXP name, SEXP recurse) {
  void *clib = R_ExternalPtrAddr(lib);
  const char *cname = CHAR(STRING_ELT(name, 0));
  int crecurse = LOGICAL(recurse)[0];
  void *sym = c_dlsym(clib, cname, 1, crecurse);
  if (!sym) error("Cannot find dynamic symbol %s", cname);
  SEXP xptr = R_MakeExternalPtr(sym, R_NilValue, R_NilValue);
  return xptr;
}

SEXP load_r(SEXP path) {
  const char *cpath = CHAR(STRING_ELT(path, 0));
  void *ret = c_dlopen(cpath, RTLD_NOW);
  if (!ret) error("Cannot load R shared library");
  SEXP xptr = R_MakeExternalPtr(ret, R_NilValue, R_NilValue);
  return xptr;
}

typedef int (*r_init_t)(int argc, char *argv[]);
typedef SEXP (*r_parse_eval_string_t)(const char *, SEXP);
typedef int  (*r_read_console_t)(const char *, unsigned char *, int, int);
typedef void (*r_repl_dll_init_t)(void);

int rem_read_console(const char *prompt, unsigned char *buf, int buflen, int hist) {
  return 0;
}

SEXP init_r(SEXP R) {
  void *CR = R_ExternalPtrAddr(R);

  char *argv2[]= {
    "R",
    "-q",
    "--vanilla",
    "--gui=none",
    "--no-restore",
    "--no-save",
    "--no-readline"
  };

  setenv("R_DEFAULT_PACKAGES", "NULL", 1);

  int *sign = c_dlsym(CR, "R_SignalHandlers", 1, 1);
  *sign = 0;

  r_init_t init = (r_init_t) c_dlsym(CR, "Rf_initEmbeddedR", 1, 1);
  if (!init) error("Cannot find Rf_initEmbeddedR");
  init(sizeof(argv2) / sizeof(argv2[0]), argv2);

  r_repl_dll_init_t repl_init = (r_repl_dll_init_t) c_dlsym(CR, "R_ReplDLLinit", 1, 1);
  if (!repl_init) error("Cannot find R_ReplDllInit");
  repl_init();

  FILE** outp = c_dlsym(CR, "R_Outputfile", 1, 1);
  if (!outp) error("Cannot find R_Outputfile");
  *outp = NULL;
  FILE** cons = c_dlsym(CR, "R_Consolefile", 1, 1);
  if (!cons) error("Cannot find R_Consolefile");
  *cons = NULL;
  r_read_console_t* read_console = c_dlsym(CR, "ptr_R_ReadConsole", 1, 1);
  if (!read_console) error("Cannot find ptr_R_ReadConsole");
  *read_console = rem_read_console;

  return R_NilValue;
}

SEXP eval_r(SEXP R, SEXP expr) {
  void *CR = R_ExternalPtrAddr(R);
  r_parse_eval_string_t parse_eval_string =
    (r_parse_eval_string_t) c_dlsym(CR, "R_ParseEvalString", 1, 1);
  if (!parse_eval_string) error("Cannot find R_ParseEvalString");
  SEXP *glenv = c_dlsym(CR, "R_GlobalEnv", 1, 1);
  if (!glenv) error("Cannot find R_GlovalEnv");
  parse_eval_string(CHAR(STRING_ELT(expr, 0)), *glenv);
  return R_NilValue;
}

static const R_CallMethodDef callMethods[]  = {
  { "rdlopen",   (DL_FUNC) rdlopen,   1 },
  { "rdlsym",    (DL_FUNC) rdlsym,    3 },
  { "load_r",    (DL_FUNC) load_r,    1 },
  { "init_r",    (DL_FUNC) init_r,    1 },
  { "eval_r",    (DL_FUNC) eval_r,    2 },
  { NULL, NULL, 0 }
};

#endif

void R_init_callr(DllInfo *dll) {
#ifdef __linux__
  linux_init();
#endif
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
