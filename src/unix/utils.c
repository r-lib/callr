
#include "../callr.h"

char *callr__tmp_string(SEXP str, int i) {
  const char *ptr = CHAR(STRING_ELT(str, i));
  char *cstr = R_alloc(1, strlen(ptr) + 1);
  strcpy(cstr, ptr);
  return cstr;
}

char **callr__tmp_character(SEXP chr) {
  size_t i, n = LENGTH(chr);
  char **cchr = (void*) R_alloc(n + 1, sizeof(char*));
  for (i = 0; i < n; i++) {
    cchr[i] = callr__tmp_string(chr, i);
  }
  cchr[n] = 0;
  return cchr;
}

int callr__nonblock_fcntl(int fd, int set) {
  int flags;
  int r;

  do { r = fcntl(fd, F_GETFL); } while (r == -1 && errno == EINTR);
  if (r == -1) { return -errno; }

  /* Bail out now if already set/clear. */
  if (!!(r & O_NONBLOCK) == !!set) { return 0; }

  if (set) { flags = r | O_NONBLOCK; } else { flags = r & ~O_NONBLOCK; }

  do { r = fcntl(fd, F_SETFL, flags); } while (r == -1 && errno == EINTR);
  if (r) { return -errno; }

  return 0;
}

int callr__cloexec_fcntl(int fd, int set) {
  int flags;
  int r;

  do { r = fcntl(fd, F_GETFD); } while (r == -1 && errno == EINTR);
  if (r == -1) { return -errno; }

  /* Bail out now if already set/clear. */
  if (!!(r & FD_CLOEXEC) == !!set) { return 0; }

  if (set) { flags = r | FD_CLOEXEC; } else { flags = r & ~FD_CLOEXEC; }

  do { r = fcntl(fd, F_SETFD, flags); } while (r == -1 && errno == EINTR);
  if (r) { return -errno; }

  return 0;
}
