
#include "pmload.hpp"

extern "C" {

void *c_dlsym(
  void *lib,
  const char *name,
  int add_underscore,
  int recurse) {

  return pmload::my_dlsym(lib, name, add_underscore, recurse);
}

void *c_dlopen(
  const char *file,
  int flags) {

  return pmload::my_dlopen(file, flags);
}

} /* extern C */
