
#ifndef CALLR_TYPES_H
#define CALLR_TYPES_H

#ifdef _WIN32
#include <windows.h>
typedef DWORD pid_t;
#else
#include <signal.h>
#endif

typedef struct {
  pid_t *stor_begin;
  pid_t *stor_end;
  pid_t *end;
} callr_vector_t;

#define VECTOR(v) ((v).stor_begin)

void callr_vector_init(callr_vector_t *v, size_t size, size_t alloc_size);
size_t callr_vector_size(const callr_vector_t *v);
void callr_vector_reserve(callr_vector_t *v, size_t size);
void callr_vector_clear(callr_vector_t *v);
void callr_vector_push_back(callr_vector_t *v, pid_t e);
int callr_vector_find(const callr_vector_t *v, pid_t e, size_t from, size_t *idx);
void callr_vector_rooted_tree(pid_t root, const callr_vector_t *nodes,
				 const callr_vector_t *parents,
				 callr_vector_t *result);

#endif
