
#include "../callr.h"

callr__child_list_t child_list_head = { 0, 0, 0 };
callr__child_list_t *child_list = &child_list_head;
callr__child_list_t child_free_list_head = { 0, 0, 0 };
callr__child_list_t *child_free_list = &child_free_list_head;

void callr__freelist_add(callr__child_list_t *ptr) {
  ptr->next = child_free_list->next;
  child_free_list->next = ptr;
}

/* This is not a race condition with the SIGCHLD handler, because this
   function is only called with the handler blocked, from callr.c */

void callr__freelist_free() {
  callr__child_list_t *ptr = child_free_list->next;
  while (ptr) {
    callr__child_list_t *next = ptr->next;
    free(ptr);
    ptr = next;
  }
  child_free_list->next = 0;
}

int callr__child_add(pid_t pid, SEXP status) {
  callr__child_list_t *child = calloc(1, sizeof(callr__child_list_t));
  if (!child) return 1;
  child->pid = pid;
  child->status = status;
  child->next = child_list->next;
  child_list->next = child;
  return 0;
}

/* This is actually not used currently. But it should work fine. */
/* LCOV_EXCL_START */

void callr__child_remove(pid_t pid) {
  callr__child_list_t *prev = child_list, *ptr = child_list->next;
  while (ptr) {
    if (ptr->pid == pid) {
      prev->next = ptr->next;
      memset(ptr, 0, sizeof(*ptr));
      /* Defer freeing the memory, because malloc/free are typically not
	 reentrant, and if we free in the SIGCHLD handler, that can cause
	 crashes. The test case in test-run.R (see comments there)
	 typically brings this out. */
      callr__freelist_add(ptr);
      return;
    }
    prev = ptr;
    ptr = ptr->next;
  }
}

/* This is actually not used currently. But it should work fine. */

callr__child_list_t *callr__child_find(pid_t pid) {
  callr__child_list_t *ptr = child_list->next;
  while (ptr) {
    if (ptr->pid == pid) return ptr;
    ptr = ptr->next;
  }
  return 0;
}

/* LCOV_EXCL_STOP */

SEXP callr__killem_all() {
  callr__child_list_t *ptr = child_list->next;
  int killed = 0;

  callr__remove_sigchld();

  while (ptr) {
    callr__child_list_t *next = ptr->next;
    SEXP status = ptr->status;
    callr_handle_t *handle =
      (callr_handle_t*) R_ExternalPtrAddr(status);
    int wp, wstat;

    if (handle && handle->cleanup) {
      int ret = kill(ptr->pid, SIGKILL);
      do {
	wp = waitpid(ptr->pid, &wstat, 0);
      } while (wp == -1 && errno == EINTR);
      if (ret == 0) killed++;
    }

    /* waitpid errors are ignored here... */

    R_ClearExternalPtr(status);
    /* The handle will be freed in the finalizer, otherwise there is
       a race condition here. */

    free(ptr);

    ptr = next;
  }

  child_list->next = 0;
  callr__freelist_free();

  if (killed > 0) {
    REprintf("Unloading callr shared library, killed %d processes\n",
	     killed);
  }

  return R_NilValue;
}
