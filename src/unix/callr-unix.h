
#ifndef R_CALLR_UNIX_H
#define R_CALLR_UNIX_H

typedef struct callr_handle_s {
  int exitcode;
  int collected;	 /* Whether exit code was collected already */
  pid_t pid;
  int fd0;			/* writeable */
  int fd1;			/* readable */
  int fd2;			/* readable */
  int waitpipe[2];		/* use it for wait() with timeout */
  int cleanup;
  callr_connection_t *pipes[3];
} callr_handle_t;

char *callr__tmp_string(SEXP str, int i);
char **callr__tmp_character(SEXP chr);

void callr__sigchld_callback(int sig, siginfo_t *info, void *ctx);
void callr__setup_sigchld();
void callr__remove_sigchld();
void callr__block_sigchld();
void callr__unblock_sigchld();

void callr__finalizer(SEXP status);
SEXP callr__killem_all();

/* Child list and its functions */

typedef struct callr__child_list_s {
  pid_t pid;
  SEXP status;
  struct callr__child_list_s *next;
} callr__child_list_t;

int callr__child_add(pid_t pid, SEXP status);
void callr__child_remove(pid_t pid);
callr__child_list_t *callr__child_find(pid_t pid);
void callr__freelist_add(callr__child_list_t *ptr);
void callr__freelist_free();

void callr__collect_exit_status(SEXP status, int retval, int wstat);

int callr__nonblock_fcntl(int fd, int set);
int callr__cloexec_fcntl(int fd, int set);

/* Control connections*/

void callr__create_control_read(callr_handle_t *handle,
				   int fd, const char *membername,
				   SEXP privatex);
void callr__create_control_write(callr_handle_t *handle,
				    int fd, const char *membername,
				    SEXP privatex);

/* Interruptible system calls */

int callr__interruptible_poll(struct pollfd fds[],
				 nfds_t nfds, int timeout);

#endif
