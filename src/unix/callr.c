
#ifndef _WIN32

#include "../callr.h"

/* Internals */

static void callr__child_init(callr_handle_t *handle, int pipes[3][2],
				 char *command, char **args, int error_fd,
				 const char *std_out, const char *std_err,
				 callr_options_t *options);

static SEXP callr__make_handle(SEXP private, int cleanup);
static void callr__handle_destroy(callr_handle_t *handle);
void callr__create_connections(callr_handle_t *handle, SEXP private,
				  const char *encoding);

/* Define BSWAP_32 on Big Endian systems */
#ifdef WORDS_BIGENDIAN
#if (defined(__sun) && defined(__SVR4))
#include <sys/byteorder.h>
#elif (defined(__APPLE__) && defined(__ppc__) || defined(__ppc64__))
#include <libkern/OSByteOrder.h>
#define BSWAP_32 OSSwapInt32
#elif (defined(__OpenBSD__))
#define BSWAP_32(x) swap32(x)
#elif (defined(__GLIBC__))
#include <byteswap.h>
#define BSWAP_32(x) bswap_32(x)
#endif
#endif

extern callr__child_list_t child_list_head;
extern callr__child_list_t *child_list;
extern callr__child_list_t child_free_list_head;
extern callr__child_list_t *child_free_list;

/* We are trying to make sure that the variables in the library are
   properly set to their initial values after a library (re)load.
   This function is called from `R_init_callr`. */

void R_init_callr_unix() {
  child_list_head.pid = 0;
  child_list_head.status = 0;
  child_list_head.next = 0;
  child_list = &child_list_head;

  child_free_list_head.pid = 0;
  child_free_list_head.status = 0;
  child_free_list_head.next = 0;
  child_free_list = &child_free_list_head;
}

/* These run in the child process, so no coverage here. */
/* LCOV_EXCL_START */

void callr__write_int(int fd, int err) {
  int dummy = write(fd, &err, sizeof(int));
  (void) dummy;
}

static void callr__child_init(callr_handle_t* handle, int pipes[3][2],
				 char *command, char **args, int error_fd,
				 const char *std_out, const char *std_err,
				 callr_options_t *options) {

  int fd0, fd1, fd2;
  int i;

  setsid();

  /* The dup2 calls make sure that stdin, stdout and stderr use file
     descriptors 0, 1 and 3 respectively. */

  /* stdin is coming from /dev/null */

  fd0 = open("/dev/null", O_RDONLY);
  if (fd0 == -1) { callr__write_int(error_fd, - errno); raise(SIGKILL); }

  if (fd0 != 0) fd0 = dup2(fd0, 0);
  if (fd0 == -1) { callr__write_int(error_fd, - errno); raise(SIGKILL); }

  /* stdout is going into file or a pipe */

  if (!std_out) {
    fd1 = open("/dev/null", O_RDWR);
  } else if (!strcmp(std_out, "|")) {
    fd1 = pipes[1][1];
    close(pipes[1][0]);
  } else {
    fd1 = open(std_out, O_CREAT | O_TRUNC| O_RDWR, 0644);
  }
  if (fd1 == -1) { callr__write_int(error_fd, - errno); raise(SIGKILL); }

  if (fd1 != 1) fd1 = dup2(fd1, 1);
  if (fd1 == -1) { callr__write_int(error_fd, - errno); raise(SIGKILL); }

  /* stderr, to file or a pipe */

  if (!std_err) {
    fd2 = open("/dev/null", O_RDWR);
  } else if (!strcmp(std_err, "|")) {
    fd2 = pipes[2][1];
    close(pipes[2][0]);
  } else {
    fd2 = open(std_err, O_CREAT | O_TRUNC| O_RDWR, 0644);
  }
  if (fd2 == -1) { callr__write_int(error_fd, - errno); raise(SIGKILL); }

  if (fd2 != 2) fd2 = dup2(fd2, 2);
  if (fd2 == -1) { callr__write_int(error_fd, - errno); raise(SIGKILL); }

  callr__nonblock_fcntl(fd0, 0);
  callr__nonblock_fcntl(fd1, 0);
  callr__nonblock_fcntl(fd2, 0);

  for (i = 3; i < error_fd; i++) {
    close(i);
  }
  for (i = error_fd + 1; ; i++) {
    if (-1 == close(i) && i > 200) break;
  }

  execvp(command, args);
  callr__write_int(error_fd, - errno);
  raise(SIGKILL);
}

/* LCOV_EXCL_STOP */

SEXP callr__disconnect_process_handle(SEXP status) {
  R_SetExternalPtrTag(status, R_NilValue);
  return R_NilValue;
}

void callr__finalizer(SEXP status) {
  callr_handle_t *handle = (callr_handle_t*) R_ExternalPtrAddr(status);
  pid_t pid;
  int wp, wstat;
  SEXP private;

  callr__block_sigchld();

  /* Free child list nodes that are not needed any more. */
  callr__freelist_free();

  /* Already freed? */
  if (!handle) goto cleanup;

  pid = handle->pid;

  if (handle->cleanup) {
    /* Do a non-blocking waitpid() to see if it is running */
    do {
      wp = waitpid(pid, &wstat, WNOHANG);
    } while (wp == -1 && errno == EINTR);

    /* Maybe just waited on it? Then collect status */
    if (wp == pid) callr__collect_exit_status(status, wstat);

    /* If it is running, we need to kill it, and wait for the exit status */
    if (wp == 0) {
      kill(-pid, SIGKILL);
      do {
	wp = waitpid(pid, &wstat, 0);
      } while (wp == -1 && errno == EINTR);
      callr__collect_exit_status(status, wstat);
    }
  }

  /* Copy over pid and exit status */
  private = PROTECT(R_ExternalPtrTag(status));
  if (!isNull(private)) {
    SEXP sone = PROTECT(ScalarLogical(1));
    SEXP spid = PROTECT(ScalarInteger(pid));
    SEXP sexitcode = PROTECT(ScalarInteger(handle->exitcode));

    defineVar(install("exited"), sone, private);
    defineVar(install("pid"), spid, private);
    defineVar(install("exitcode"), sexitcode, private);
    UNPROTECT(3);
  }

  UNPROTECT(1);

  /* Note: if no cleanup is requested, then we still have a sigchld
     handler, to read out the exit code via waitpid, but no handle
     any more. */

  /* Deallocate memory */
  R_ClearExternalPtr(status);
  callr__handle_destroy(handle);

 cleanup:
  callr__unblock_sigchld();
}

static SEXP callr__make_handle(SEXP private, int cleanup) {
  callr_handle_t * handle;
  SEXP result;

  handle = (callr_handle_t*) malloc(sizeof(callr_handle_t));
  if (!handle) { error("Out of memory"); }
  memset(handle, 0, sizeof(callr_handle_t));
  handle->waitpipe[0] = handle->waitpipe[1] = -1;

  result = PROTECT(R_MakeExternalPtr(handle, private, R_NilValue));
  R_RegisterCFinalizerEx(result, callr__finalizer, 1);
  handle->cleanup = cleanup;

  UNPROTECT(1);
  return result;
}

static void callr__handle_destroy(callr_handle_t *handle) {
  if (!handle) return;
  free(handle);
}

void callr__make_socketpair(int pipe[2]) {
#if defined(__linux__)
  static int no_cloexec;
  if (no_cloexec)  goto skip;

  if (socketpair(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0, pipe) == 0)
    return;

  /* Retry on EINVAL, it means SOCK_CLOEXEC is not supported.
   * Anything else is a genuine error.
   */
  if (errno != EINVAL) {
    error("callr socketpair: %s", strerror(errno)); /* LCOV_EXCL_LINE */
  }

  no_cloexec = 1;

skip:
#endif

  if (socketpair(AF_UNIX, SOCK_STREAM, 0, pipe)) {
    error("callr socketpair: %s", strerror(errno));
  }

  callr__cloexec_fcntl(pipe[0], 1);
  callr__cloexec_fcntl(pipe[1], 1);
}

SEXP callr_exec(SEXP command, SEXP args, SEXP std_out, SEXP std_err,
		   SEXP windows_verbatim_args,
		   SEXP windows_hide_window, SEXP private, SEXP cleanup,
		   SEXP encoding) {

  char *ccommand = callr__tmp_string(command, 0);
  char **cargs = callr__tmp_character(args);
  int ccleanup = INTEGER(cleanup)[0];
  const char *cstdout = isNull(std_out) ? 0 : CHAR(STRING_ELT(std_out, 0));
  const char *cstderr = isNull(std_err) ? 0 : CHAR(STRING_ELT(std_err, 0));
  const char *cencoding = CHAR(STRING_ELT(encoding, 0));
  callr_options_t options = { 0 };

  pid_t pid;
  int err, exec_errorno = 0, status;
  ssize_t r;
  int signal_pipe[2] = { -1, -1 };
  int pipes[3][2] = { { -1, -1 }, { -1, -1 }, { -1, -1 } };

  callr_handle_t *handle = NULL;
  SEXP result;

  if (pipe(signal_pipe)) { goto cleanup; }
  callr__cloexec_fcntl(signal_pipe[0], 1);
  callr__cloexec_fcntl(signal_pipe[1], 1);

  callr__setup_sigchld();

  result = PROTECT(callr__make_handle(private, ccleanup));
  handle = R_ExternalPtrAddr(result);

  /* Create pipes, if requested. TODO: stdin */
  if (cstdout && !strcmp(cstdout, "|")) callr__make_socketpair(pipes[1]);
  if (cstderr && !strcmp(cstderr, "|")) callr__make_socketpair(pipes[2]);

  callr__block_sigchld();

  pid = fork();

  /* TODO: how could we test a failure? */
  if (pid == -1) {		/* ERROR */
    err = -errno;
    if (signal_pipe[0] >= 0) close(signal_pipe[0]);
    if (signal_pipe[1] >= 0) close(signal_pipe[1]);
    callr__unblock_sigchld();
    goto cleanup;
  }

  /* CHILD */
  if (pid == 0) {
    /* LCOV_EXCL_START */
    callr__child_init(handle, pipes, ccommand, cargs, signal_pipe[1],
			 cstdout, cstderr, &options);
    goto cleanup;
    /* LCOV_EXCL_STOP */
  }

  /* We need to know the callr children */
  if (callr__child_add(pid, result)) {
    err = -errno;
    if (signal_pipe[0] >= 0) close(signal_pipe[0]);
    if (signal_pipe[1] >= 0) close(signal_pipe[1]);
    callr__unblock_sigchld();
    goto cleanup;
  }

  /* SIGCHLD can arrive now */
  callr__unblock_sigchld();

  if (signal_pipe[1] >= 0) close(signal_pipe[1]);

  do {
    r = read(signal_pipe[0], &exec_errorno, sizeof(exec_errorno));
  } while (r == -1 && errno == EINTR);

  if (r == 0) {
    ; /* okay, EOF */
  } else if (r == sizeof(exec_errorno)) {
    do {
      err = waitpid(pid, &status, 0); /* okay, read errorno */
    } while (err == -1 && errno == EINTR);

  } else if (r == -1 && errno == EPIPE) {
    do {
      err = waitpid(pid, &status, 0); /* okay, got EPIPE */
    } while (err == -1 && errno == EINTR);

  } else {
    goto cleanup;
  }

  if (signal_pipe[0] >= 0) close(signal_pipe[0]);

  /* Set fds for standard I/O */
  /* TODO: implement stdin */
  handle->fd0 = handle->fd1 = handle->fd2 = -1;
  if (pipes[1][0] >= 0) {
    handle->fd1 = pipes[1][0];
    callr__nonblock_fcntl(handle->fd1, 1);
  }
  if (pipes[2][0] >= 0) {
    handle->fd2 = pipes[2][0];
    callr__nonblock_fcntl(handle->fd2, 1);
  }

  /* Closed unused ends of pipes */
  if (pipes[1][1] >= 0) close(pipes[1][1]);
  if (pipes[2][1] >= 0) close(pipes[2][1]);

  /* Create proper connections */
  callr__create_connections(handle, private, cencoding);

  if (exec_errorno == 0) {
    handle->pid = pid;
    UNPROTECT(1);		/* result */
    return result;
  }

 cleanup:
  error("callr error");
}

void callr__collect_exit_status(SEXP status, int wstat) {
  callr_handle_t *handle = R_ExternalPtrAddr(status);

  /* This must be called from a function that blocks SIGCHLD.
     So we are not blocking it here. */

  if (!handle) {
    error("Invalid handle, already finalized");
  }

  if (handle->collected) { return; }

  /* We assume that errors were handled before */
  if (WIFEXITED(wstat)) {
    handle->exitcode = WEXITSTATUS(wstat);
  } else {
    handle->exitcode = - WTERMSIG(wstat);
  }

  handle->collected = 1;
}

/* In general we need to worry about three asynchronous processes here:
 * 1. The main code, i.e. the code in this function.
 * 2. The finalizer, that can be triggered by any R function.
 *    A good strategy is to avoid calling R functions here completely.
 *    Functions that return immediately, like `R_CheckUserInterrupt`, or
 *    a `ScalarLogical` that we return, are fine.
 * 3. The SIGCHLD handler that we just block at the beginning, but it can
 *    still be called between the R function doing the `.Call` to us, and
 *    the signal blocking call.
 *
 * Keeping these in mind, we do this:
 *
 * 1. If the exit status was copied over to R already, we return
 *    immediately from R. Otherwise this C function is called.
 * 2. We block SIGCHLD.
 * 3. If we already collected the exit status, then this process has
 *    finished, so we don't need to wait.
 * 4. We set up a self-pipe that we can poll. The pipe will be closed in
 *    the SIGCHLD signal handler, and that triggers the poll event.
 * 5. We unblock the SIGCHLD handler, so that it can trigger the pipe event.
 * 6. We start polling. We poll in small time chunks, to keep the wait still
 *    interruptible.
 * 7. We keep polling until the timeout expires or the process finishes.
 */

SEXP callr_wait(SEXP status, SEXP timeout) {
  callr_handle_t *handle = R_ExternalPtrAddr(status);
  int ctimeout = INTEGER(timeout)[0], timeleft = ctimeout;
  struct pollfd fd;
  int ret = 0;
  pid_t pid;

  callr__block_sigchld();

  if (!handle) {
    callr__unblock_sigchld();
    error("Internal callr error, handle already removed");
  }

  pid = handle->pid;

  /* If we already have the status, then return now. */
  if (handle->collected) {
    callr__unblock_sigchld();
    return ScalarLogical(1);
  }

  /* Make sure this is active, in case another package replaced it... */
  callr__setup_sigchld();
  callr__block_sigchld();

  /* Setup the self-pipe that we can poll */
  if (pipe(handle->waitpipe)) {
    callr__unblock_sigchld();
    error("callr error: %s", strerror(errno));
  }
  callr__nonblock_fcntl(handle->waitpipe[0], 1);
  callr__nonblock_fcntl(handle->waitpipe[1], 1);

  /* Poll on the pipe, need to unblock sigchld before */
  fd.fd = handle->waitpipe[0];
  fd.events = POLLIN;
  fd.revents = 0;

  callr__unblock_sigchld();

  while (ctimeout < 0 || timeleft > CALLR_INTERRUPT_INTERVAL) {
    do {
      ret = poll(&fd, 1, CALLR_INTERRUPT_INTERVAL);
    } while (ret == -1 && errno == EINTR);

    /* If not a timeout, then we are done */
    if (ret != 0) break;

    R_CheckUserInterrupt();

    /* We also check if the process is alive, because the SIGCHLD is
       not delivered in valgrind :( */
    ret = kill(pid, 0);
    if (ret != 0) return ScalarLogical(1);

    if (ctimeout >= 0) timeleft -= CALLR_INTERRUPT_INTERVAL;
  }

  /* Maybe we are not done, and there is a little left from the timeout */
  if (ret == 0 && timeleft >= 0) {
    do {
      ret = poll(&fd, 1, timeleft);
    } while (ret == -1 && errno == EINTR);
  }

  if (ret == -1) {
    error("callr wait with timeout error: %s", strerror(errno));
  }

  if (handle->waitpipe[0] >= 0) close(handle->waitpipe[0]);
  if (handle->waitpipe[1] >= 0) close(handle->waitpipe[1]);
  handle->waitpipe[0] = -1;

  return ScalarLogical(ret != 0);
}

/* This is similar to `callr_wait`, but a bit simpler, because we
 * don't need to wait and poll. The same restrictions listed there, also
 * apply here.
 *
 * 1. If the exit status was copied over to R already, we return
 *    immediately from R. Otherwise this C function is called.
 * 2. We block SIGCHLD.
 * 3. If we already collected the exit status, then this process has
 *    finished, and we return FALSE.
 * 4. Otherwise we do a non-blocking `waitpid`, because the process might
 *    have finished, we just haven't collected its exit status yet.
 * 5. If the process is still running, `waitpid` returns 0. We return TRUE.
 * 6. Otherwise we collect the exit status, and return FALSE.
 */

SEXP callr_is_alive(SEXP status) {
  callr_handle_t *handle = R_ExternalPtrAddr(status);
  pid_t pid;
  int wstat, wp;
  int ret = 0;

  callr__block_sigchld();

  if (!handle) {
    callr__unblock_sigchld();
    error("Internal callr error, handle already removed");
  }

  if (handle->collected) goto cleanup;

  /* Otherwise a non-blocking waitpid to collect zombies */
  pid = handle->pid;
  do {
    wp = waitpid(pid, &wstat, WNOHANG);
  } while (wp == -1 && errno == EINTR);

  /* Some other error? */
  if (wp == -1) {
    callr__unblock_sigchld();
    error("callr_is_alive: %s", strerror(errno));
  }

  /* If running, return TRUE, otherwise collect exit status, return FALSE */
  if (wp == 0) {
    ret = 1;
  } else {
    callr__collect_exit_status(status, wstat);
  }

 cleanup:
  callr__unblock_sigchld();
  return ScalarLogical(ret);
}

/* This is essentially the same as `callr_is_alive`, but we return an
 * exit status if the process has already finished. See above.
 */

SEXP callr_get_exit_status(SEXP status) {
  callr_handle_t *handle = R_ExternalPtrAddr(status);
  pid_t pid;
  int wstat, wp;
  SEXP result;

  callr__block_sigchld();

  if (!handle) {
    callr__unblock_sigchld();
    error("Internal callr error, handle already removed");
  }

  /* If we already have the status, then just return */
  if (handle->collected) {
    result = PROTECT(ScalarInteger(handle->exitcode));
    goto cleanup;
  }

  /* Otherwise do a non-blocking waitpid to collect zombies */
  pid = handle->pid;
  do {
    wp = waitpid(pid, &wstat, WNOHANG);
  } while (wp == -1 && errno == EINTR);

  /* Some other error? */
  if (wp == -1) {
    callr__unblock_sigchld();
    error("callr_get_exit_status: %s", strerror(errno));
  }

  /* If running, do nothing otherwise collect */
  if (wp == 0) {
    result = PROTECT(R_NilValue);
  } else {
    callr__collect_exit_status(status, wstat);
    result = PROTECT(ScalarInteger(handle->exitcode));
  }

 cleanup:
  callr__unblock_sigchld();
  UNPROTECT(1);
  return result;
}

/* See `callr_wait` above for the description of async processes and
 * possible race conditions.
 *
 * This is mostly along the lines of `callr_is_alive`. After we
 * successfully sent the signal, we try a `waitpid` just in case the
 * callr has aborted on it. This is a harmless race condition, because
 * the process might not have been cleaned up yet, when we call `waitpid`,
 * but that's OK, then its exit status will be collected later, e.g. in
 * the SIGCHLD handler.
 */

SEXP callr_signal(SEXP status, SEXP signal) {
  callr_handle_t *handle = R_ExternalPtrAddr(status);
  pid_t pid;
  int wstat, wp, ret, result;

  callr__block_sigchld();

  if (!handle) {
    callr__unblock_sigchld();
    error("Internal callr error, handle already removed");
  }

  /* If we already have the status, then return `FALSE` */
  if (handle->collected) {
    result = 0;
    goto cleanup;
  }

  /* Otherwise try to send signal */
  pid = handle->pid;
  ret = kill(pid, INTEGER(signal)[0]);

  if (ret == 0) {
    result = 1;
  } else if (ret == -1 && errno == ESRCH) {
    result = 0;
  } else {
    callr__unblock_sigchld();
    error("callr_signal: %s", strerror(errno));
    return R_NilValue;
  }

  /* Possibly dead now, collect status */
  do {
    wp = waitpid(pid, &wstat, WNOHANG);
  } while (wp == -1 && errno == EINTR);

  if (wp == -1) {
    callr__unblock_sigchld();
    error("callr_get_exit_status: %s", strerror(errno));
  }

 cleanup:
  callr__unblock_sigchld();
  return ScalarLogical(result);
}

/* This is a special case of `callr_signal`, and we implement it almost
 * the same way. We make an effort to return a TRUE/FALSE value to indicate
 * if the process died as a response to our KILL signal. This is not 100%
 * accurate because of the unavoidable race conditions. (E.g. it might have
 * been killed by another process's KILL signal.)
 *
 * To do a better job for the return value, we call a `waitpid` before
 * delivering the signal, as a final check to see if the child process is
 * still alive or not.
 */

SEXP callr_kill(SEXP status, SEXP grace) {
  callr_handle_t *handle = R_ExternalPtrAddr(status);
  pid_t pid;
  int wstat, wp, result = 0;

  callr__block_sigchld();

  if (!handle) {
    callr__unblock_sigchld();
    error("Internal callr error, handle already removed");
  }

  /* Check if we have an exit status, it yes, just return (FALSE) */
  if (handle->collected) { goto cleanup; }

  /* Do a non-blocking waitpid to collect zombies */
  pid = handle->pid;
  do {
    wp = waitpid(pid, &wstat, WNOHANG);
  } while (wp == -1 && errno == EINTR);

  /* Some other error? */
  if (wp == -1) {
    callr__unblock_sigchld();
    error("callr_kill: %s", strerror(errno));
  }

  /* If the process is not running, return (FALSE) */
  if (wp != 0) { goto cleanup; }

  /* It is still running, so a SIGKILL */
  int ret = kill(-pid, SIGKILL);
  if (ret == -1 && errno == ESRCH) { goto cleanup; }
  if (ret == -1) {
    callr__unblock_sigchld();
    error("process_kill: %s", strerror(errno));
  }

  /* Do a waitpid to collect the status and reap the zombie */
  do {
    wp = waitpid(pid, &wstat, 0);
  } while (wp == -1 && errno == EINTR);

  /* Collect exit status, and check if it was killed by a SIGKILL
     If yes, this was most probably us (although we cannot be sure in
     general... */
  callr__collect_exit_status(status, wstat);
  result = handle->exitcode == - SIGKILL;

 cleanup:
  callr__unblock_sigchld();
  return ScalarLogical(result);
}

SEXP callr_get_pid(SEXP status) {
  callr_handle_t *handle = R_ExternalPtrAddr(status);

  if (!handle) { error("Internal callr error, handle already removed"); }

  return ScalarInteger(handle->pid);
}

/* We send a 0 signal to check if the process is alive. Note that a process
 * that is in a zombie state also counts as 'alive' with this method.
*/

SEXP callr__process_exists(SEXP pid) {
  pid_t cpid = INTEGER(pid)[0];
  int res = kill(cpid, 0);
  if (res == 0) {
    return ScalarLogical(1);
  } else if (errno == ESRCH) {
    return ScalarLogical(0);
  } else {
    error("kill syscall error: %s", strerror(errno));
    return R_NilValue;
  }
}

#endif
