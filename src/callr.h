
#ifndef CALLR_H
#define CALLR_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/socket.h>
#include <signal.h>
#include <sys/wait.h>
#include <poll.h>
#endif

#include <Rinternals.h>
#include "callr-connection.h"

#ifdef _WIN32
#include "win/callr-win.h"
#else
#include "unix/callr-unix.h"
#endif

/* API from R */

SEXP callr_exec(SEXP command, SEXP args, SEXP std_out, SEXP std_err,
		   SEXP windows_verbatim_args,
		   SEXP windows_hide_window, SEXP private_, SEXP cleanup,
		   SEXP encoding);
SEXP callr_wait(SEXP status, SEXP timeout);
SEXP callr_is_alive(SEXP status);
SEXP callr_get_exit_status(SEXP status);
SEXP callr_signal(SEXP status, SEXP signal);
SEXP callr_kill(SEXP status, SEXP grace);
SEXP callr_get_pid(SEXP status);

SEXP callr_poll(SEXP statuses, SEXP ms);

SEXP callr__process_exists(SEXP pid);
SEXP callr__disconnect_process_handle(SEXP status);

SEXP callr_is_named_pipe_open(SEXP pipe_ext);
SEXP callr_close_named_pipe(SEXP pipe_ext);
SEXP callr_create_named_pipe(SEXP name, SEXP mode);
SEXP callr_write_named_pipe(SEXP pipe_ext, SEXP text);

/* Common declarations */

/* Interruption interval in ms */
#define CALLR_INTERRUPT_INTERVAL 200

/* Various OSes and OS versions return various poll codes when the
   child's end of the pipe is closed, so we cannot provide a more
   elaborate API. See e.g. http://www.greenend.org.uk/rjk/tech/poll.html
   In particular, (recent) macOS return both POLLIN and POLLHUP,
   Cygwin return POLLHUP, and most others return just POLLIN, so there
   is not way to distinguish. Essentially, if a read would not block,
   and the fd is still open, then we return with PXREADY.

   So for us, we just have:
*/

#define PXNOPIPE  1		/* we never captured this output */
#define PXREADY   2		/* one fd is ready, or got EOF */
#define PXTIMEOUT 3		/* no fd is ready before the timeout */
#define PXCLOSED  4		/* fd was already closed when started polling */
#define PXSILENT  5		/* still open, but no data or EOF for now. No timeout, either */
                                /* but there were events on other fds */

typedef struct {
  int windows_verbatim_args;
  int windows_hide;
} callr_options_t;

#ifdef __cplusplus
}
#endif

#endif
