
#include <R.h>
#include <R_ext/Rdynload.h>

#include "../callr.h"

static HANDLE callr__global_job_handle = NULL;

static void callr__init_global_job_handle(void) {
  /* Create a job object and set it up to kill all contained processes when
   * it's closed. Since this handle is made non-inheritable and we're not
   * giving it to anyone, we're the only process holding a reference to it.
   * That means that if this process exits it is closed and all the
   * processes it contains are killed. All processes created with callr
   * that are spawned without the cleanup flag are assigned to this job.
   *
   * We're setting the JOB_OBJECT_LIMIT_SILENT_BREAKAWAY_OK flag so only
   * the processes that we explicitly add are affected, and *their*
   * subprocesses are not. This ensures that our child processes are not
   * limited in their ability to use job control on Windows versions that
   * don't deal with nested jobs (prior to Windows 8 / Server 2012). It
   * also lets our child processes create detached processes without
   * explicitly breaking away from job control (which callr_exec
   * doesn't do, either). */

  SECURITY_ATTRIBUTES attr;
  JOBOBJECT_EXTENDED_LIMIT_INFORMATION info;

  memset(&attr, 0, sizeof attr);
  attr.bInheritHandle = FALSE;

  memset(&info, 0, sizeof info);
  info.BasicLimitInformation.LimitFlags =
      JOB_OBJECT_LIMIT_BREAKAWAY_OK |
      JOB_OBJECT_LIMIT_SILENT_BREAKAWAY_OK |
      JOB_OBJECT_LIMIT_DIE_ON_UNHANDLED_EXCEPTION |
      JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;

  callr__global_job_handle = CreateJobObjectW(&attr, NULL);
  if (callr__global_job_handle == NULL) {
    CALLR_ERROR("Creating global job object", GetLastError());
  }

  if (!SetInformationJobObject(callr__global_job_handle,
                               JobObjectExtendedLimitInformation,
                               &info,
                               sizeof info)) {
    CALLR_ERROR("Setting up global job object", GetLastError());
  }
}

void R_init_callr_win() {
  /* Nothing to do currently */
}

SEXP callr__killem_all() {
  if (callr__global_job_handle) {
    TerminateJobObject(callr__global_job_handle, 1);
    CloseHandle(callr__global_job_handle);
    callr__global_job_handle = NULL;
  }
  return R_NilValue;
}

int callr__utf8_to_utf16_alloc(const char* s, WCHAR** ws_ptr) {
  int ws_len, r;
  WCHAR* ws;

  ws_len = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ s,
    /* cbMultiByte =    */ -1,
    /* lpWideCharStr =  */ NULL,
    /* cchWideChar =    */ 0);

  if (ws_len <= 0) { return GetLastError(); }

  ws = (WCHAR*) R_alloc(ws_len,  sizeof(WCHAR));
  if (ws == NULL) { return ERROR_OUTOFMEMORY; }

  r = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ s,
    /* cbMultiBytes =   */ -1,
    /* lpWideCharStr =  */ ws,
    /* cchWideChar =    */ ws_len);

  if (r != ws_len) {
    error("callr error interpreting UTF8 command or arguments");
  }

  *ws_ptr = ws;
  return 0;
}

WCHAR* callr__quote_cmd_arg(const WCHAR *source, WCHAR *target) {
  size_t len = wcslen(source);
  size_t i;
  int quote_hit;
  WCHAR* start;

  if (len == 0) {
    /* Need double quotation for empty argument */
    *(target++) = L'"';
    *(target++) = L'"';
    return target;
  }

  if (NULL == wcspbrk(source, L" \t\"")) {
    /* No quotation needed */
    wcsncpy(target, source, len);
    target += len;
    return target;
  }

  if (NULL == wcspbrk(source, L"\"\\")) {
    /*
     * No embedded double quotes or backlashes, so I can just wrap
     * quote marks around the whole thing.
     */
    *(target++) = L'"';
    wcsncpy(target, source, len);
    target += len;
    *(target++) = L'"';
    return target;
  }

  /*
   * Expected input/output:
   *   input : hello"world
   *   output: "hello\"world"
   *   input : hello""world
   *   output: "hello\"\"world"
   *   input : hello\world
   *   output: hello\world
   *   input : hello\\world
   *   output: hello\\world
   *   input : hello\"world
   *   output: "hello\\\"world"
   *   input : hello\\"world
   *   output: "hello\\\\\"world"
   *   input : hello world\
   *   output: "hello world\\"
   */

  *(target++) = L'"';
  start = target;
  quote_hit = 1;

  for (i = len; i > 0; --i) {
    *(target++) = source[i - 1];

    if (quote_hit && source[i - 1] == L'\\') {
      *(target++) = L'\\';
    } else if(source[i - 1] == L'"') {
      quote_hit = 1;
      *(target++) = L'\\';
    } else {
      quote_hit = 0;
    }
  }
  target[0] = L'\0';
  wcsrev(start);
  *(target++) = L'"';
  return target;
}

static int callr__make_program_args(SEXP args, int verbatim_arguments,
				       WCHAR **dst_ptr) {
  const char* arg;
  WCHAR* dst = NULL;
  WCHAR* temp_buffer = NULL;
  size_t dst_len = 0;
  size_t temp_buffer_len = 0;
  WCHAR* pos;
  int arg_count = LENGTH(args);
  int err = 0;
  int i;

  /* Count the required size. */
  for (i = 0; i < arg_count; i++) {
    DWORD arg_len;
    arg = CHAR(STRING_ELT(args, i));

    arg_len = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ arg,
    /* cbMultiBytes =   */ -1,
    /* lpWideCharStr =  */ NULL,
    /* cchWideChar =    */ 0);

    if (arg_len == 0) { return GetLastError(); }

    dst_len += arg_len;

    if (arg_len > temp_buffer_len) { temp_buffer_len = arg_len; }
  }

  /* Adjust for potential quotes. Also assume the worst-case scenario */
  /* that every character needs escaping, so we need twice as much space. */
  dst_len = dst_len * 2 + arg_count * 2;

  /* Allocate buffer for the final command line. */
  dst = (WCHAR*) R_alloc(dst_len, sizeof(WCHAR));

  /* Allocate temporary working buffer. */
  temp_buffer = (WCHAR*) R_alloc(temp_buffer_len, sizeof(WCHAR));

  pos = dst;
  for (i = 0; i < arg_count; i++) {
    DWORD arg_len;
    arg = CHAR(STRING_ELT(args, i));

    /* Convert argument to wide char. */
    arg_len = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ arg,
    /* cbMultiBytes =   */ -1,
    /* lpWideCharStr =  */ temp_buffer,
    /* cchWideChar =    */ (int) (dst + dst_len - pos));

    if (arg_len == 0) {
      err = GetLastError();
      goto error;
    }

    if (verbatim_arguments) {
      /* Copy verbatim. */
      wcscpy(pos, temp_buffer);
      pos += arg_len - 1;
    } else {
      /* Quote/escape, if needed. */
      pos = callr__quote_cmd_arg(temp_buffer, pos);
    }

    *pos++ = i < arg_count - 1 ? L' ' : L'\0';
  }

  *dst_ptr = dst;
  return 0;

error:
  return err;
}

static WCHAR* callr__search_path_join_test(const WCHAR* dir,
					      size_t dir_len,
					      const WCHAR* name,
					      size_t name_len,
					      const WCHAR* ext,
					      size_t ext_len,
					      const WCHAR* cwd,
					      size_t cwd_len) {
  WCHAR *result, *result_pos;
  DWORD attrs;
  if (dir_len > 2 && dir[0] == L'\\' && dir[1] == L'\\') {
    /* It's a UNC path so ignore cwd */
    cwd_len = 0;
  } else if (dir_len >= 1 && (dir[0] == L'/' || dir[0] == L'\\')) {
    /* It's a full path without drive letter, use cwd's drive letter only */
    cwd_len = 2;
  } else if (dir_len >= 2 && dir[1] == L':' &&
      (dir_len < 3 || (dir[2] != L'/' && dir[2] != L'\\'))) {
    /* It's a relative path with drive letter (ext.g. D:../some/file)
     * Replace drive letter in dir by full cwd if it points to the same drive,
     * otherwise use the dir only.
     */
    if (cwd_len < 2 || _wcsnicmp(cwd, dir, 2) != 0) {
      cwd_len = 0;
    } else {
      dir += 2;
      dir_len -= 2;
    }
  } else if (dir_len > 2 && dir[1] == L':') {
    /* It's an absolute path with drive letter
     * Don't use the cwd at all
     */
    cwd_len = 0;
  }

  /* Allocate buffer for output */
  result = result_pos = (WCHAR*) R_alloc(
    (cwd_len + 1 + dir_len + 1 + name_len + 1 + ext_len + 1),
    sizeof(WCHAR));

  /* Copy cwd */
  wcsncpy(result_pos, cwd, cwd_len);
  result_pos += cwd_len;

  /* Add a path separator if cwd didn't end with one */
  if (cwd_len && wcsrchr(L"\\/:", result_pos[-1]) == NULL) {
    result_pos[0] = L'\\';
    result_pos++;
  }

  /* Copy dir */
  wcsncpy(result_pos, dir, dir_len);
  result_pos += dir_len;

  /* Add a separator if the dir didn't end with one */
  if (dir_len && wcsrchr(L"\\/:", result_pos[-1]) == NULL) {
    result_pos[0] = L'\\';
    result_pos++;
  }

  /* Copy filename */
  wcsncpy(result_pos, name, name_len);
  result_pos += name_len;

  if (ext_len) {
    /* Add a dot if the filename didn't end with one */
    if (name_len && result_pos[-1] != '.') {
      result_pos[0] = L'.';
      result_pos++;
    }

    /* Copy extension */
    wcsncpy(result_pos, ext, ext_len);
    result_pos += ext_len;
  }

  /* Null terminator */
  result_pos[0] = L'\0';

  attrs = GetFileAttributesW(result);

  if (attrs != INVALID_FILE_ATTRIBUTES &&
      !(attrs & FILE_ATTRIBUTE_DIRECTORY)) {
    return result;
  }

  return NULL;
}


/*
 * Helper function for search_path
 */
static WCHAR* callr__path_search_walk_ext(const WCHAR *dir,
					     size_t dir_len,
					     const WCHAR *name,
					     size_t name_len,
					     WCHAR *cwd,
					     size_t cwd_len,
					     int name_has_ext) {
  WCHAR* result;

  /* If the name itself has a nonempty extension, try this extension first */
  if (name_has_ext) {
    result = callr__search_path_join_test(dir, dir_len,
					     name, name_len,
					     L"", 0,
					     cwd, cwd_len);
    if (result != NULL) {
      return result;
    }
  }

  /* Try .com extension */
  result = callr__search_path_join_test(dir, dir_len,
					   name, name_len,
					   L"com", 3,
					   cwd, cwd_len);
  if (result != NULL) {
    return result;
  }

  /* Try .exe extension */
  result = callr__search_path_join_test(dir, dir_len,
					   name, name_len,
					   L"exe", 3,
					   cwd, cwd_len);
  if (result != NULL) {
    return result;
  }

  return NULL;
}


/*
 * search_path searches the system path for an executable filename -
 * the windows API doesn't provide this as a standalone function nor as an
 * option to CreateProcess.
 *
 * It tries to return an absolute filename.
 *
 * Furthermore, it tries to follow the semantics that cmd.exe, with this
 * exception that PATHEXT environment variable isn't used. Since CreateProcess
 * can start only .com and .exe files, only those extensions are tried. This
 * behavior equals that of msvcrt's spawn functions.
 *
 * - Do not search the path if the filename already contains a path (either
 *   relative or absolute).
 *
 * - If there's really only a filename, check the current directory for file,
 *   then search all path directories.
 *
 * - If filename specified has *any* extension, search for the file with the
 *   specified extension first.
 *
 * - If the literal filename is not found in a directory, try *appending*
 *   (not replacing) .com first and then .exe.
 *
 * - The path variable may contain relative paths; relative paths are relative
 *   to the cwd.
 *
 * - Directories in path may or may not end with a trailing backslash.
 *
 * - CMD does not trim leading/trailing whitespace from path/pathex entries
 *   nor from the environment variables as a whole.
 *
 * - When cmd.exe cannot read a directory, it will just skip it and go on
 *   searching. However, unlike posix-y systems, it will happily try to run a
 *   file that is not readable/executable; if the spawn fails it will not
 *   continue searching.
 *
 * UNC path support: we are dealing with UNC paths in both the path and the
 * filename. This is a deviation from what cmd.exe does (it does not let you
 * start a program by specifying an UNC path on the command line) but this is
 * really a pointless restriction.
 *
 */
static WCHAR* callr__search_path(const WCHAR *file,
				    WCHAR *cwd,
				    const WCHAR *path) {
  int file_has_dir;
  WCHAR* result = NULL;
  WCHAR *file_name_start;
  WCHAR *dot;
  const WCHAR *dir_start, *dir_end, *dir_path;
  size_t dir_len;
  int name_has_ext;

  size_t file_len = wcslen(file);
  size_t cwd_len = wcslen(cwd);

  /* If the caller supplies an empty filename,
   * we're not gonna return c:\windows\.exe -- GFY!
   */
  if (file_len == 0
      || (file_len == 1 && file[0] == L'.')) {
    return NULL;
  }

  /* Find the start of the filename so we can split the directory from the */
  /* name. */
  for (file_name_start = (WCHAR*)file + file_len;
       file_name_start > file
           && file_name_start[-1] != L'\\'
           && file_name_start[-1] != L'/'
           && file_name_start[-1] != L':';
       file_name_start--);

  file_has_dir = file_name_start != file;

  /* Check if the filename includes an extension */
  dot = wcschr(file_name_start, L'.');
  name_has_ext = (dot != NULL && dot[1] != L'\0');

  if (file_has_dir) {
    /* The file has a path inside, don't use path */
    result = callr__path_search_walk_ext(
        file, file_name_start - file,
        file_name_start, file_len - (file_name_start - file),
        cwd, cwd_len,
        name_has_ext);

  } else {
    dir_end = path;

    /* The file is really only a name; look in cwd first, then scan path */
    result = callr__path_search_walk_ext(L"", 0,
					    file, file_len,
					    cwd, cwd_len,
					    name_has_ext);

    while (result == NULL) {
      if (*dir_end == L'\0') {
        break;
      }

      /* Skip the separator that dir_end now points to */
      if (dir_end != path || *path == L';') {
        dir_end++;
      }

      /* Next slice starts just after where the previous one ended */
      dir_start = dir_end;

      /* Slice until the next ; or \0 is found */
      dir_end = wcschr(dir_start, L';');
      if (dir_end == NULL) {
        dir_end = wcschr(dir_start, L'\0');
      }

      /* If the slice is zero-length, don't bother */
      if (dir_end - dir_start == 0) {
        continue;
      }

      dir_path = dir_start;
      dir_len = dir_end - dir_start;

      /* Adjust if the path is quoted. */
      if (dir_path[0] == '"' || dir_path[0] == '\'') {
        ++dir_path;
        --dir_len;
      }

      if (dir_path[dir_len - 1] == '"' || dir_path[dir_len - 1] == '\'') {
        --dir_len;
      }

      result = callr__path_search_walk_ext(dir_path, dir_len,
					      file, file_len,
					      cwd, cwd_len,
					      name_has_ext);
    }
  }

  return result;
}

void callr__error(const char *message, DWORD errorcode,
		     const char *file, int line) {
  LPVOID lpMsgBuf;
  char *msg;

  FormatMessage(
    FORMAT_MESSAGE_ALLOCATE_BUFFER |
    FORMAT_MESSAGE_FROM_SYSTEM |
    FORMAT_MESSAGE_IGNORE_INSERTS,
    NULL,
    errorcode,
    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
    (LPTSTR) &lpMsgBuf,
    0, NULL );

  msg = R_alloc(1, strlen(lpMsgBuf) + 1);
  strcpy(msg, lpMsgBuf);
  LocalFree(lpMsgBuf);

  error("callr error, %s: #%d %s at '%s:%d'", message,
	(int) errorcode, msg, file, line);
}

void callr__collect_exit_status(SEXP status, DWORD exitcode);

DWORD callr__terminate(callr_handle_t *handle, SEXP status) {
  DWORD err;

  err = TerminateProcess(handle->hProcess, 2);
  if (err) callr__collect_exit_status(status, 2);

  WaitForSingleObject(handle->hProcess, INFINITE);
  CloseHandle(handle->hProcess);
  handle->hProcess = 0;
  return err;
}

SEXP callr__disconnect_process_handle(SEXP status) {
  R_SetExternalPtrTag(status, R_NilValue);
  return R_NilValue;
}

void callr__finalizer(SEXP status) {
  callr_handle_t *handle = (callr_handle_t*) R_ExternalPtrAddr(status);
  SEXP private;

  if (!handle) return;

  if (handle->cleanup && !handle->collected) {
    /* Just in case it is running */
    callr__terminate(handle, status);
  }

  /* Copy over pid and exit status */
  private = PROTECT(R_ExternalPtrTag(status));
  if (!isNull(private)) {
    SEXP sone = PROTECT(ScalarLogical(1));
    SEXP spid = PROTECT(ScalarInteger(handle->dwProcessId));
    SEXP sexitcode = PROTECT(ScalarInteger(handle->exitcode));

    defineVar(install("exited"), sone, private);
    defineVar(install("pid"), spid, private);
    defineVar(install("exitcode"), sexitcode, private);
    UNPROTECT(3);
  }
  UNPROTECT(1);

  if (handle->hProcess) CloseHandle(handle->hProcess);
  R_ClearExternalPtr(status);
  callr__handle_destroy(handle);
}

SEXP callr__make_handle(SEXP private, int cleanup) {
  callr_handle_t * handle;
  SEXP result;

  handle = (callr_handle_t*) malloc(sizeof(callr_handle_t));
  if (!handle) { error("Out of memory"); }
  memset(handle, 0, sizeof(callr_handle_t));

  result = PROTECT(R_MakeExternalPtr(handle, private, R_NilValue));
  R_RegisterCFinalizerEx(result, callr__finalizer, 1);
  handle->cleanup = cleanup;

  UNPROTECT(1);
  return result;
}

void callr__handle_destroy(callr_handle_t *handle) {
  if (!handle) return;
  if (handle->child_stdio_buffer) free(handle->child_stdio_buffer);
  free(handle);
}

SEXP callr_exec(SEXP command, SEXP args, SEXP std_out, SEXP std_err,
		   SEXP windows_verbatim_args, SEXP windows_hide,
		   SEXP private, SEXP cleanup, SEXP encoding) {

  const char *cstd_out = isNull(std_out) ? 0 : CHAR(STRING_ELT(std_out, 0));
  const char *cstd_err = isNull(std_err) ? 0 : CHAR(STRING_ELT(std_err, 0));
  const char *cencoding = CHAR(STRING_ELT(encoding, 0));

  int err = 0;
  WCHAR *path;
  WCHAR *application_path = NULL, *application = NULL, *arguments = NULL,
    *cwd = NULL;
  callr_options_t options;
  STARTUPINFOW startup = { 0 };
  PROCESS_INFORMATION info = { 0 };
  DWORD process_flags;

  callr_handle_t *handle;
  int ccleanup = INTEGER(cleanup)[0];
  SEXP result;
  DWORD dwerr;

  options.windows_verbatim_args = LOGICAL(windows_verbatim_args)[0];
  options.windows_hide = LOGICAL(windows_hide)[0];

  err = callr__utf8_to_utf16_alloc(CHAR(STRING_ELT(command, 0)),
				      &application);
  if (err) { CALLR_ERROR("utf8 -> utf16 conversion", err); }

  err = callr__make_program_args(
      args,
      options.windows_verbatim_args,
      &arguments);
  if (err) { CALLR_ERROR("making program args", err); }

  /* Inherit cwd */
  {
    DWORD cwd_len, r;

    cwd_len = GetCurrentDirectoryW(0, NULL);
    if (!cwd_len) {
      CALLR_ERROR("get current directory length", GetLastError());
    }

    cwd = (WCHAR*) R_alloc(cwd_len, sizeof(WCHAR));

    r = GetCurrentDirectoryW(cwd_len, cwd);
    if (r == 0 || r >= cwd_len) {
      CALLR_ERROR("get current directory", GetLastError());
    }
  }

  /* Get PATH environment variable */
  {
    DWORD path_len, r;

    path_len = GetEnvironmentVariableW(L"PATH", NULL, 0);
    if (!path_len) {
      CALLR_ERROR("get env var length", GetLastError());
    }

    path = (WCHAR*) R_alloc(path_len, sizeof(WCHAR));

    r = GetEnvironmentVariableW(L"PATH", path, path_len);
    if (r == 0 || r >= path_len) {
      CALLR_ERROR("get env var", GetLastError());
    }
  }

  result = PROTECT(callr__make_handle(private, ccleanup));
  handle = R_ExternalPtrAddr(result);

  err = callr__stdio_create(handle, cstd_out, cstd_err,
			       &handle->child_stdio_buffer, private,
			       cencoding);
  if (err) { CALLR_ERROR("setup stdio", err); }

  application_path = callr__search_path(application, cwd, path);
  if (!application_path) {
    R_ClearExternalPtr(result);
    callr__stdio_destroy(handle->child_stdio_buffer);
    free(handle);
    error("Command not found");
  }

  startup.cb = sizeof(startup);
  startup.lpReserved = NULL;
  startup.lpDesktop = NULL;
  startup.lpTitle = NULL;
  startup.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;

  startup.cbReserved2 = callr__stdio_size(handle->child_stdio_buffer);
  startup.lpReserved2 = (BYTE*) handle->child_stdio_buffer;

  startup.hStdInput = callr__stdio_handle(handle->child_stdio_buffer, 0);
  startup.hStdOutput = callr__stdio_handle(handle->child_stdio_buffer, 1);
  startup.hStdError = callr__stdio_handle(handle->child_stdio_buffer, 2);
  startup.wShowWindow = options.windows_hide ? SW_HIDE : SW_SHOWDEFAULT;

  process_flags = CREATE_UNICODE_ENVIRONMENT |
    CREATE_SUSPENDED |
    CREATE_NO_WINDOW;

  if (!ccleanup) {
    /* Note that we're not setting the CREATE_BREAKAWAY_FROM_JOB flag. That
     * means that callr might not let you create a fully deamonized
     * process when run under job control. However the type of job control
     * that callr itself creates doesn't trickle down to subprocesses
     * so they can still daemonize.
     *
     * A reason to not do this is that CREATE_BREAKAWAY_FROM_JOB makes the
     * CreateProcess call fail if we're under job control that doesn't
     * allow breakaway.
     */

    process_flags |= DETACHED_PROCESS | CREATE_NEW_PROCESS_GROUP;
  }

  err = CreateProcessW(
    /* lpApplicationName =    */ application_path,
    /* lpCommandLine =        */ arguments,
    /* lpProcessAttributes =  */ NULL,
    /* lpThreadAttributes =   */ NULL,
    /* bInheritHandles =      */ 1,
    /* dwCreationFlags =      */ process_flags,
    /* lpEnvironment =        */ NULL,
    /* lpCurrentDirectory =   */ cwd,
    /* lpStartupInfo =        */ &startup,
    /* lpProcessInformation = */ &info);

  if (!err) { CALLR_ERROR("create process", GetLastError()); }

  handle->hProcess = info.hProcess;
  handle->dwProcessId = info.dwProcessId;

  /* If the process isn't spawned as detached, assign to the global job */
  /* object so windows will kill it when the parent process dies. */
  if (!ccleanup) {
    if (! callr__global_job_handle) callr__init_global_job_handle();

    if (!AssignProcessToJobObject(callr__global_job_handle, info.hProcess)) {
      /* AssignProcessToJobObject might fail if this process is under job
       * control and the job doesn't have the
       * JOB_OBJECT_LIMIT_SILENT_BREAKAWAY_OK flag set, on a Windows
       * version that doesn't support nested jobs.
       *
       * When that happens we just swallow the error and continue without
       * establishing a kill-child-on-parent-exit relationship, otherwise
       * there would be no way for R/callr applications run under job
       * control to spawn processes at all.
       */
      DWORD err = GetLastError();
      if (err != ERROR_ACCESS_DENIED) {
	CALLR_ERROR("Assign to job object", err);
      }
    }
  }

  dwerr = ResumeThread(info.hThread);
  if (dwerr == (DWORD) -1) CALLR_ERROR("resume thread", GetLastError());
  CloseHandle(info.hThread);

  callr__stdio_destroy(handle->child_stdio_buffer);
  handle->child_stdio_buffer = NULL;

  UNPROTECT(1);
  return result;
}

void callr__collect_exit_status(SEXP status, DWORD exitcode) {
  callr_handle_t *handle = R_ExternalPtrAddr(status);
  handle->exitcode = exitcode;
  handle->collected = 1;
}

SEXP callr_wait(SEXP status, SEXP timeout) {
  int ctimeout = INTEGER(timeout)[0], timeleft = ctimeout;
  callr_handle_t *handle = R_ExternalPtrAddr(status);
  DWORD err, err2, exitcode;

  if (handle->collected) return R_NilValue;

  err2 = WAIT_TIMEOUT;
  while (ctimeout < 0 || timeleft > CALLR_INTERRUPT_INTERVAL) {
    err2 = WaitForSingleObject(handle->hProcess, CALLR_INTERRUPT_INTERVAL);
    if (err2 != WAIT_TIMEOUT) break;
    R_CheckUserInterrupt();
    timeleft -= CALLR_INTERRUPT_INTERVAL;
  }

  /* Maybe there is some time left from the timeout */
  if (err2 == WAIT_TIMEOUT && timeleft >= 0) {
    err2 = WaitForSingleObject(handle->hProcess, timeleft);
  }

  if (err2 == WAIT_FAILED) {
    CALLR_ERROR("wait on process", GetLastError());
  } else if (err2 == WAIT_TIMEOUT) {
    return ScalarLogical(FALSE);
  }

  /* Collect  */
  err = GetExitCodeProcess(handle->hProcess, &exitcode);
  if (!err) { CALLR_ERROR("get exit code after wait", GetLastError()); }

  callr__collect_exit_status(status, exitcode);

  return ScalarLogical(TRUE);
}

SEXP callr_is_alive(SEXP status) {
  callr_handle_t *handle = R_ExternalPtrAddr(status);
  DWORD err, exitcode;

  if (handle->collected) return ScalarLogical(0);

  /* Otherwise try to get exit code */
  err = GetExitCodeProcess(handle->hProcess, &exitcode);
  if (!err) {
    CALLR_ERROR("get exit code to check if alive", GetLastError());
  }

  if (exitcode == STILL_ACTIVE) {
    return ScalarLogical(1);
  } else {
    callr__collect_exit_status(status, exitcode);
    return ScalarLogical(0);
  }
}

SEXP callr_get_exit_status(SEXP status) {
  callr_handle_t *handle = R_ExternalPtrAddr(status);
  DWORD err, exitcode;

  if (handle->collected) return ScalarInteger(handle->exitcode);

  /* Otherwise try to get exit code */
  err = GetExitCodeProcess(handle->hProcess, &exitcode);
  if (!err) {CALLR_ERROR("get exit status", GetLastError()); }

  if (exitcode == STILL_ACTIVE) {
    return R_NilValue;
  } else {
    callr__collect_exit_status(status, exitcode);
    return ScalarInteger(handle->exitcode);
  }
}

SEXP callr_signal(SEXP status, SEXP signal) {
  callr_handle_t *handle = R_ExternalPtrAddr(status);
  DWORD err, exitcode = STILL_ACTIVE;

  if (handle->collected) return ScalarLogical(0);

  switch (INTEGER(signal)[0]) {

  case 15:   /* SIGTERM */
  case 9:    /* SIGKILL */
  case 2: {  /* SIGINT */
    /* Call GetExitCodeProcess to see if it is done */
    /* TODO: there is a race condition here, might finish right before
       we are terminating it... */
    err = GetExitCodeProcess(handle->hProcess, &exitcode);
    if (!err) {
      CALLR_ERROR("get exit code after signal", GetLastError());
    }

    if (exitcode == STILL_ACTIVE) {

      /* We only cleanup the tree if the process is still running. */
      /* TODO: we are not running this for now, until we can properly
	 work around pid reuse. */
      /* callr__cleanup_child_tree(handle->dwProcessId); */
      err = callr__terminate(handle, status);
      return ScalarLogical(err != 0);

    } else {
      callr__collect_exit_status(status, exitcode);
      return ScalarLogical(0);
    }
  }

  case 0: {
    /* Health check: is the process still alive? */
    err = GetExitCodeProcess(handle->hProcess, &exitcode);
    if (!err) {
      CALLR_ERROR("get exit code for signal 0", GetLastError());
    }

    if (exitcode == STILL_ACTIVE) {
      return ScalarLogical(1);
    } else {
      return ScalarLogical(0);
    }
  }

  default:
    error("Unsupported signal on this platform");
    return R_NilValue;
  }
}

SEXP callr_kill(SEXP status, SEXP grace) {
  return callr_signal(status, ScalarInteger(9));
}

SEXP callr_get_pid(SEXP status) {
  callr_handle_t *handle = R_ExternalPtrAddr(status);

  if (!handle) { error("Internal callr error, handle already removed"); }

  return ScalarInteger(handle->dwProcessId);
}

SEXP callr__process_exists(SEXP pid) {
  DWORD cpid = INTEGER(pid)[0];
  HANDLE proc = OpenProcess(PROCESS_QUERY_INFORMATION, FALSE, cpid);
  if (proc == NULL) {
    DWORD err = GetLastError();
    if (err == ERROR_INVALID_PARAMETER) return ScalarLogical(0);
    CALLR_ERROR("open process to check if it exists", err);
    return R_NilValue;
  } else {
    /* Maybe just finished, and in that case we still have a valid handle.
       Let's see if this is the case. */
    DWORD exitcode;
    DWORD err = GetExitCodeProcess(proc, &exitcode);
    CloseHandle(proc);
    if (!err) {
      CALLR_ERROR(
	"get exit code to check if it exists",
	GetLastError());
    }
    return ScalarLogical(exitcode == STILL_ACTIVE);
  }
}
