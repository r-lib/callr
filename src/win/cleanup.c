
#include "../callr.h"
#include "../callr-types.h"

#include <windows.h>
#include <tlhelp32.h>

void callr__cleanup_child_tree(DWORD pid) {
  HANDLE snapshot;
  PROCESSENTRY32W pr_child;
  BOOL ret;
  callr_vector_t tokill;
  callr_vector_t pids;
  callr_vector_t ppids;
  size_t i, num_kill;

  pr_child.dwSize = sizeof(PROCESSENTRY32W);

  snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if (snapshot == INVALID_HANDLE_VALUE) {
    warning("Cannot cleanup, cannot create snapshot");
  }

  callr_vector_init(&tokill, 0, 10);
  callr_vector_init(&pids, 0, 1000);
  callr_vector_init(&ppids, 0, 1000);

  ret = Process32FirstW(snapshot, &pr_child);
  if (!ret) {
    CloseHandle(snapshot);
    warning("Cannot cleanup, cannot create snapshot");
  }

  while (ret) {
    callr_vector_push_back(&pids, pr_child.th32ProcessID);
    callr_vector_push_back(&ppids, pr_child.th32ParentProcessID);
    ret = Process32NextW(snapshot, &pr_child);
  }

  callr_vector_rooted_tree(pid, &pids, &ppids, &tokill);
  num_kill = callr_vector_size(&tokill);

  for (i = 1; i < num_kill; i++) {
    pid_t child = VECTOR(tokill)[i];
    HANDLE child_process = OpenProcess(PROCESS_ALL_ACCESS, FALSE, child);
    if (child_process == NULL) continue;
    TerminateProcess(child_process, 1);
  }

  CloseHandle(snapshot);
}
