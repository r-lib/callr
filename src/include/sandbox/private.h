typedef int sandbox_filter_type;
#define SANDBOX_FILTER_PATH 0
#define SANDBOX_CHECK_NO_REPORT 0
#define sandbox_check(pid, kind, filter, path) 0

// evil hack, ClosureFileSystemPhysical calls mmap() with this flag which somehow ruins recursive loading
// this way, we don't have to modify dyld
#undef MAP_RESILIENT_CODESIGN
#define MAP_RESILIENT_CODESIGN 0