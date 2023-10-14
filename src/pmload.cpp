#include <dlfcn.h>
#include <thread>
#include <span>
#include <sys/mman.h>

#include "dyld/common/MachOAnalyzer.h"
#include "dyld/common/ClosureFileSystemPhysical.h"

#include "pmload.hpp"

using namespace std::chrono;

bool dyld3::MachOFile::canHavePrecomputedDlopenClosure(const char *path, void (^failureReason)(const char *)) const {
	assert(0);
	return false;
}

struct MyLibHandle {
	const char magic[6] = "MAGIC";
	std::string name;
	dyld3::closure::LoadedFileInfo file_info;

	const dyld3::MachOAnalyzer *GetMA() {
		return (const dyld3::MachOAnalyzer *)file_info.fileContent; // WOW
	}
};

static void *my_dlopen(const char *file, int flags);

static void my_dyld_stub_binder() {
	assert(0);
}

thread_local std::vector<MyLibHandle *> loaders;

void *pmload::my_dlsym(void *lib, const char *name, bool add_underscore, bool recurse) {
	assert(lib);
	Diagnostics diag;
	dyld3::MachOAnalyzer::FoundSymbol foundInfo;
	auto my_handle = (MyLibHandle *)lib;
	if (strncmp(my_handle->magic, "MAGIC", 6) != 0) { // not one of our libs, maybe make this a different subclass?
		return dlsym(lib, name);
	}
	auto dyldMA = my_handle->GetMA();

	auto symbol_name = std::string(name);
	if (add_underscore) {
		symbol_name = "_" + symbol_name;
	}

	bool found = dyldMA->findExportedSymbol(diag, symbol_name.c_str(), true, foundInfo, nullptr);
	diag.assertNoError();
	if (!found && recurse) {
		// try to find in one of the other loaders
		for (auto loader : loaders) {
			if (loader == my_handle) {
				continue;
			}
			auto ptr = my_dlsym(loader, name, add_underscore, false);
			if (ptr) {
				return ptr;
			}
		}

		return nullptr;
	}
	if (foundInfo.kind == dyld3::MachOAnalyzer::FoundSymbol::Kind::headerOffset) {
		auto ptr = ((char *)dyldMA + foundInfo.value);
		return ptr;
	}
	return nullptr;
}

static void *patched_dlopen(const char *path, int mode) {
	// printf("%p\t%s\n", __builtin_return_address(0), path);

	return pmload::my_dlopen(path, mode);
}

static void *patched_dlsym(void *handle, const char *symbol) {
	auto res = pmload::my_dlsym(handle, symbol);
	// printf("dlsym(%s) at %p\n", symbol, res);
	return res;
}

// we could look at the call stack return address etc.

struct ProgramVars {
	const void *mh;
	int *NXArgcPtr;
	const char ***NXArgvPtr;
	const char ***environPtr;
	const char **__prognamePtr;
};

typedef void (*Initializer)(int argc, const char *const argv[], const char *const envp[], const char *const apple[],
                            const ProgramVars *vars);

void *pmload::my_dlopen(const char *file, int flags) {

	if (!file) {
		// walk previous handles to find the correct loader chain
		auto return_address = __builtin_return_address(0);

		for (auto loader : loaders) {
			if (return_address > loader->file_info.fileContent &&
			    return_address < (char *)loader->file_info.fileContent + loader->file_info.fileContentLen) {
				return loader;
			}
		}

		return dlopen(nullptr, flags);
	}

	// only load files once in a loader chain
	// TODO have our own loader wrappers for stuff we can only dlopen too
	for (auto loader : loaders) {
		if (strcmp(file, loader->name.c_str()) == 0) {
			return loader;
		}
	}

	assert(file); // TODO how do we handle dlopen(NULL)?
	__block Diagnostics diag;
	dyld3::closure::FileSystemPhysical fs;

	auto res_hdl = new MyLibHandle();
	res_hdl->name = file;
	res_hdl->file_info =
	    dyld3::MachOAnalyzer::load(diag, fs, file, dyld3::GradedArchs::arm64, dyld3::Platform::macOS, (char *)file);

	if (!res_hdl->file_info.fileContent) {
		// mmap failed
		auto dlopen_res = dlopen(file, flags);
		assert(dlopen_res); // TODO if this also fails is a top level error no
		return dlopen_res;
	}

	auto dyldMA = res_hdl->GetMA();
	assert(dyldMA);

	STACK_ALLOC_OVERFLOW_SAFE_ARRAY(const void *, bindTargets, 1000);
	bindTargets.resize(1000);

	auto lib_self_reference = dlopen(nullptr, RTLD_NOW);

	dyldMA->forEachDependentDylib(^(const char *loadPath, bool isWeak, bool isReExport, bool isUpward,
	                                uint32_t compatVersion, uint32_t curVersion, bool &stop) {
	  auto loadPathStr = std::string(loadPath);
	  {
		  const std::string loaderPathStr = "@loader_path";
		  size_t start_pos = loadPathStr.find(loaderPathStr);
		  if (start_pos != std::string::npos) {
			  auto fileStr = std::string(file);
			  auto last_slash_pos = fileStr.rfind("/");
			  assert(last_slash_pos != std::string::npos);
			  auto fileDirectoryStr = fileStr.substr(0, last_slash_pos);
			  loadPathStr.replace(start_pos, loaderPathStr.size(), fileDirectoryStr);
		  }
	  }

	  // FIXME
	  {
		  const std::string rPathStr = "@rpath";
		  size_t start_pos = loadPathStr.find(rPathStr);
		  if (start_pos != std::string::npos) {
			  auto fileStr = std::string(file);
			  auto last_slash_pos = fileStr.rfind("/");
			  assert(last_slash_pos != std::string::npos);
			  auto fileDirectoryStr = fileStr.substr(0, last_slash_pos);
			  loadPathStr.replace(start_pos, rPathStr.size(), fileDirectoryStr);
		  }
	  }

	  assert(my_dlopen(loadPathStr.c_str(), RTLD_NOW));
	});

	dyldMA->forEachBindTarget(
	    diag, false,
	    ^(const dyld3::MachOAnalyzer::BindTargetInfo &info, bool &stop) {
		  auto symbol = std::string(info.symbolName);
		  void *ptr = nullptr;
		  if (!ptr && symbol == "dyld_stub_binder") { // TODO what do we do with this?
			  ptr = (void *)my_dyld_stub_binder;
		  }
		  if (!ptr && symbol == "_dlopen") {
			  ptr = (void *)patched_dlopen;
		  }
		  if (!ptr && symbol == "_dlsym") {
			  ptr = (void *)patched_dlsym;
		  }
		  if (!ptr) {
			  for (auto loader : loaders) {
				  ptr = my_dlsym(loader, symbol.c_str(), false);
				  if (ptr) {
					  break;
				  }
			  }
		  }
		  if (!ptr) {
			  ptr = dlsym(lib_self_reference, symbol.c_str() + 1);
		  }
		  if (!ptr) {
			  printf("Could not find %s referenced in %s\n", symbol.c_str(), file);
		  }
		  assert(ptr);
		  bindTargets[info.targetIndex] = ptr;
	    },
	    ^(const dyld3::MachOAnalyzer::BindTargetInfo &info, bool &stop) {
		  printf("Override %s in %s\n", info.symbolName, file);
	    });

	diag.assertNoError();

	uintptr_t slide = dyldMA->getSlide();
	dyldMA->forEachSegment(^(const dyld3::MachOAnalyzer::SegmentInfo &segInfo, bool &stop) {
	  uint8_t *start = (uint8_t *)(segInfo.vmAddr + slide);
	  size_t size = (size_t)segInfo.vmSize;

	  if (segInfo.writable()) {
		  // printf("SEG\t%s, %p - %p\n", segInfo.segName, start, start  + size);
		  auto mprotect_res = mprotect((void *)start, size, PROT_WRITE);
		  assert(!mprotect_res);
	  }
	});

	dyldMA->forEachImportedSymbol(
	    diag, ^(const char *symbolName, uint64_t n_value, uint8_t n_type, uint8_t n_sect, uint16_t n_desc, bool &stop) {
	        //  printf("%s\n", symbolName);
	    });

	if (dyldMA->hasChainedFixups()) {
		dyldMA->withChainStarts(diag, 0, ^(const dyld_chained_starts_in_image *starts) {
		  dyldMA->fixupAllChainedFixups(diag, starts, slide, bindTargets,
			                            ^(void *loc, void *newValue) {
			                                // printf("%p (%ld) -> %p\n", loc, (char*)loc - (char*) dyldMA, newValue);
			                            });
		});

		diag.assertNoError();
	}

	else if (dyldMA->hasOpcodeFixups()) {
		// process all rebase opcodes
		dyldMA->forEachRebaseLocation_Opcodes(diag, ^(uint64_t runtimeOffset, bool &stop) {
		  uintptr_t *loc = (uintptr_t *)((uint8_t *)dyldMA + runtimeOffset);
		  uintptr_t locValue = *loc;
		  uintptr_t newValue = locValue + slide;

		  *loc = newValue;
		});
		diag.assertNoError();

		// process all bind opcodes
		dyldMA->forEachBindLocation_Opcodes(
		    diag,
		    ^(uint64_t runtimeOffset, unsigned targetIndex, bool &stop) {
			  uintptr_t *loc = (uintptr_t *)((uint8_t *)dyldMA + runtimeOffset);
			  uintptr_t newValue = (uintptr_t)(bindTargets[targetIndex]);

			  *loc = newValue;
		    },
		    ^(uint64_t runtimeOffset, unsigned overrideBindTargetIndex, bool &stop) {
			  // TODO do we need to distinguish here?
			  uintptr_t *loc = (uintptr_t *)((uint8_t *)dyldMA + runtimeOffset);
			  uintptr_t newValue = (uintptr_t)(bindTargets[overrideBindTargetIndex]);

			  *loc = newValue;
		    });
	} else {
		// process internal relocations
		dyldMA->forEachRebaseLocation_Relocations(diag, ^(uint64_t runtimeOffset, bool &stop) {
		  uintptr_t *loc = (uintptr_t *)((uint8_t *)dyldMA + runtimeOffset);
		  uintptr_t locValue = *loc;
		  uintptr_t newValue = locValue + slide;

		  *loc = newValue;
		});
		diag.assertNoError();

		// process external relocations
		dyldMA->forEachBindLocation_Relocations(diag, ^(uint64_t runtimeOffset, unsigned targetIndex, bool &stop) {
		  uintptr_t *loc = (uintptr_t *)((uint8_t *)dyldMA + runtimeOffset);
		  uintptr_t newValue = (uintptr_t)(bindTargets[targetIndex]);
		  *loc = newValue;
		});
	}

	diag.assertNoError();

	dyldMA->forEachSegment(^(const dyld3::MachOAnalyzer::SegmentInfo &segInfo, bool &stop) {
	  const uint8_t *start = (uint8_t *)(segInfo.vmAddr + slide);
	  size_t size = (size_t)segInfo.vmSize;
	  int mprotect_res = mprotect((void *)start, size, PROT_READ);
	  assert(!mprotect_res);

	  if (segInfo.writable()) {
		  mprotect_res = mprotect((void *)start, size, PROT_WRITE);
		  assert(!mprotect_res);
	  }
	  if (segInfo.executable()) {
		  mprotect_res = mprotect((void *)start, size, PROT_EXEC);
		  assert(!mprotect_res);
	  }
	});

	dyld3::MachOAnalyzer::VMAddrConverter vmAddrConverter = dyldMA->makeVMAddrConverter(true);

	ProgramVars vars;
	vars.mh = dyldMA;

	dyldMA->forEachInitializer(diag, vmAddrConverter, ^(uint32_t offset) {
	  auto func = (Initializer)((uint8_t *)dyldMA + offset);
	  func(0, nullptr, nullptr, nullptr, &vars);
	});

	// printf("%s loaded to %p\n", file, res_hdl->file_info.fileContent);
	loaders.push_back(res_hdl);
	return res_hdl;
}
//
// static void ThreadExecuteTasksR(const char *file) {
//
//	std::string script = R"(
// print("Hello, R")
//)";
//
//	typedef void (*Rf_initEmbeddedRFunc)(int, char **);
//	typedef void *(*R_ParseEvalStringFunc)(const char *, void *);
//
//	auto lib_hdl = my_dlopen(file, RTLD_NOW);
//	assert(lib_hdl);
//
//	auto initialize = (Rf_initEmbeddedRFunc)my_dlsym(lib_hdl, "Rf_initEmbeddedR");
//	auto run = (R_ParseEvalStringFunc)my_dlsym(lib_hdl, "R_ParseEvalString");
//	auto global_env = (void *)my_dlsym(lib_hdl, "R_GlobalEnv");
//	assert(initialize);
//	assert(run);
//	assert(global_env);
//
//	char *dummy = "R";
//	initialize(1, &dummy);
//
//	auto res = run(script.c_str(), global_env);
//}
//
//static void ThreadExecuteTasksPython(const char *file) {
//
//	std::string script = R"(
//import math
//s = 0
//for i in range(10000000):
//    s = math.ceil(s + i)
//print(s)
//)";
//
//	typedef void (*Py_InitializeExFunc)(int);
//	typedef void (*Py_FinalizeExFunc)();
//	typedef int (*PyRun_SimpleStringFunc)(const char *);
//	typedef void *(*Py_CompileStringFunc)(const char *, const char *, int);
//
//	// PyObject *Py_CompileString(const char *str, const char *filename, int start)
//
//	auto lib_hdl = my_dlopen(file, RTLD_NOW);
//	assert(lib_hdl);
//
//	auto initialize = (Py_InitializeExFunc)my_dlsym(lib_hdl, "Py_InitializeEx");
//	auto run = (PyRun_SimpleStringFunc)my_dlsym(lib_hdl, "PyRun_SimpleString");
//	auto finalize = (Py_FinalizeExFunc)my_dlsym(lib_hdl, "Py_FinalizeEx");
//	auto compile = (Py_CompileStringFunc)my_dlsym(lib_hdl, "Py_CompileString");
//
//	initialize(0);
//	// auto code = compile(script.c_str(), "moo", input);
//	auto res = run(script.c_str());
//	finalize();
//}
//
//int main(int argc, char **argv) {
//	std::vector<std::unique_ptr<std::thread>> threads;
//
//	int num_threads = 10;
//
//	for (int i = 0; i < num_threads; i++) {
//		threads.push_back(std::make_unique<std::thread>(ThreadExecuteTasksPython, argv[1]));
//	}
//
//	for (int i = 0; i < num_threads; i++) {
//		threads[i]->join();
//	}
//
//	return 0;
//}
