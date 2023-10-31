
#include <stdio.h>
#include <stdint.h>
#include <sys/stat.h>

#include <elf.h>

/*
* When the LD_DEBUG environment variable is set, TRACE_LD messages
* will be printed to stderr
*/
#define TRACE_APP_NAME "ld.so"
#define TRACE_LD(...) do { if (__trace_ld) { TRACE(__VA_ARGS__); } } while (0)

static int __trace_ld = 0;

#include "trace.h"

#include "list.c"
#include "hashmap.c"

typedef void* (*voidptr_func)();

extern char **environ;

static __thread hashmap_t * dumb_symbol_table;
static __thread hashmap_t * glob_dat;
static __thread hashmap_t * objects_map;
static __thread hashmap_t * tls_map;
static size_t current_tls_offset = 16;

static char * last_error = NULL;

static int _target_is_suid = 0;

typedef struct elf_object {
  FILE * file;

  /* Full copy of the header. */
  Elf64_Ehdr header;

  char * dyn_string_table;
  size_t dyn_string_table_size;

  Elf64_Sym * dyn_symbol_table;
  size_t dyn_symbol_table_size;

  Elf64_Dyn * dynamic;
  Elf64_Word * dyn_hash;

  void (*init)(int, char **, char **);
  void (**init_array)(void);
  size_t init_array_size;

  uintptr_t base;

  list_t * dependencies;

  int loaded;

} elf_t;

/* Locate library for LD_LIBRARY PATH */
static char * find_lib(const char * file) {
  TRACE_LD("Finding library %s", file);

  /* If it was an absolute path, there's no need to find it. */
  if (strchr(file, '/')) {
    TRACE_LD("Found at %s", file);
    return strdup(file);
  }

  /* Collect the environment variable. */
  char * path = _target_is_suid ? NULL : getenv("LD_LIBRARY_PATH");
  if (!path) {
    /* Not set - this is the default state. Should probably read from config file? */
    path = "/lib:/usr/lib";
  }

  /* Duplicate so we can tokenize without editing */
  char * xpath = strdup(path);
  char * p, * last;
  for ((p = strtok_r(xpath, ":", &last)); p; p = strtok_r(NULL, ":", &last)) {
    /* Go through each LD_LIBRARY_PATH entry */
    int r;
    struct stat stat_buf;

    /* Append the requested file to that path */
    char * exe = malloc(strlen(p) + strlen(file) + 2);
    *exe = '\0';
    strcat(exe, p);
    strcat(exe, "/");
    strcat(exe, file);

    /* See if it exists */
    r = stat(exe, &stat_buf);
    if (r != 0) {
      /* Nope. */
      free(exe);
      continue;
    }

    /* It exists, so this is what we want. */
    TRACE_LD("Found at %s", exe);
    return exe;
  }
  free(xpath);

  /* No match found. */
  TRACE_LD("Not found");
  return NULL;
}

static elf_t * open_object(const char * path) {

  /* If no path (eg. dlopen(NULL)), return the main object (the executable). */
  if (!path) {
    return NULL;
    // return _main_obj;
  }

  /* If we've already opened a file with this name, return it - don't load things twice. */
  if (hashmap_has(objects_map, (void*)path)) {
    elf_t * object = hashmap_get(objects_map, (void*)path);
    return object;
  }

  /* Locate the library */
  char * file = find_lib(path);
  if (!file) {
    TRACE_LD("Could not find library: %s", path);
    last_error = "Could not find library.";
    return NULL;
  }

  /* Open the library. */
  FILE * f = fopen(file, "r");

  /* Failed to open? Unlikely, but could mean permissions problems. */
  if (!f) {
    TRACE_LD("Could not open library: %s", file);
    free(file);
    last_error = "Could not open library.";
    return NULL;
  }

  /* Free the expanded path, we don't need it anymore. */
  free(file);

  /* Initialize a fresh object object. */
  elf_t * object = malloc(sizeof(elf_t));
  memset(object, 0, sizeof(elf_t));
  hashmap_set(objects_map, (void*)path, object);

  /* Really unlikely... */
  if (!object) {
    TRACE_LD("Could not allocate space");
    last_error = "Could not allocate space.";
    return NULL;
  }

  object->file = f;

  /* Read the header */
  size_t r = fread(&object->header, sizeof(Elf64_Ehdr), 1, object->file);

  /* Header failed to read? */
  if (!r) {
    TRACE_LD("Failed to read object header");
    last_error = "Failed to read object header.";
    free(object);
    return NULL;
  }

  /* Is this actually an ELF object? */
  if (object->header.e_ident[0] != ELFMAG0 ||
      object->header.e_ident[1] != ELFMAG1 ||
      object->header.e_ident[2] != ELFMAG2 ||
      object->header.e_ident[3] != ELFMAG3) {

    last_error = "Not an ELF object.";
    free(object);
    return NULL;
  }

  /* Prepare a list for tracking dependencies. */
  object->dependencies = list_create();

  return object;
}

/* Calculate the size of an object file by examining its phdrs */
static size_t object_calculate_size(elf_t * object) {

  uintptr_t base_addr = (uintptr_t)-1;
  uintptr_t end_addr  = 0x0;
  size_t headers = 0;
  while (headers < object->header.e_phnum) {
    Elf64_Phdr phdr;

    /* Read the phdr */
    fseek(object->file, object->header.e_phoff + object->header.e_phentsize * headers, SEEK_SET);
    fread(&phdr, object->header.e_phentsize, 1, object->file);

    switch (phdr.p_type) {
    case PT_LOAD:
      {
        /* If this loads lower than our current base... */
        if (phdr.p_vaddr < base_addr) {
          base_addr = phdr.p_vaddr;
        }

        /* Or higher than our current end address... */
        if (phdr.p_memsz + phdr.p_vaddr > end_addr) {
          end_addr = phdr.p_memsz + phdr.p_vaddr;
        }
      }
      break;
      /* TODO: Do we care about other PHDR types here? */
    default:
      break;
    }

    headers++;
  }

  /* If base_addr is still -1, then no valid phdrs were found, and the object has no loaded size. */
  if (base_addr == (uintptr_t)-1) return 0;
  return end_addr - base_addr;
}

/* Load an object into memory */
static uintptr_t object_load(elf_t * object, uintptr_t base) {

  uintptr_t end_addr = 0x0;

  object->base = base;

  size_t headers = 0;
  while (headers < object->header.e_phnum) {
    Elf64_Phdr phdr;

    /* Read the phdr */
    fseek(object->file, object->header.e_phoff + object->header.e_phentsize * headers, SEEK_SET);
    fread(&phdr, object->header.e_phentsize, 1, object->file);

    switch (phdr.p_type) {
    case PT_LOAD:
      {
        /* Request memory to load this PHDR into */
        // char * args[] = {(char *)(base + phdr.p_vaddr), (char *)phdr.p_memsz};
        // sysfunc(TOARU_SYS_FUNC_MMAP, args);
        // TODO: mmap, error
        char *m = malloc(phdr.p_memsz);
        * ((uintptr_t*) (base + phdr.p_vaddr)) = (uintptr_t) m;

        /* Copy the code into memory */
        fseek(object->file, phdr.p_offset, SEEK_SET);
        fread((void *)(base + phdr.p_vaddr), phdr.p_filesz, 1, object->file);
        // TODO clear_cache(base + phdr.p_vaddr, base + phdr.p_vaddr + phdr.p_filesz);

        /* Zero the remaining area */
        size_t r = phdr.p_filesz;
        while (r < phdr.p_memsz) {
          *(char *)(phdr.p_vaddr + base + r) = 0;
          r++;
        }

        /* If this expands our end address, be sure to update it */
        if (end_addr < phdr.p_vaddr + base + phdr.p_memsz) {
          end_addr = phdr.p_vaddr + base + phdr.p_memsz;
        }
      }
      break;
    case PT_DYNAMIC:
      {
        /* Keep a reference to the dynamic section, which is actually loaded by a PT_LOAD normally. */
        object->dynamic = (Elf64_Dyn *)(base + phdr.p_vaddr);
      }
      break;
    default:
      break;
    }

    headers++;
  }

  return end_addr;
}

/* Perform cleanup after loading */
static int object_postload(elf_t * object) {

  /* If there is a dynamic table, parse it. */
  if (object->dynamic) {
    Elf64_Dyn * table;

    /* Locate string tables */
    table = object->dynamic;
    while (table->d_tag) {
      switch (table->d_tag) {
      case DT_HASH:
        object->dyn_hash = (Elf64_Word *)(object->base + table->d_un.d_ptr);
        object->dyn_symbol_table_size = object->dyn_hash[1];
        break;
      case DT_STRTAB:
        object->dyn_string_table = (char *)(object->base + table->d_un.d_ptr);
        break;
      case DT_SYMTAB:
        object->dyn_symbol_table = (Elf64_Sym *)(object->base + table->d_un.d_ptr);
        break;
      case DT_STRSZ: /* Size of string table */
        object->dyn_string_table_size = table->d_un.d_val;
        break;
      case DT_INIT: /* DT_INIT - initialization function */
        object->init = (void (*)(int, char **, char**))(table->d_un.d_ptr + object->base);
        break;
      case DT_INIT_ARRAY: /* DT_INIT_ARRAY - array of constructors */
        object->init_array = (void (**)(void))(table->d_un.d_ptr + object->base);
        break;
      case DT_INIT_ARRAYSZ: /* DT_INIT_ARRAYSZ - size of the table of constructors */
        object->init_array_size = table->d_un.d_val / sizeof(uintptr_t);
        break;
      default:
        TRACE_LD("Warning: Unknown dynamic table entry: %d", (int) table->d_tag);
        break;
      }
      table++;
    }

    /*
     * Read through dependencies
     * We have to do this separately from the above to make sure
     * we have the dynamic string tables loaded first, as they
     * are needed for the dependency names.
     */
    table = object->dynamic;
    while (table->d_tag) {
      switch (table->d_tag) {
      case 1:
        list_insert(object->dependencies, object->dyn_string_table + table->d_un.d_val);
        break;
      }
      table++;
    }
  }

  return 0;
}

/* Whether symbol addresses is needed for a relocation type */
static int need_symbol_for_type(unsigned int type) {
#ifdef __x86_64__
  switch(type) {
  case R_X86_64_64:
  case R_X86_64_PC32:
  case R_X86_64_COPY:
  case R_X86_64_GLOB_DAT:
  case R_X86_64_JUMP_SLOT:
  case R_X86_64_8:
  case R_X86_64_TPOFF64:
  case R_X86_64_DTPMOD64:
  case R_X86_64_DTPOFF64:
    return 1;
  default:
    return 0;
  }
#else
  switch(type) {
  case 1024:
  case 1025:
  case 1026:
  case 1030:
  case 1031:
  case 257:
    return 1;
  default:
    return 0;
  }

#endif
}

__attribute__((unused))
static size_t __tlsdesc_static(size_t * a) {
	return a[1];
}

/* Apply ELF relocations */
static int object_relocate(elf_t * object) {

  /* If there is a dynamic symbol table, load symbols */
  if (object->dyn_symbol_table) {
    Elf64_Sym * table = object->dyn_symbol_table;
    size_t i = 0;
    while (i < object->dyn_symbol_table_size) {
      char * symname = (char *)((uintptr_t)object->dyn_string_table + table->st_name);

      int is_tls = (table->st_info & 0xF) == 6;
      TRACE_LD("Adding symbol %s, tls: %d", symname, is_tls);

      /* If we haven't added this symbol to our symbol table, do so now. */
      if (table->st_shndx) {
        if (!hashmap_has(dumb_symbol_table, symname)) {
          hashmap_set(dumb_symbol_table, symname, (void*)(table->st_value + (is_tls ? 0 : object->base)));
          table->st_value = table->st_value + (is_tls ? 0 : object->base);
        } else {
          table->st_value = (uintptr_t)hashmap_get(dumb_symbol_table, symname);
        }
      } else {
        if (hashmap_has(dumb_symbol_table, symname)) {
          table->st_value = (uintptr_t)hashmap_get(dumb_symbol_table, symname);
        } else {
          table->st_value = table->st_value + (is_tls ? 0 : object->base);
        }
      }

      table++;
      i++;
    }
  }

  /* Find relocation table */
  for (uintptr_t x = 0; x < object->header.e_shentsize * object->header.e_shnum; x += object->header.e_shentsize) {
    Elf64_Shdr shdr;
    /* Load section header */
    fseek(object->file, object->header.e_shoff + x, SEEK_SET);
    fread(&shdr, object->header.e_shentsize, 1, object->file);

    /* Relocation table found */
    if (shdr.sh_type == SHT_REL) {
      TRACE_LD("Found a REL section, this is not handled.");
    } else if (shdr.sh_type == SHT_RELA) {
      Elf64_Rela * table = (Elf64_Rela *)(shdr.sh_addr + object->base);
      while ((uintptr_t)table - ((uintptr_t)shdr.sh_addr + object->base) < shdr.sh_size) {
        unsigned int symbol = ELF64_R_SYM(table->r_info);
        unsigned int type = ELF64_R_TYPE(table->r_info);
        Elf64_Sym * sym = &object->dyn_symbol_table[symbol];

        /* If we need symbol for this, get it. */
        char * symname = NULL;
        uintptr_t x = sym->st_value;
        symname = (char *)((uintptr_t)object->dyn_string_table + sym->st_name);
        TRACE_LD("Relocating symbol %s, type: %d", symname, (int) type);
        if (need_symbol_for_type(type) || (type == 5)) {
          if (symname && hashmap_has(dumb_symbol_table, symname)) {
            TRACE_LD("Symbol found: %s", symname);
            x = (uintptr_t)hashmap_get(dumb_symbol_table, symname);
          } else {
            /* This isn't fatal, but do log a message if debugging is enabled. */
            TRACE_LD("Symbol not found: %s", symname);
            x = 0x0;
          }
        }

        /* Relocations, symbol lookups, etc. */
        switch (type) {
#if defined(__x86_64__)
        case R_X86_64_GLOB_DAT: /* 6 */
          TRACE_LD("R_X86_64_GLOB_DAT");
          if (symname && hashmap_has(glob_dat, symname)) {
            x = (uintptr_t)hashmap_get(glob_dat, symname);
          } /* fallthrough */
        case R_X86_64_JUMP_SLOT: /* 7 */
          TRACE_LD("R_X86_64_JUMP_SLOT");
          memcpy((void*)(table->r_offset + object->base), &x, sizeof(uintptr_t));
          break;
        case R_X86_64_RELATIVE: /* 8*/
          TRACE_LD("R_X86_64_RELATIVE");
          x = object->base;
          x += table->r_addend;
          //*((ssize_t *)(table->r_offset + object->base));
          memcpy((void*)(table->r_offset + object->base), &x, sizeof(uintptr_t));
          break;
        case R_X86_64_64: /* 1 */
          TRACE_LD("R_X86_64_64");
          x += table->r_addend;
          memcpy((void*)(table->r_offset + object->base), &x, sizeof(uintptr_t));
          break;
        case R_X86_64_COPY: /* 5 */
          TRACE_LD("R_X86_64_COPY");
          memcpy((void *)(table->r_offset + object->base), (void *)x, sym->st_size);
          break;
        case R_X86_64_TPOFF64:
          TRACE_LD("R_X86_64_TPOFF64");
          x += *((ssize_t *)(table->r_offset + object->base));
          if (!hashmap_has(tls_map, symname)) {
            if (!sym->st_size) {
              fprintf(stderr, "Haven't placed %s in static TLS yet but don't know its size?\n", symname);
            }
            x += current_tls_offset;
            hashmap_set(tls_map, symname, (void*)(current_tls_offset));
            current_tls_offset += sym->st_size; /* TODO alignment restrictions */
          } else {
            x += (size_t)hashmap_get(tls_map, symname);
          }
          memcpy((void *)(table->r_offset + object->base), &x, sizeof(uintptr_t));
          break;
        case R_X86_64_DTPMOD64:
          TRACE_LD("R_X86_64_DTPMOD64");
          if (!hashmap_has(tls_map, symname)) { fprintf(stderr, "tls entry is unallocated?\n"); break; }
          x = 0;
          memcpy((void *)(table->r_offset + object->base), &x, sizeof(uintptr_t));
          break;
        case R_X86_64_DTPOFF64:
          TRACE_LD("R_X86_64_DTPOFF64");
          if (!hashmap_has(tls_map, symname)) { fprintf(stderr, "tls entry is unallocated?\n"); break; }
          x = table->r_addend;
          x += (size_t)hashmap_get(tls_map, symname);
          memcpy((void *)(table->r_offset + object->base), &x, sizeof(uintptr_t));
          break;
#elif defined(__aarch64__)
        case R_AARCH64_COPY: /* COPY */
          memcpy((void *)(table->r_offset + object->base), (void *)x, sym->st_size);
          break;
        case R_AARCH64_GLOB_DAT: /* GLOB_DAT */
          if (symname && hashmap_has(glob_dat, symname)) {
            x = (uintptr_t)hashmap_get(glob_dat, symname);
          } /* fallthrough */
        case R_AARCH64_JUMP_SLOT: /* JUMP_SLOT */
          memcpy((void*)(table->r_offset + object->base), &x, sizeof(uintptr_t));
          break;
        case R_AARCH64_RELATIVE: /* RELATIVE */
          x = object->base;
          x += table->r_addend;
          memcpy((void*)(table->r_offset + object->base), &x, sizeof(uintptr_t));
          break;
        case R_AARCH64_ABS64: /* ABS64 */
          x += table->r_addend;
          memcpy((void*)(table->r_offset + object->base), &x, sizeof(uintptr_t));
          break;
        case 1030: /* TLS_TPREL64 TPREL(S+A) */
          TRACE_LD("1010");
          x += *((ssize_t *)(table->r_offset + object->base)); /* A */
          if (!hashmap_has(tls_map, symname)) {
            if (!sym->st_size) {
              fprintf(stderr, "Haven't placed %s in static TLS yet but don't know its size?\n", symname);
            }
            x += current_tls_offset;
            hashmap_set(tls_map, symname, (void*)(current_tls_offset));
            current_tls_offset += sym->st_size; /* TODO alignment restrictions */
          } else {
            size_t val = (size_t)hashmap_get(tls_map, symname);
            TRACE_LD("add %#zx to %zx\n", val, x);
            x += val;
          }
          memcpy((void *)(table->r_offset + object->base), &x, sizeof(uintptr_t));
          break;
        case 1031: {
          if (symbol) {
            if (!hashmap_has(tls_map, symname)) {
              fprintf(stderr, "Warning: Don't know where to get %s (symbol %d) from TLS\n", symname, symbol);
              break;
            }
            x += *((ssize_t *)(table->r_offset + object->base));
            x += (size_t)hashmap_get(tls_map, symname);
          } else {
            /* local tls descriptor? no symbol? idk what to do with this, hope it works */
            x = current_tls_offset;
            current_tls_offset += 8*4; /* idk, alignment I guess */
          }
          uintptr_t func = (uintptr_t)&__tlsdesc_static;
          memcpy((void *)(table->r_offset + object->base), &func, sizeof(uintptr_t));
          memcpy((void *)(table->r_offset + object->base + sizeof(uintptr_t)), &x, sizeof(uintptr_t));
          break;
        }
        case R_AARCH64_IRELATIVE: {
          // TODO
          TRACE_LD("R_AARCH64_IRELATIVE ignored");
          x = (uintptr_t) NULL;
          break;
        }
#else
# error "unsupported"
#endif
#if 0
        case 6: /* GLOB_DAT */
          if (symname && hashmap_has(glob_dat, symname)) {
            x = (uintptr_t)hashmap_get(glob_dat, symname);
          }
        case 7: /* JUMP_SLOT */
          memcpy((void *)(table->r_offset + object->base), &x, sizeof(uintptr_t));
          break;
        case 1: /* 32 */
          x += *((ssize_t *)(table->r_offset + object->base));
          memcpy((void *)(table->r_offset + object->base), &x, sizeof(uintptr_t));
          break;
        case 2: /* PC32 */
          x += *((ssize_t *)(table->r_offset + object->base));
          x -= (table->r_offset + object->base);
          memcpy((void *)(table->r_offset + object->base), &x, sizeof(uintptr_t));
          break;
        case 8: /* RELATIVE */
          x = object->base;
          x += *((ssize_t *)(table->r_offset + object->base));
          memcpy((void *)(table->r_offset + object->base), &x, sizeof(uintptr_t));
          break;
        case 5: /* COPY */
          memcpy((void *)(table->r_offset + object->base), (void *)x, sym->st_size);
          break;
        case 14: /* TLS_TPOFF */
          x = *((ssize_t *)(table->r_offset + object->base));
          if (!hashmap_has(tls_map, symname)) {
            if (!sym->st_size) {
              fprintf(stderr, "Haven't placed %s in static TLS yet but don't know its size?\n", symname);
            }
            current_tls_offset += sym->st_size; /* TODO alignment restrictions */
            hashmap_set(tls_map, symname, (void*)(current_tls_offset));
            x -= current_tls_offset;
          } else {
            x -= (size_t)hashmap_get(tls_map, symname);
          }
          memcpy((void *)(table->r_offset + object->base), &x, sizeof(uintptr_t));
          break;
#endif
        default:
          {
            TRACE_LD("Unknown relocation type: %d", type);
            char msg[200];
            snprintf(msg, 200, "Unimplemented relocation (%d) requested, bailing.\n", type);
            // TODO sysfunc(TOARU_SYS_FUNC_LOGHERE, (char**)msg);
            exit(1);
          }
          break;
        }

        table++;
      }
    }
  }

  TRACE_LD("All symbols were relocated");

  return 0;
}

static void * do_actual_load(const char * filename, elf_t * lib, int flags) {
  (void)flags;

  if (!lib) {
    last_error = "could not open library (not found, or other failure)";
    TRACE_LD("could not open library");
    return NULL;
  }

  size_t lib_size = object_calculate_size(lib);

  /* Needs to be at least a page. */
  if (lib_size < 4096) {
    lib_size = 4096;
  }

  /*
   * Allocate space to load the library
   * This is where we should really be loading things into COW
   * but we don't have the functionality available.
   */
  uintptr_t load_addr = (uintptr_t)valloc(lib_size);
  object_load(lib, load_addr);

  /* Perform cleanup steps */
  object_postload(lib);

  /* Ensure dependencies are available */
  node_t * item;
  while ((item = list_pop(lib->dependencies))) {

    elf_t * _lib = open_object(item->value);

    if (!_lib) {
      /* Missing dependencies are fatal to this process, but
       * not to the entire application. */
      free((void *)load_addr);
      last_error = "Failed to load a dependency.";
      lib->loaded = 0;
      TRACE_LD("Failed to load object: %s", (const char*) item->value);
      return NULL;
    }

    if (!_lib->loaded) {
      do_actual_load(item->value, _lib, 0);
      TRACE_LD("Loaded %s at 0x%x", (const char*) item->value, (int) lib->base);
    }

  }

  /* Perform relocations */
  TRACE_LD("Relocating object %s", filename);
  object_relocate(lib);

  /* We're done with the file. */
  fclose(lib->file);

  /* If the library has an init function, call that. */
  if (lib->init) {
    TRACE_LD("Calling init for %s", filename);
    lib->init(0, 0, environ);
  }

  /* If there was an init_array, call everything in it */
  if (lib->init_array) {
    for (size_t i = 0; i < lib->init_array_size; i++) {
      TRACE_LD("Init %s #%d 0x%lx()", filename, (int) i, (long int) lib->init_array[i]);
      lib->init_array[i]();
    }
  }

  lib->loaded = 1;

  /* And return an object for the loaded library */
  TRACE_LD("Object is loaded");
  return (void *)lib;
}

void *xdlopen(const char *filename, int flags) {
  TRACE_LD("dlopen(%s,0x%x)", filename, flags);

  elf_t *lib = open_object(filename);
  if (!lib) {
    return NULL;
  }

  if (lib->loaded) {
    return lib;
  }

  void *ret = do_actual_load(filename, lib, flags);
  if (!ret) {
    TRACE_LD("Dependency load failure");
    hashmap_remove(objects_map, (void*)filename);
  }

  TRACE_LD("Loaded %s at 0x%x", filename, (int) lib->base);
  return ret;
}

void * xdlsym(void* handle, const char* symbol) {
  elf_t * lib = (elf_t*) handle;

  Elf64_Sym * sym = lib->dyn_symbol_table;
  char * strings = lib->dyn_string_table;
  int dynsym_sh_ndx = 0;
  off_t dynsym_st_value = 0;
  int found_sym = 0;

  for(int i = 0; i < lib->dyn_symbol_table_size; sym++, i++) {
    if (strcmp(strings + sym->st_name, symbol) == 0) {
      dynsym_st_value = sym->st_value;
      dynsym_sh_ndx = sym->st_shndx;
      found_sym = 1;
      break;
    }
  }

  if (!found_sym) {
    return NULL;
  }

  Elf64_Ehdr * elf = &lib->header;
  char * shoff = ((char *) elf) + elf->e_shoff;
  for (int i = 0; elf->e_shnum; i++, shoff += elf->e_shentsize) {
    if (dynsym_sh_ndx == i) {
      Elf64_Shdr * sh = (Elf64_Shdr *) shoff;
      return (void *)(sh->sh_offset + dynsym_st_value - sh->sh_addr);
    }
  }

  return NULL;
}

void linux_init(void) {
  dumb_symbol_table = hashmap_create(100);
  glob_dat = hashmap_create(10);
  objects_map = hashmap_create(10);
  tls_map = hashmap_create(10);
  char * trace_ld_env = getenv("XLD_DEBUG");
  if (trace_ld_env && (!strcmp(trace_ld_env,"1") || !strcmp(trace_ld_env,"yes"))) {
    __trace_ld = 1;
  }
}
