#pragma once


namespace pmload {

    void *my_dlsym(void *lib, const char *name, bool add_underscore = true, bool recurse = true);
    void *my_dlopen(const char *file, int flags);


    } // namespace pmload
