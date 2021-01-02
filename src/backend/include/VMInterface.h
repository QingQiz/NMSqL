#ifndef _VMINTERFACE_H
#define _VMINTERFACE_H

extern "C" {
typedef int (*nmsql_callback)(void *, const int, const char *const *,
                              const char *const *);
int exec(const void *dbEngine, const char *ir, nmsql_callback callback,
         void *args);
}

#endif
