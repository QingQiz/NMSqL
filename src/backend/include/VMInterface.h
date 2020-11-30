#ifndef _VMINTERFACE_H
#define _VMINTERFACE_H

extern "C"{
typedef int (*nmsql_callback)(void*,int,char**, char**);
int exec(char* ir, nmsql_callback callback, void* args);
}

#endif
