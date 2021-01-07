#ifndef _FILE_H
#define _FILE_H
#include <stdio.h>

#define S_4K 4096

int PagerOpenFile(const char* fileName);
int PagerWriteFile(size_t offset, size_t size, const void* data);
int PagerReadFile(size_t offset, size_t size, void* data);
int PagerCloseFile();

#endif

