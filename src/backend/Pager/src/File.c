#include "File.h"
#include <unistd.h>

static FILE *DbFile;

int PagerOpenFile(const char *fileName) {
    if (access(fileName, F_OK)) {
        fclose(fopen(fileName, "w"));
    }
    DbFile = fopen(fileName, "rwb+");

    if (DbFile == NULL) {
        return 1;
    }
    return 0;
}

int PagerCloseFile() {
    return fclose(DbFile) != 0;
}

int PagerWriteFile(size_t offset, size_t size, const void *data) {
    if (size > S_4K) return 1;

    // fseek(DbFile, 0, SEEK_END);
    // int fileSize = ftell(DbFile);
    // while (fileSize < offset) {
        // fputc(0, DbFile);
        // fileSize++;
    // }

    fseek(DbFile, offset, SEEK_SET);

    int rc = fwrite(data, size, 1, DbFile);

    if (rc != 1) return 1;

    fflush(DbFile);

    return 0;
}

int PagerReadFile(size_t offset, size_t size, void* data) {
    fseek(DbFile, offset, SEEK_SET);

    return fread(data, size, 1, DbFile) != 1;
}
