#include <stdint.h>
#include <stdlib.h>
#include <string.h>

struct TabeMetadata {
    int32_t indexCnt;
    char **index;

    int32_t columnCnt;
    char **column;

    int32_t cookie;
};


struct TabeMetadata* getTableMetadata(char* tableName) {
    static struct TabeMetadata metadata;
    metadata.indexCnt = 2;
    metadata.index = (char**)malloc(sizeof(char*)*metadata.indexCnt);
    metadata.index[0] = "idx_xxx_a_b:a,b";
    metadata.index[1] = "idx_xxx_a:a";

    metadata.columnCnt = 3;
    metadata.column = (char**)malloc(sizeof(char*)*metadata.columnCnt);
    metadata.column[0] = "a";
    metadata.column[1] = "b";
    metadata.column[2] = "c";

    metadata.cookie = 281;

    return &metadata;
}

char** getAllTableNames() {
    char **a= malloc(2*sizeof(char*));
    a[0] = "xxx";
    a[1] = NULL;
    return a;
}
