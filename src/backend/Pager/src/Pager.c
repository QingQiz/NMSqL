#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include "File.h"
#include "Pager.h"


static DbFileHeader* dbHeader;


/// open file, return 0 if success
int OpenDbFile(const char* fileName) {
    if (access(fileName, F_OK) == 0) {
        int rc = PagerOpenFile(fileName);
        if (rc) return rc;

        dbHeader = malloc(sizeof(DbFileHeader));
        PagerReadFile(0, sizeof(DbFileHeader), dbHeader);
        return rc;
    }
    return 1;
}

/// close file opened, return 0 if success
int CloseDbFile() {
    PagerWriteFile(0, sizeof(DbFileHeader), dbHeader);
    free(dbHeader);
    return PagerCloseFile();
}

/// create a db file with db file header
int CreateDbFile(const char* fileName) {
    if (access(fileName, F_OK) == 0)  return 1;
    if (PagerOpenFile(fileName)) return 1;

    dbHeader = (DbFileHeader*)malloc(sizeof(DbFileHeader));
    dbHeader->availablePageNo = 0;
    dbHeader->maxPageNo = 0;
    dbHeader->cookie = 0;
    dbHeader->version = DB_FILE_VERSION;
    strncpy(dbHeader->dbSign, "NMSqL", 5);

    return PagerWriteDbHeader(dbHeader);
}

int WriteBPTreeMetaToPage(Pgno_t pgno, BPTreeMeta_t *bptMeta) {
    Page_t pg;

    pg.record_size = 1;
    pg.records[0].nData = sizeof(BPTreeMeta_t);
    pg.records[0].data.pData = malloc(pg.records[0].nData);
    memcpy(pg.records[0].data.pData, bptMeta, pg.records[0].nData);

    return PagerWritePage(pgno, &pg);
}

BPTreeMeta_t *GetBPTreeMetaFromPage(Pgno_t pgno) {
    Page_t *pg = PagerReadPage(pgno);

    return pg->records[0].data.pData;
}

Pgno_t CreatePage() {
    return ++(dbHeader->maxPageNo);
}

/// write db file header to db file
int PagerWriteDbHeader(DbFileHeader *header) {
    return PagerWriteFile(0, sizeof(DbFileHeader), header);
}


Page_t* GetMemPage(Pgno_t pgno) {
    // TODO add a hash table to cache page
    return PagerReadPage(pgno);
}

/// TODO: we saved pointer currently, it should be removed
Page_t* PagerReadPage(Pgno_t pgno) {
    if (pgno > dbHeader->maxPageNo) return NULL;

    void *data = malloc(S_4K);
    PagerReadFile(CALC_OFFSET(pgno), S_4K, data);

    Page_t *res = malloc(sizeof(Page_t));
    *res = *(Page_t*)data;

    res->records = malloc(res->record_size * sizeof(Record_t));

    size_t offset = sizeof(Page_t);
    int nRecRead = 0;

    while (nRecRead++ < res->record_size) {
        Record_t *x = (Record_t*)(data + offset);

        offset += sizeof(Record_t);

        x->key.data = malloc(x->key.size);
        memcpy(x->key.data, data + offset, x->key.size);

        offset += x->key.size;

        if (res->isLeaf) { // read data for leaf node
            x->data.pData = malloc(x->nData);
            memcpy(x->data.pData, data + offset, x->nData);
            offset += x->nData;
        }

        res->records[nRecRead - 1] = *x;
    }
    return res;
}

int WritePage(Pgno_t pgno, Page_t *pg) {
    return PagerWritePage(pgno, pg);
}

int PagerWritePage(Pgno_t pgno, const Page_t *pg) {
    if (pgno > dbHeader->maxPageNo) return 1;

    int offset = CALC_OFFSET(pgno);
    int res;

    res = PagerWriteFile(offset, sizeof(Page_t), pg);
    if (res) return res;

    offset += sizeof(Page_t);

    for (size_t i = 0; i < pg->record_size; i++) {
        res = PagerWriteFile(offset, sizeof(Record_t), pg->records + i);
        if (res) return res;

        offset += sizeof(Record_t);

        PagerWriteFile(offset, pg->records[i].key.size, pg->records[i].key.data);

        offset += pg->records[i].key.size;

        if (pg->isLeaf) {
            res = PagerWriteFile(offset, pg->records[i].nData, pg->records[i].data.pData);
            if (res) return res;

            offset += pg->records[i].nData;
        }
    }

    return 0;
}

int DeletePage(Pgno_t pgno) {
    // TODO
    return 0;
}
