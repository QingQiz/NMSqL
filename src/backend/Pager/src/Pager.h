#ifndef _PAGER_H
#define _PAGER_H

#include <stddef.h>

#include "../../include/PagerInterface.h"

#define DB_FILE_VERSION 1
#define CALC_OFFSET(pgno) (sizeof(DbFileHeader) + S_4K * ((pgno ) - 1))

typedef struct {
    unsigned char version;          // current: 1
    char dbSign[5];                 // string NMSqL
    unsigned int cookie;            // db cookie, init is 0
    unsigned int maxPageNo;         // max page no used, init is 0
    unsigned int availablePageNo;   // -> page1 -> page2 -> ...
} DbFileHeader;

int PagerWriteDbHeader(DbFileHeader *header);
int PagerWritePage(Pgno_t pgno, const Page_t *pg);
Page_t* PagerReadPage(Pgno_t pgno);

int OpenDbFile(const char* fileName);
int CloseDbFile();
int CreateDbFile(const char* fileName);
int WriteBPTreeMetaToPage(Pgno_t pgno, BPTreeMeta_t *bptMeta);

#endif
