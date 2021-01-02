#ifndef _PAGERINTERFACE_H
#define _PAGERINTERFACE_H

#include "BTreeInterface.h"

BPTreeMeta_t *GetBPTreeMetaFromPage(Pgno_t);

// return 0 if success
int WriteBPTreeMetaToPage(Pgno_t, BPTreeMeta_t *);

Page_t *GetMemPage(Pgno_t);

// return 0 if success
int WritePage(Pgno_t, Page_t *);

Pgno_t CreatePage();

// return 0 if success
int DeletePage(Pgno_t)

#endif
