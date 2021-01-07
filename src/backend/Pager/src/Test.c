#include "File.h"
#include "Pager.h"
#include <stdlib.h>
#include <string.h>


int main() {
    int res = CreateDbFile("asdxxx");
    if (res) {
        OpenDbFile("asdxxx");
    }

    Page_t pg;
    pg.address = 1;
    pg.isLeaf = 1;
    pg.next = 2;
    pg.prev = 3;
    pg.record_size = 2;
    pg.records = malloc(pg.record_size * sizeof(Record_t));

    Address_t addr;

    addr.pgno = 10;
    addr.offset = 100;

    pg.parent = addr;

    Record_t rec;

    rec.address = addr;
    rec.isDelete = 0;
    rec.key.size = 10;
    rec.key.data = malloc(10);
    strncpy(rec.key.data, "asdkjalksdajskldj", 10);
    rec.offset = 10;
    rec.nData = 10;
    rec.data.pData = malloc(10);
    strncpy(rec.data.pData, "sljksacflkjasdjlk", 10);

    pg.records[0] = rec;
    pg.records[1] = rec;

    int pgno = CreatePage();
    PagerWritePage(pgno, &pg);


    Page_t* test = PagerReadPage(pgno);

    int a = memcmp(test, &pg, 40);
    printf("%d", a);
    CloseDbFile();
}

