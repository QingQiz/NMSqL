//
// Created by 严愉程 on 2020/10/25.
//

#ifndef BPTREE_BPTREE_H
#define BPTREE_BPTREE_H

#include "predefined.h"

typedef int ReturnCode;

class BPTree {
public:

BPTree();
~BPTree();

ReturnCode create(char* fileName);
ReturnCode drop();
ReturnCode clear();

ReturnCode open(BtCursor*, Pgno_t metaPage);
ReturnCode close();

ReturnCode search(BtCursor*, Key_t key);
ReturnCode insert(BtCursor* cursor, Key_t key, void* pData, Size_t nData);
ReturnCode remove(BtCursor*);

ReturnCode first(BtCursor*); // move cursor to the first row
ReturnCode root(BtCursor*); // move cursor to the root page
ReturnCode next(BtCursor*); // move cursor to the next row

private:
BPTreeMeta_t metaData; // metaData of the BPTree
ReturnCode BPTreeSearch(BtCursor* cursor, Key_t key);


ReturnCode insertInNode(Pgno_t pgno, Key_t key, void* pData, Size_t nData);
ReturnCode splitNode(Pgno_t pgno, Pgno_t &newPageNo1, Pgno_t &newPageNo2);
ReturnCode deleteInNode(Pgno_t pgno, Key_t key); // delete the key in the node

};


#endif //BPTREE_BPTREE_H
