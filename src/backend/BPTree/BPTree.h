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

ReturnCode open(btCursor*, pgno_t metaPage);
ReturnCode close();

ReturnCode search(btCursor*, key_t key);
ReturnCode insert(btCursor* cursor, key_t key, void* pData, size_t nData);
ReturnCode remove(btCursor*);

ReturnCode first(btCursor*); // move cursor to the first row
ReturnCode root(btCursor*); // move cursor to the root page
ReturnCode next(btCursor*); // move cursor to the next row

private:
BPTreeMeta_t metaData; // metaData of the BPTree
ReturnCode BPTreeSearch(btCursor* cursor, key_t key);


ReturnCode insertInNode(pgno_t pgno, key_t key, void* pData, size_t nData);
ReturnCode splitNode(pgno_t pgno, pgno_t &newPageNo1, pgno_t &newPageNo2);
ReturnCode deleteInNode(pgno_t pgno, key_t key); // delete the key in the node

};


#endif //BPTREE_BPTREE_H
