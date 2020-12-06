//
// Created by 严愉程 on 2020/10/25.
//

#ifndef BPTREE_BPTREE_H
#define BPTREE_BPTREE_H

#include "predefined.h"


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


ReturnCode insertInLeafNode(Pgno_t leaf_pgno, Key_t key, void* pData, Size_t nData);
ReturnCode insertInInternalNode(Pgno_t internal_pgno, Key_t key, Node_t* childNode);
ReturnCode splitNode(Pgno_t pgno);
ReturnCode updateParentIndex(Address_t parentAddress, Key_t newKey); // 修改指向某个子节点的父节点的索引。
ReturnCode deleteInNode(Pgno_t pgno, Key_t key); // delete the key in the node
ReturnCode reArrangeNode(Pgno_t pgno); // after insert, spilt or remove operation neet to rearrange the node
ReturnCode isRootPage(Pgno_t pgno);

int getIdInCellPointArray(Address_t address); // 

};


#endif //BPTREE_BPTREE_H
