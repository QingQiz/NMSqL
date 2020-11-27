//
// Created by 严愉程 on 2020/10/25.
//

#ifndef BPTREE_BPTREE_H
#define BPTREE_BPTREE_H

#include "predefined.h"

typedef int ReturnCode;

class BPTree {
public:

ReturnCode create(char* fileName);
ReturnCode drop();
ReturnCode clear();

ReturnCode open(btCursor*, pgno_t root_page);
ReturnCode close();

ReturnCode search(btCursor*, key_t key);
ReturnCode insert(btCursor*, key_t key, void* data);
ReturnCode remove(btCursor*, key_t key);

ReturnCode first(btCursor*); // move cursor to the first row
ReturnCode root(btCursor*); // move cursor to the root page
ReturnCode next(btCursor*); // move cursor to the next row

private:
meta_t metaData;


};


#endif //BPTREE_BPTREE_H
