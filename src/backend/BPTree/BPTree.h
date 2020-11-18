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

ReturnCode Search(btCursor*, key_t key);
ReturnCode search(btCursor*, key_t lowerKey, key_t upperKey);
ReturnCode insert(btCursor*, key_t key);
ReturnCode remove(btCursor*, key_t key);

private:
meta_t metaData;


};


#endif //BPTREE_BPTREE_H
