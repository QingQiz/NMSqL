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

ReturnCode open(pgno_t root_page);
ReturnCode close();

ReturnCode search(key_t key);
ReturnCode search(key_t lowerKey, key_t upperKey);
ReturnCode inseart(key_t key);
ReturnCode remove(key_t key);

private:
meta_t metaData;


};


#endif //BPTREE_BPTREE_H
