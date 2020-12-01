//
// Created by 严愉程 on 2020/10/27.
//

#ifndef BPTREE_PREDEFINED_H
#define BPTREE_PREDEFINED_H

#include <string.h>
#include <vector>

typedef int Pgno_t;
typedef int Offset_t;
typedef unsigned int Size_t;
struct Address_t{
    Pgno_t pgno;
    Offset_t offset;
    Address_t(Pgno_t no, Offset_t offset):pgno(no), offset(offset){}
    Address_t(){}
    bool operator==(const Address_t b){
        return this->pgno == b.pgno && this->offset == b.offset;
    }
};

const Address_t NULLAddress = Address_t(0, 0); // 这是一个不该出现的地址

struct Key_t{
    char data[16];
    Key_t(const char *str = ""){
        strcpy(data, str);
    }
};

inline int keycmp(const Key_t &a, const Key_t &b) {
    int x = strlen(a.data) - strlen(b.data);
    return x == 0 ? strcmp(a.data, b.data) : x;
}

bool operator<(const Key_t &l, const Key_t &r){
    return keycmp(l, r) < 0;
}

bool operator>(const Key_t &l, const Key_t &r){
    return keycmp(l, r) > 0;
}

bool operator==(const Key_t &l, const Key_t &r){
    return keycmp(l, r) == 0;
}

bool operator<=(const Key_t &l, const Key_t &r){
    return keycmp(l, r) <= 0;
}


/* meta information of B+ tree */
struct BPTreeMeta_t{
    int capacity; // capacity of the node
    int value_size; // size of the value
    int key_size; // size of the key
    int internal_node_num;
    int leaf_node_num;
    int height;
    Address_t slot; // where is the place to insert data
    Address_t root; // where is the root of B+Tree
    Address_t first; // where is the first of leaf node
};


struct Record_t{
    Key_t key;
    Address_t address; // offset of the cell pointer
    int offset; // offset of the data (NOTE: BPTree的阶段并不进行offset的维护，写回的时候，由pager来维护)
    Size_t nData; // size of the data
    void* pData; // data in the memory
};

struct Node_t {
    bool isLeaf;
    Address_t parent; // parent node offset
    Address_t next;
    Address_t prev;
    // Size_t record_num;
    // Record_t *records; // records array, dynamic memory allocation (NOTE：内存分配的时候应该比record_num多分配一个)
    std::vector<Record_t> records; // records array
};

struct BtCursor{
    Address_t address;
    Pgno_t rootPage;
    int id; //用于表示是cell pointer array中的第几个。从0开始。

};

/*
 * 
 */
struct MemPage{
    bool isLeaf; //True if is leaf node

};

#endif //BPTREE_PREDEFINED_H
