//
// Created by 严愉程 on 2020/10/27.
//

#ifndef BPTREE_PREDEFINED_H
#define BPTREE_PREDEFINED_H

#include <string.h>

typedef int pgno_t;
typedef int offset_t;
//typedef unsigned int size_t;
typedef struct address_t{
    pgno_t pgno;
    offset_t offset;
    address_t(pgno_t no, offset_t offset){
        this->pgno = no;
        this->offset = offset;
    }
    bool operator==(const address_t b){
        return this->pgno == b.pgno && this->offset == b.offset;
    }
};

const address_t NULLAddress = address_t(0, 0); // 这是一个不该出现的地址

struct key_t{
    char data[16];
    key_t(const char *str = ""){
        strcpy(data, str);
    }
};

inline int keycmp(const key_t &a, const key_t &b) {
    int x = strlen(a.data) - strlen(b.data);
    return x == 0 ? strcmp(a.data, b.data) : x;
}

bool operator<(const key_t &l, const key_t &r){
    return keycmp(l, r) < 0;
}

bool operator>(const key_t &l, const key_t &r){
    return keycmp(l, r) > 0;
}

bool operator==(const key_t &l, const key_t &r){
    return keycmp(l, r) == 0;
}

bool operator<=(const key_t &l, const key_t &r){
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
    address_t slot; // where is the place to insert data
    address_t root; // where is the root of B+Tree
    address_t first; // where is the first of leaf node
};

struct index_t {
    key_t key;
    address_t child; // offset of the cell pointer
    // NOTE:如果是index的话应该不用考虑下面三个字段。
    int offset; // offset of the data
    size_t nData; // size of the data
    void* pData; // data in the memory

};

struct internal_node_t{
    bool isLeaf;
    address_t parent;
    address_t next;
    address_t prev;
    size_t child_num; 
    index_t *children; // children array, dynamic memory allocation
};

struct record_t{
    key_t key;
    address_t address; // offset of the cell pointer
    int offset; // offset of the data (NOTE: BPTree的阶段并不进行offset的维护，写回的时候，由pager来维护)
    size_t nData; // size of the data
    void* pData; // data in the memory
};

struct leaf_node_t {
    bool isLeaf;
    address_t parent; // parent node offset
    address_t next;
    address_t prev;
    size_t record_num;
    record_t *records; // records array, dynamic memory allocation (NOTE：内存分配的时候应该比record_num多分配一个)
};

struct btCursor{
    address_t address;
    pgno_t rootPage;
    int id; //用于表示是cell pointer array中的第几个。从0开始。

};

/*
 * 
 */
struct MemPage{
    bool isLeaf; //True if is leaf node

};

#endif //BPTREE_PREDEFINED_H
