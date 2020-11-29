#ifndef _BTREEINTERFACE_H
#define _BTREEINTERFACE_H

#include <string.h>

typedef int pgno_t;
typedef int offset_t;
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
struct meta_t{
    int capacity; // capacity of the node
    int value_size; // size of the value
    int key_size; // size of the key
    int internal_node_num;
    int leaf_node_num;
    int height;
    address_t slot; // where is the place to insert data
    address_t root; // where is the root of B+Tree
    address_t first; // where is the first of leaf
};

struct index_t {
    key_t key;
    address_t child; // offset of child's node
    int size;
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
    address_t data; // offset of the data
    int size;
};

struct leaf_node_t {
    bool isLeaf;
    address_t parent; // parent node offset
    address_t next;
    address_t prev;
    size_t record_num;
    record_t *records; // records array, dynamic memory allocation
};

struct btCursor{
    address_t address;
    pgno_t rootPage;

};

/*
 * 
 */
struct MemPage{
    bool isLeaf; //True if is leaf node

};


typedef int ReturnCode;

class BPTree {
public:

BPTree();
~BPTree();

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


};

#endif
