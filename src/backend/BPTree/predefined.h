//
// Created by 严愉程 on 2020/10/27.
//

#ifndef BPTREE_PREDEFINED_H
#define BPTREE_PREDEFINED_H

#include <string.h>

typedef int pgno_t;
typedef int offset_t;
typedef struct address_t{
    pgno_t pgno;
    offset_t offset;
};

struct key_t{
    char data[16];
    key_t(const char *str = ""){
        bzero(data, sizeof(data));
        strcpy(data, str);
    }
};

inline int keycmp(const key_t &a, const key_t &b) {
    int x = strlen(a.data) - strlen(b.data);
    return x == 0 ? strcmp(a.data, b.data) : x;
}

#define OPERATOR_KEYCMP(type) \
    bool operator< (const key_t &l, const type &r) {\
        return keycmp(l, r.key) < 0;\
    }\
    bool operator< (const type &l, const key_t &r) {\
        return keycmp(l.key, r) < 0;\
    }\
    bool operator== (const key_t &l, const type &r) {\
        return keycmp(l, r.key) == 0;\
    }\
    bool operator== (const type &l, const key_t &r) {\
        return keycmp(l.key, r) == 0;\
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
};

struct internal_node_t{
    address_t parent;
    address_t next;
    address_t prev;
    size_t child_num; 
    index_t children[10]; // children array, dynamic memory allocation
};

struct record_t{
    key_t key;
    address_t data; // offset of the data
    int size;
};

struct leaf_node_t {
    address_t parent; // parent node offset
    address_t next;
    address_t prev;
    size_t record_num;
    record_t records[10]; // records array, dynamic memory allocation
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

#endif //BPTREE_PREDEFINED_H
