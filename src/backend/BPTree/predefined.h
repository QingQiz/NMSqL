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
typedef int ReturnCode;

struct Address_t{
    Pgno_t pgno;
    Offset_t offset;
    Address_t(Pgno_t no, Offset_t offset):pgno(no), offset(offset){}
    Address_t(){}
    bool operator==(const Address_t b){
        return this->pgno == b.pgno && this->offset == b.offset;
    }
};

const Address_t NULLAddress = Address_t(0, 0); // illegel address
const Pgno_t NULLPgno = 0; // illegel pgno

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

bool operator>=(const Key_t &l, const Key_t &r){
    return keycmp(l, r) >= 0;
}

/* meta information of B+ tree */
struct BPTreeMeta_t{
    int maxDegree; // maximun capacity of the node
    Size_t page_header_size; // size of page header
    Size_t cell_size; // size of the cell
    Size_t page_size;
    Size_t cellPointer_size; // 
    Size_t internal_node_num;
    Size_t leaf_node_num;
    Size_t height;
    // Address_t slot; // where is the place to insert data
    Pgno_t root; // where is the root of B+Tree
    Address_t first; // where is the first of data row。
};

union Data_t{ // data stored in the cell
    void* pData; // used in leaf node 
    Pgno_t childAddress; // used in internal node
    Data_t(){}
    Data_t(void* pData):pData(pData){}
    Data_t(Pgno_t address):childAddress(address){} // NOTE：根据输入类型的不同，调用不同的构造函数。之前没这么用过，不知道这样可不可以。
};


struct Record_t{
    Key_t key;
    bool isDelete;
    Address_t address; // if leaf node offset of the cell pointer
                       // if internal node offset of the leaf node
    Offset_t offset; // offset of the data
    Size_t nData; // size of the data
    Data_t data;
    Record_t(){}
    Record_t(Key_t key, Address_t address, Offset_t offset, Size_t nData, Data_t data):key(key), isDelete(0), address(address), offset(offset), nData(nData), data(data){}
};

struct Node_t {
    bool isLeaf;
    Address_t parent; // parent index offset
    Pgno_t address;
    Pgno_t next;
    Pgno_t prev;
    std::vector<Record_t> records; // records array
};

struct BtCursor{
    Address_t address;
    Pgno_t rootPage;
    int id; //用于表示是cell pointer array中的第几个。从0开始。

};


#endif //BPTREE_PREDEFINED_H
