#ifndef _BTREEINTERFACE_H
#define _BTREEINTERFACE_H

typedef int Pgno_t;
typedef int Offset_t;
typedef unsigned int Size_t;
typedef int ReturnCode;

struct Address_t {
    Pgno_t pgno;
    Offset_t offset;
};

struct Key_t {
    char data[16];
};

/* meta information of B+ tree */
struct BPTreeMeta_t {
    int maxDegree;            // maximun capacity of the node
    Size_t page_header_size;  // size of page header
    Size_t cell_size;         // size of the cell
    Size_t page_size;
    Size_t cellPointer_size;  //
    Size_t internal_node_num;
    Size_t leaf_node_num;
    Size_t height;
    // Address_t slot; // where is the place to insert data
    Pgno_t root;      // where is the root of B+Tree
    Address_t first;  // where is the first of data row。
};

union Data_t {            // data stored in the cell
    void *pData;          // used in leaf node
    Pgno_t childAddress;  // used in internal node
};

struct Record_t {
    Key_t key;
    bool isDelete;
    Address_t address;  // if leaf node offset of the cell pointer
                        // if internal node offset of the leaf node
    Offset_t offset;    // offset of the data
    Size_t nData;       // size of the data
    Data_t data;
};

struct Page_t {
    bool isLeaf;
    Address_t parent;  // parent index offset
    Pgno_t address;
    Pgno_t next;
    Pgno_t prev;
    Size_t record_size;
    Record_t *records;  // records array
};

struct BtCursor {
    Address_t address;
    Pgno_t rootPage;
    int id;  //用于表示是cell pointer array中的第几个。从0开始。
};

#endif
