#ifndef _BTREEINTERFACE_H
#define _BTREEINTERFACE_H

typedef int Pgno_t;
typedef int Offset_t;
typedef unsigned int Size_t;
typedef int ReturnCode;

typedef struct {
    Pgno_t pgno;
    Offset_t offset;
} Address_t;

typedef struct {
    Size_t size;
    void *data;
} Key_t;

/* meta information of B+ tree */
typedef struct {
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
} BPTreeMeta_t;

typedef union {            // data stored in the cell
    void *pData;          // used in leaf node
    Pgno_t childAddress;  // used in internal node
} Data_t;

typedef struct {
    Key_t key;
    unsigned char isDelete;
    Address_t address;  // if leaf node offset of the cell pointer
                        // if internal node offset of the leaf node
    Offset_t offset;    // offset of the data
    Size_t nData;       // size of the data
    Data_t data;
} Record_t;

typedef struct {
    unsigned char isLeaf;
    Address_t parent;  // parent index offset
    Pgno_t address;
    Pgno_t next;
    Pgno_t prev;
    Size_t record_size;
    Record_t *records;  // records array
} Page_t;

typedef struct {
    Address_t address;
    Pgno_t rootPage;
    int id;  //用于表示是cell pointer array中的第几个。从0开始。
} BtCursor;

#endif
