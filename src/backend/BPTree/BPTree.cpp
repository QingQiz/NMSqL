//
// Created by 严愉程 on 2020/10/25.
//

#include "BPTree.h"
#include "../include/PagerInterface.h"

// move cursor to the root page
ReturnCode BPTree::root(btCursor* cursor){
    cursor->address = address_t(cursor->rootPage, 0);
}

// ReturnCode 0:not find;
//            1:find and move the cursor to the row
ReturnCode BPTree::search(btCursor* cursor, key_t key){
    root(cursor);
    return BPTreeSearch(cursor, key);
}

// ReturnCode 0:not find;
//            1:find and move the cursor to the row
ReturnCode BPTreeSearch(btCursor* cursor, key_t key){ //seach in the node to which the cursor pointed
    internal_node_t* node = (internal_node_t*)getMemPage(cursor->address.pgno); //get the node the cursor pointed to
    if(!node->isLeaf){
        for(int i = 0; i < node->child_num; i++){
            index_t index = node->children[i];
            if(key <= index.key){
                cursor->address = index.child;
                return BPTreeSearch(cursor, key);
                continue;
            }
        }
    }
    else{
        leaf_node_t* leaf_node = (leaf_node_t*)node;
        bool isFound = 0;
        for(int i = 0; i < leaf_node->record_num; i++){
            record_t record = leaf_node->records[i];
            if(key == record.key){
                cursor->address = record.address;
                isFound = 1;
                break;
            }
            else if(key > record.key){
                isFound = 0;
                break;
            }
        }
        return isFound;
    }
}

// move cursor to the first row
ReturnCode BPTree::first(btCursor* cursor){
    cursor->address = metaData.first; // NOTE:不要忘了维护BTree metaData中的first
    return 1;
}

// move cursor to the next row
// ReturnCode 0:end of the list
//            1:SUCCESS
ReturnCode BPTree::next(btCursor* cursor){
    leaf_node_t* leaf_node = (leaf_node_t*)getMemPage(cursor->address.pgno);
    if(cursor->id < leaf_node->record_num-1){
        cursor->id++;
        cursor->address = leaf_node->records[cursor->id].address;
    }
    else{ // next row has been stored in the next leaf node;
        if(leaf_node->next == NULLAddress) return 0; // reached the end of list
        leaf_node_t* next_leaf_node = (leaf_node_t*)getMemPage(leaf_node->next.pgno);
        cursor->id = 0;
        cursor->address = next_leaf_node->records[0].address;
    }
    return 1;
}

ReturnCode insert(btCursor* cursor, key_t key, void* data){
    

}


//******************* Node related operation *****************************

