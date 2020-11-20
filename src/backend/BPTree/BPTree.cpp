//
// Created by 严愉程 on 2020/10/25.
//

#include "BPTree.h"
#include "../include/PagerInterface.h"

ReturnCode BPTree::root(btCursor* cursor){
    // move cursor to the root
    cursor->address = address_t(cursor->rootPage, 0);
}

//return 0:not find; 1:find and move the cursor to the data 
ReturnCode BPTree::search(btCursor* cursor, key_t key){
    root(cursor);
    return BPTreeSearch(cursor, key);
}

//return 0:not find; 1:find and move the cursor to the data 
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
                cursor->address = record.data;
                isFound = 1;
                break;
            }
        }
        return isFound;
    }
}

ReturnCode BPTree::search(btCursor* cursor, key_t lowerKey, key_t upperKey){
    search(cursor, lowerKey);

}


// move the cursor to the first leaf node
ReturnCode BPTree::first(btCursor* cursor){
    cursor->address = metaData.first;
    return 1;
}

// move the cursor to the next leaf node
ReturnCode BPTree::next(btCursor* cursor){
    leaf_node_t* leaf_node = (leaf_node_t*)getMemPage(cursor->address.pgno);
    cursor->address = leaf_node->next;
    return 1;
}

ReturnCode insert(btCursor* cursor, key_t key, void* data){
    


}
