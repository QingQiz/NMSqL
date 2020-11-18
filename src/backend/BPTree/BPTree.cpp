//
// Created by 严愉程 on 2020/10/25.
//

#include "BPTree.h"
#include "../include/PagerInterface.h"

ReturnCode move2root(btCursor* cursor){
    // move cursor to the root
    cursor->address = address_t(cursor->rootPage, 0);
}

ReturnCode search(btCursor* cursor, key_t key){
    move2root(cursor);
    BPTreeSearch(cursor, key); //从root开始搜索
}

ReturnCode BPTreeSearch(btCursor* cursor, key_t key){ //在该节点中进行搜索
    internal_node_t* node = (internal_node_t*)getMemPage(cursor->rootPage); //get the node the cursor pointed to
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
                continue;
            }
        }
        return isFound;
    }

}