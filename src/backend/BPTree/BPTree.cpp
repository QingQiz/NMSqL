//
// Created by 严愉程 on 2020/10/25.
//

#include "BPTree.h"
#include "../include/PagerInterface.h"
#include <algorithm>

// move cursor to the root page
ReturnCode BPTree::root(BtCursor* cursor){
    cursor->address = Address_t(this->metaData.root, 0);
    cursor->rootPage = this->metaData.root;
    cursor->id = 0;
}

// ReturnCode 0:not find;
//            1:find and move the cursor to the row
ReturnCode BPTree::search(BtCursor* cursor, Key_t key){
    root(cursor);
    return BPTreeSearch(cursor, key);
}

// start from the root page, find the key node-by-node recursively.
// ReturnCode 0:not find;
//            1:find and move the cursor to the row
ReturnCode BPTree::BPTreeSearch(BtCursor* cursor, Key_t key){ //seach in the node to which the cursor pointed
    Node_t* node = (Node_t*)getMemPage(cursor->address.pgno); //get the node the cursor pointed to
    if(!node->isLeaf){ // search in internal node
        for(int i = 0; i < node->records.size(); i++){
            Record_t record = node->records[i];
            if( (key <= record.key) || (i == node->records.size() - 1 )){ // find the fisrt index bigger than key, or use the last index
                cursor->address = Address_t(record.data.childAddress, 0);
                return BPTreeSearch(cursor, key);
                break; //NOTE: 对于重复key的情况，只要找到key所在的第一个位置就行。
            }
        }
    }
    else{ // search in leaf node
        bool isFound = 0;
        for(int i = 0; i < node->records.size(); i++){
            Record_t record = node->records[i];
            if(key == record.key){
                cursor->address = record.address;
                cursor->id = i;
                if(!record.isDelete) isFound = 1;
                break;
            }
            else if(key > record.key){
                // in the current leaf node, find someone big than the key -> meas the key is not in the tree.
                isFound = 0;
                // for the convenience of insert operation, move the key to the node where it should be inserted
                break;
            }
        }
        return isFound;
    }
}

// move cursor to the first data row
ReturnCode BPTree::first(BtCursor* cursor){
    cursor->address = metaData.first;
    cursor->id = 0;
    return 1;
}

// move cursor to the next row
// ReturnCode 0:end of the list
//            1:SUCCESS
ReturnCode BPTree::next(BtCursor* cursor){
    Node_t* node = (Node_t*)getMemPage(cursor->address.pgno);

    if(cursor->id < node->records.size() - 1){
        cursor->id++;
        cursor->address = node->records[cursor->id].address;
    }
    else{ // next row has been stored in the next leaf node;
        if(node->next == NULLAddress.pgno) return 0; // reached the end of list
        Node_t* node = (Node_t*)getMemPage(node->next);
        cursor->id = 0;
        cursor->address = node->records[0].address;
    }

    if(node->records[cursor->id].isDelete == 1) return next(cursor);
    else return 1;
}

ReturnCode BPTree::insert(BtCursor* cursor, Key_t key, void* pData, Size_t nData){
    int rc = search(cursor, key); // find the leaf node where the key should be inserted
    rc = insertInLeafNode(cursor->address.pgno, key, pData, nData);
    return rc;
}

// soft delete, than move the cursor to the next data row
ReturnCode BPTree::remove(BtCursor* cursor){
    Node_t* node = (Node_t*)getMemPage(cursor->address.pgno);
    int id = cursor->id;
    node->records[id].isDelete = 1;
    next(cursor);
    return 1;
}


//******************* Node related operation *****************************

// return 1 : success
//        0 : can't find the key failed
ReturnCode BPTree::deleteInNode(BtCursor* cursor){
    Node_t* node = (Node_t*)getMemPage(cursor->address.pgno);
    node->records.erase(node->records.begin()+cursor->id);
    if(node->records.size() < this->metaData.maxDegree / 2){ // node does not have enough records
        Node_t* leftNode = (Node_t*)getMemPage(node->prev);
        Node_t* rightNode = (Node_t*)getMemPage(node->next);
        if(halfMoreRecords(node->prev) && node->parent.pgno == leftNode->parent.pgno){ // try borrow from left node
            Record_t temp = leftNode->records.back();
            leftNode->records.erase(leftNode->records.end());
            node->records.insert(node->records.begin(), temp);
            updateParentIndex(leftNode->parent, leftNode->records.back().key);
        }
        else if(halfMoreRecords(node->next) && node->parent.pgno == rightNode->parent.pgno){ // try borrow from right node
            Record_t temp = rightNode->records.front();
            rightNode->records.erase(rightNode->records.begin());
            node->records.push_back(temp);
            updateParentIndex(node->parent, node->records.back().key);
        }
        else{ // merge with brother node
            // NOTE：左右兄弟都没有多余record的情况，需要进行合并。
            if(leftNode->parent.pgno == node->parent.pgno){
                // NOTE：和左边兄弟节点进行合并
                // NOTE：这里的删除操作就不用考虑，删除完之后将cursor移动到下一个data row了。
                leftNode->records.insert(leftNode->records.end(), node->records.begin(), node->records.end());
                leftNode->next = node->next;
                deletePage(node->address);
                

            }
            else if(rightNode->parent.pgno == node->parent.pgno){
                // NOTE：和右边兄弟节点进行合并

            }
            else return 0;
        }
    }
    else{ // delete the key directly
        if(cursor->id == node->records.size()){
            updateParentIndex(node->parent, node->records.back().key);
        }
        reArrangeNode(node->address);
    }
}

// insert key to the leaf node
ReturnCode BPTree::insertInLeafNode(Pgno_t pgno, Key_t key, void* pData, Size_t nData){
    Node_t* node = (Node_t*)getMemPage(pgno);

    bool needToSpilt = (node->records.size() == this->metaData.maxDegree);

    if(key >= node->records.back().key){
        // if inserting key is no lees than the last key in node, insert the end of the array
        Record_t newRecord(key, NULLAddress, 0, nData, Data_t(pData));
        node->records.push_back(newRecord);
        reArrangeNode(node->address);
        if(needToSpilt){
            splitNode(node->address); // TODO
        }
        else{
            updateParentIndex(node->parent, key); // NOTE: 本节点的最后一个key发生了变化，更新父亲节点的分割key
        }
    }
    else{
        // else find the fisrt key in node bigger than the inserting key
        for(int i = 0; i < node->records.size(); i++){
            if(key < node->records[i].key){ // 
                Record_t newRecord(key, NULLAddress, 0, nData, Data_t(pData));
                node->records.insert(node->records.begin() + i, newRecord);
                reArrangeNode(node->address);
                break;
            }
        }
        if(needToSpilt){ 
            splitNode(node->address); // TODO
        }
    }
    return 1;
}

ReturnCode BPTree::insertInInternalNode(Pgno_t internalPgno, Key_t key, Node_t* childNode){
    Node_t* node = (Node_t*)getMemPage(internalPgno);

    bool needToSplit = (node->records.size() == this->metaData.maxDegree);

    if(key >= node->records.back().key){
        Record_t newRecord(key, NULLAddress, 0, sizeof(Address_t), Data_t(childNode->address));
        node->records.push_back(newRecord);
        reArrangeNode(node->address);
        if(needToSplit){
            splitNode(node->address);
        }
        else updateParentIndex(node->parent, key); // NOTE: 本节点的最后一个key发生了变化，更新父亲节点的分割key
    }
    else{
        // else find the first key in node bigger than the inserting key
        for(int i = 0; i < node->records.size(); i++){
            if(key < node->records[i].key){
                Record_t newRecord(key, NULLAddress, 0, sizeof(Address_t), Data_t(childNode->address));
                node->records.insert(node->records.begin() + i, newRecord);
                reArrangeNode(node->address);
                break;
            }
        }
        if(needToSplit){
            splitNode(node->address); // TODO
        }
    }
}

// split internal node or leaf node
ReturnCode BPTree::splitNode(Pgno_t pgno){
    Node_t* node = (Node_t*)getMemPage(pgno);
    Node_t* newNode = (Node_t*)createNewPage(this->metaData.page_size);

    newNode->isLeaf = node->isLeaf;
    newNode->next = node->next;
    // newNode->parent = ; // 调用insertInInternalNode之后进行更新。
    newNode->prev = node->address;
    node->next = newNode->address;
    
    int splitPosition = node->records.size() / 2;
    std::copy(node->records.begin() + splitPosition, node->records.end(), newNode->records.begin());
    node->records.erase(node->records.begin());

    if(isRootPage(pgno)){ // if need to split the root page
        Node_t* newRoot = (Node_t*)createNewPage(this->metaData.page_size);
        newRoot->isLeaf = 0;
        newRoot->next = NULLPgno;
        newRoot->prev = NULLPgno;
        this->metaData.root = newRoot->address;
    }
    else{
        updateParentIndex(node->parent, node->records.back().key);
        insertInInternalNode(node->parent.pgno, newNode->records.back().key, newNode);
    }
}


// 修改指向某个子节点的父节点中的索引。
ReturnCode BPTree::updateParentIndex(Address_t address, Key_t newKey){
    Node_t* node = (Node_t*)getMemPage(address.pgno);
    int id = getIdInCellPointArray(address);
    node->records[id].key = newKey;
    if(id == (node->records.size() - 1) && !isRootPage(address.pgno)){
        updateParentIndex(node->parent, newKey); // update parent index recursively
    }
    return 1;
}

int BPTree::getIdInCellPointArray(Address_t address){
    int id = (address.offset - this->metaData.page_header_size) / this->metaData.cellPointer_size;
    return id;
}


ReturnCode BPTree::reArrangeNode(Pgno_t pgno){
    Node_t* node = (Node_t*)getMemPage(pgno);
    for(int i = 0; i < node->records.size(); i++){
        node->records[i].address.pgno = pgno;
        node->records[i].address.offset = this->metaData.page_header_size + i * this->metaData.cellPointer_size;
    }
    return 1;
}

ReturnCode BPTree::isRootPage(Pgno_t pgno){
    return pgno == this->metaData.root;
}

ReturnCode BPTree::halfMoreRecords(Pgno_t pgno){
    if(pgno == NULLPgno) return 0;
    Node_t* node = (Node_t*)getMemPage(pgno);
    return node->records.size() > this->metaData.maxDegree / 2;
}

