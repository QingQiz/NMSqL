//
// Created by 17110 on 2020/11/18.
//
#include "DBEngine.h"
#include "BTreeInterface.h"
#include "DbEngineInterface.h"
#include "DBEDefines.h"
#include <cstdlib>

/*
 * realization of DBEngine.h
 */

int findPageByName(const char *name) {
    return 1;
}

/*
 * realization of DbEngineInterface.h
 */

Cursor *open(const char *indexName, int flag) {
    if (flag == CURSOR_BTREE) {
        auto *btcursor = (btCursor *) malloc(sizeof(btCursor));
        auto rootPage = (pgno_t) findPageByName(indexName);
        BPTree bpTree;
        bpTree.open(btcursor, rootPage);
        auto *cursor = (Cursor *) malloc(sizeof(Cursor));
        cursor->cursor = (void *) btcursor;
        cursor->cursorType = CURSOR_BTREE;
        return cursor;
    } else {
        auto *cursor = (Cursor *) malloc(sizeof(Cursor));
        cursor->cursor = NULL;
        cursor->cursorType = CURSOR_LIST;
        return cursor;
    }
}

int close(Cursor *cursor) {
    if (cursor->cursorType == CURSOR_BTREE) {
        auto *btcursor = cursor->cursor;
        free(btcursor);
        return CLOSE_SUCCESS;
    } else {
        return CLOSE_FAILED;
    }
}

int create(const char *dbTable, const char *indexName, CursorType indexType,
           const int indexColumnCnt, const char **indexColumns) {}

int find(Cursor *cursor, const void *key) {
    if (cursor->cursorType == CURSOR_BTREE) {
        auto *btcursor = (btCursor *) cursor->cursor;
        BPTree bpTree;
        bpTree.search(btcursor, *((key_t *) key));
        return FIND_SUCCESS;
    } else {
        return FIND_FAILED;
    }
    //    TODO: requires new API.
}

void *getKey(Cursor *cursor) {
    /*
     * requires api from btree
     */
    if (cursor->cursorType == CURSOR_BTREE) {
        return NULL;
    } else {
        return NULL;
    }
}

void *getValue(Cursor *cursor) {
    /*
     * requires api from pager
     */
}

int insert(Cursor *cursor, const void *key, const void *value) {
    if (cursor->cursorType == CURSOR_BTREE) {
        auto *btcursor = cursor->cursor;
        BPTree bpTree;
        return INSERT_SUCCESS;
    } else {
        return INSERT_FAILED;
    }
    //    TODO:API requires re-define.
}

int erase(Cursor *cursor) {
    if (cursor->cursorType == CURSOR_BTREE) {
        auto *btcursor = cursor->cursor;
        BPTree bpTree;
        return ERASE_SUCCESS;
    } else {
        return ERASE_FAILED;
    }
    //    TODO:API requires re-define.
}

int next(Cursor *cursor) {}

int reset(Cursor *cursor) {}


int createTable(const char *sql) {}

int reorganize() {}

void *getMetaData(const char *tableName) {}

int getCookies() {}

char **getTableColumns(const char *tableName) {}

/*
 * 事务相关
 */

int transaction() {}

int commit() {}

int rollback() {}

/*******************************************************************/
