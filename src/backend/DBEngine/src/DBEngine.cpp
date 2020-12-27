//
// Created by 17110 on 2020/11/18.
//
#include "DBEngine.h"
#include "BTreeInterface.h"
#include "DbEngineInterface.h"
#include "DBEDefines.h"
#include <cstdlib>

using std::string;

string extractString(const char *st) {
    string s;
    int len = st[1] + st[0] * 256;
    for (int i = 0; i < len; i++) {
        s += st[4 + i];
    }
    return s;
}

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
    string idxName = extractString(indexName);
    if (flag == CURSOR_BTREE) {
        auto *btcursor = (btCursor *) malloc(sizeof(btCursor));
        auto rootPage = (pgno_t) findPageByName(idxName.c_str());
        BPTree bpTree;
        bpTree.open(btcursor, rootPage);
        auto *cursor = (Cursor *) malloc(sizeof(Cursor));
        cursor->cursor = (void *) btcursor;
        cursor->cursorType = CURSOR_BTREE;
        bpTree.close();
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
    key_t k(extractString((const char *) key).c_str());
    if (cursor->cursorType == CURSOR_BTREE) {
        auto *btcursor = (btCursor *) cursor->cursor;
        BPTree bpTree;
        bpTree.search(btcursor, k);
        bpTree.close();
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
    if (cursor->cursorType == CURSOR_BTREE) {
        return NULL;
    } else {
        return NULL;
    }
}

int insert(Cursor *cursor, const void *key, const void *value) {
    key_t k(extractString((const char *) key).c_str());
    string data = extractString((const char *) value);
    if (cursor->cursorType == CURSOR_BTREE) {
        auto *btcursor = (btCursor *) cursor->cursor;
        BPTree bpTree;
        bpTree.insert(btcursor, *((key_t *) key), (void *) data.c_str());
        return INSERT_SUCCESS;
    } else {
        return INSERT_FAILED;
    }
}

int erase(Cursor *cursor) {
    if (cursor->cursorType == CURSOR_BTREE) {
        auto *btcursor = (btCursor *) cursor->cursor;
        BPTree bpTree;
        bpTree.remove(btcursor);
        bpTree.close();
        return ERASE_SUCCESS;
    } else {
        return ERASE_FAILED;
    }
}

int next(Cursor *cursor) {
    if (cursor->cursorType == CURSOR_BTREE) {
        auto *btcursor = (btCursor *) cursor->cursor;
        BPTree bpTree;
        bpTree.next(btcursor);
        bpTree.close();
        return NEXT_SUCCESS;
    } else {
        return NEXT_FAILED;
    }
}

int reset(Cursor *cursor) {
    if (cursor->cursorType == CURSOR_BTREE) {
        auto *btcursor = (btCursor *) cursor->cursor;
        BPTree bpTree;
        bpTree.first(btcursor);
        bpTree.close();
        return RESET_SUCCESS;
    } else {
        return RESET_FAILED;
    }
}


int createTable(const char *sql) {
    string data = extractString((sql));
    data = data.substr(13);
    BPTree bpTree;
    auto ret = bpTree.create((char *) data.c_str());
    return (int) ret;
}

int reorganize() {
    return REORGANIZE_FAILED;
}

void *getMetaData(const char *tableName) {
    return NULL;
}

int getCookies() {
    return 0;
}

int setCookies(int cookies) {
    return SET_COOKIES_FAILED;
}

char **getTableColumns(const char *tableName) {
    // 好像没有这个接口
    return NULL;
}

/*
 * 事务相关
 */

int transaction() {}

int commit() {}

int rollback() {}

/*******************************************************************/
