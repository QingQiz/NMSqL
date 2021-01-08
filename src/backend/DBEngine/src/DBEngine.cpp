//
// Created by 17110 on 2020/11/18.
//
#include "DBEngine.h"
#include "BTreeInterface.h"
#include "DbEngineInterface.h"
#include "DBEDefines.h"
#include "BPTree.h"
#include "predefined.h"
#include "PagerInterface.h"
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

int getRowSize(char *tableName) {
    return 1;
}

/*
 * realization of DbEngineInterface.h
 */

Cursor *cursorOpen(int transactionId, const char *indexName, int flag) {
    string idxName = extractString(indexName);
    if (flag == CURSOR_BTREE) {
        auto *btcursor = (BtCursor *) malloc(sizeof(BtCursor));
        auto rootPage = (pgno_t) findPageByName(idxName.c_str());
        auto *dbecursor = (DBECursor *) malloc(sizeof(DBECursor));\
        dbecursor->cursor = btcursor;
        dbecursor->key = idxName;
        BPTree bpTree;
        bpTree.open(btcursor, rootPage);
        auto *cursor = (Cursor *) malloc(sizeof(Cursor));
        cursor->cursor = (void *) dbecursor;
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

int cursorClose(int transactionId, Cursor *cursor) {
    if (cursor->cursorType == CURSOR_BTREE) {
        auto dbecursor = (DBECursor *) cursor->cursor;
        free(dbecursor->cursor);
        free(dbecursor);
        return CLOSE_SUCCESS;
    } else {
        return CLOSE_FAILED;
    }
}

int getAddress(int transactionId, Cursor *cursor) {
    auto *btcursor = (BtCursor *) ((DBECursor *) cursor->cursor)->cursor;
    return btcursor->address.pgno * PAGESIZE + btcursor->address.offset;
}

int create(const char *dbTable, const char *indexName, CursorType indexType,
           const int indexColumnCnt, const char **indexColumns) {}

int find(int transactionId, Cursor *cursor, const void *key) {
    Key_t k;
    k.data = (void *) extractString((const char *) key).c_str();
    k.size = extractString((const char *) key).length();
    if (cursor->cursorType == CURSOR_BTREE) {
        auto *btcursor = (BtCursor *) ((DBECursor *) cursor->cursor)->cursor;
        BPTree bpTree;
        bpTree.search(btcursor, k);
        bpTree.close();
        return FIND_SUCCESS;
    } else {
        return FIND_FAILED;
    }
}

void *getKey(int transactionId, Cursor *cursor) {
    /*
     * requires api from btree
     */
    if (cursor->cursorType == CURSOR_BTREE) {
        return (void *) (((DBECursor *) cursor->cursor)->key.c_str());
    } else {
        return NULL;
    }
}

void *getValue(int transactionId, Cursor *cursor) {
    /*
     * requires api from pager
     */
    if (cursor->cursorType == CURSOR_BTREE) {
        auto *btcursor = (BtCursor *) ((DBECursor *) cursor->cursor)->cursor;
        auto rtn = (void *) malloc(
                getTableMetadata((((DBECursor *) cursor->cursor)->tableName).c_str())->columnCnt * COLUMN_SIZE);
        auto page=GetMemPage(btcursor->address.pgno);
        memcpy(rtn,page,getTableMetadata((((DBECursor *) cursor->cursor)->tableName).c_str())->columnCnt * COLUMN_SIZE);
        return rtn;
    } else {
        return NULL;
    }
    // todo: need explanation from pager.
}

int insert(int transactionId, Cursor *cursor, const void *key, const void *value) {
    Key_t k;
    k.data = (void *) extractString((const char *) key).c_str();
    k.size = extractString((const char *) key).length();
    string data = extractString((const char *) value);
    if (cursor->cursorType == CURSOR_BTREE) {
        auto *btcursor = (BtCursor *) ((DBECursor *) cursor->cursor)->cursor;
        BPTree bpTree;
        bpTree.insert(btcursor, k, (void *) data.c_str(), data.length());
        return INSERT_SUCCESS;
    } else {
        return INSERT_FAILED;
    }
}

int erase(int transactionId, Cursor *cursor) {
    if (cursor->cursorType == CURSOR_BTREE) {
        auto *btcursor = (BtCursor *) ((DBECursor *) cursor->cursor)->cursor;
        BPTree bpTree;
        bpTree.remove(btcursor);
        bpTree.close();
        return ERASE_SUCCESS;
    } else {
        return ERASE_FAILED;
    }
}

int next(int transactionId, Cursor *cursor) {
    if (cursor->cursorType == CURSOR_BTREE) {
        auto *btcursor = (BtCursor *) ((DBECursor *) cursor->cursor)->cursor;
        BPTree bpTree;
        bpTree.next(btcursor);
        bpTree.close();
        return NEXT_SUCCESS;
    } else {
        return NEXT_FAILED;
    }
}

int reset(int transactionId, Cursor *cursor) {
    if (cursor->cursorType == CURSOR_BTREE) {
        auto *btcursor = (BtCursor *) ((DBECursor *) cursor->cursor)->cursor;
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


/*******************************************************************/
/*
;
;
;
;
;
;
;
;

pgno_t createTable();
pgno_t createIndex();
int clear(pgno_t page);
int destroy(pgno_t page);

int reorganize();
int getCookies();
int setCookies(int cookies);

int transaction(int *transactionId);
int commit(int transactionId);
int rollback(int transactionId);

 */
