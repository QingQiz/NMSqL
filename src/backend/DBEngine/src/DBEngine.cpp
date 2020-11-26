//
// Created by 17110 on 2020/11/18.
//
#include "DBEngine.h"
#include "BTreeInterface.h"
#include "DbEngineInterface.h"

/*
 * realization of DbEngineInterface.h
 */

Cursor *open(const char *indexName, int flag) {
	BPTree bpTree();
	if (flag==CURSOR_BTREE){

	} else {
		return NULL;
	}
}

int close(Cursor *cursor) {}

int create(const char *dbTable, const char *indexName, CursorType indexType,
           const int indexColumnCnt, const char **indexColumns) {}

int find(Cursor *cursor, const void *key) {}

void *getKey(Cursor *cursor) {}

void *getValue(Cursor *cursor) {}

// void* getRecordNumber(Cursor* cursor){}
int insert(Cursor *cursor, const void *key, const void *value) {}

int erase(Cursor *cursor) {}

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
