#pragma once

enum CursorType { CURSOR_BTREE, CURSOR_LIST };

typedef enum CursorType CursorType;

struct Cursor {
  CursorType cursorType;
};

typedef struct Cursor Cursor;

/*
 * Cursor相关操作
 */
Cursor* open(const char* dbTable, const char* indexName);
int create(const char* dbTable, const char* indexName, CursorType indexType,
           const int indexColumnCnt, const char** indexColumn);
int find(Cursor* cursor, const void* key);
int findNext(Cursor* cursor);
void* getKey(Cursor* cursor);
void* getValue(Cursor* cursor);
void* getRecordNumber(Cursor* cursor);
int insert(Cursor* cursor, const void* key, const void* value);
int erase(Cursor* cursor);
int pre(Cursor* cursor);
int next(Cursor* cursor);
int reset(Cursor* cursor);

/*
 * metadata相关操作
 */

int createTable(const char* createTableSql);
int reorganize(const char* dbTable);
void* getInfo(const char* dbTable);
int getNextRecordNumber(const char* dbTable);