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
           const int indexColumnCnt, const char** indexColumns);
int find(Cursor* cursor, const void* key);
void* getKey(Cursor* cursor);
void* getValue(Cursor* cursor);
// void* getRecordNumber(Cursor* cursor);
int insert(Cursor* cursor, const void* key, const void* value);
int erase(Cursor* cursor);
int next(Cursor* cursor);
int reset(Cursor* cursor);


int createTable(const char* sql);
int reorganize();
void* getMetaData(const char* tableName);
int getCookies();
char** getTableColumns(const char* tableName);
