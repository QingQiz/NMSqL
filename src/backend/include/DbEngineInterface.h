#ifndef _DBENGINEINTERFACE_H
#define _DBENGINEINTERFACE_H
#include <stdint.h>


/***********************************
 * for vm
 ***********************************/

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


/***********************************
 * for compiler
 ***********************************/

struct TableMetadata {
    int32_t indexCnt;       // 索引数量
    char** indexName;       // 索引名
    char** indexColumn;     // 索引列名，应和索引名是一对一的关系

    int32_t columnCnt;      // 列的数量
    char** column;          // 列明

    int32_t cookie;         // 表 Cookie
};

struct TableMetadata* getTableMetadata(char* tableName);
#endif
