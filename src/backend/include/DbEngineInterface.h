#ifndef _DBENGINEINTERFACE_H
#define _DBENGINEINTERFACE_H
#include <stdint.h>

/***********************************
 * for vm
 ***********************************/

extern "C" {

struct DbEngine {};

enum CursorType { CURSOR_BTREE, CURSOR_LIST };

typedef enum CursorType CursorType;

struct Cursor {
  CursorType cursorType;
  void *cursor;
};

typedef struct Cursor Cursor;
typedef int pgno_t;

/*
 * Cursor相关操作
 */
#define CURSOR_READ_ONLY 1
#define CURSOR_WRITE 2
Cursor *open(const void *dbEngine, int transactionId, const char *indexName,
             int flag);
int close(const void *dbEngine, int transactionId, Cursor *cursor);
int create(const void *dbEngine, const char *dbTable, const char *indexName,
           CursorType indexType, const int indexColumnCnt,
           const char **indexColumns);
int find(const void *dbEngine, int transactionId, Cursor *cursor,
         const void *key);
void *getKey(const void *dbEngine, int transactionId, Cursor *cursor);
void *getValue(const void *dbEngine, int transactionId, Cursor *cursor);
int getAddress(const void *dbEngine, int transactionId, Cursor *cursor);
// void* getRecordNumber(const void *dbEngine,Cursor* cursor);
int insert(const void *dbEngine, int transactionId, Cursor *cursor,
           const void *key, const void *value);
int erase(const void *dbEngine, int transactionId, Cursor *cursor);
int next(const void *dbEngine, int transactionId, Cursor *cursor);
int reset(const void *dbEngine, int transactionId, Cursor *cursor);

void *getValueByAddress(const void *dbEngine, int transactionId, int address);

pgno_t createTable(const void *dbEngine);
pgno_t createIndex(const void *dbEngine);
int clear(const void *dbEngine, pgno_t page);
int destroy(const void *dbEngine, pgno_t page);

int reorganize(const void *dbEngine);
void *getMetaData(const void *dbEngine, const char *tableName);
int getCookies(const void *dbEngine);
int setCookies(const void *dbEngine, int cookies);
char **getTableColumns(const void *dbEngine, const char *tableName);

/*
 * 事务相关
 */

int transaction(const void *dbEngine, int *transactionId);
int commit(const void *dbEngine, int transactionId);
int rollback(const void *dbEngine, int transactionId);

/***********************************
 * for compiler
 ***********************************/

struct TableMetadata {
  int32_t indexCnt; // 索引数量
  char **index;     // 索引
                    //   格式：索引名:逗号分隔的列名
                    //   例如：index_a_b:a,b
                    //     代表 a，b 上的索引，名为 index_a_b

  int32_t columnCnt; // 列的数量
  char **column;     // 列名

  int32_t cookie; // 表 Cookie
};

struct TableMetadata *getTableMetadata(const void *dbEngine, char *tableName);
}
#endif
