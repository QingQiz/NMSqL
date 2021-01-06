#ifndef _DBENGINEINTERFACE_H
#define _DBENGINEINTERFACE_H
#include <stdint.h>

/***********************************
 * for vm
 ***********************************/

extern "C" {

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
Cursor *cursorOpen(int transactionId, const char *indexName, int flag);
int cursorClose(int transactionId, Cursor *cursor);
int find(int transactionId, Cursor *cursor, const void *key);
void *getKey(int transactionId, Cursor *cursor);
void *getValue(int transactionId, Cursor *cursor);
int getAddress(int transactionId, Cursor *cursor);
int insert(int transactionId, Cursor *cursor, const void *key,
           const void *value);
int erase(int transactionId, Cursor *cursor);
int next(int transactionId, Cursor *cursor);
int reset(int transactionId, Cursor *cursor);

pgno_t createTable();
pgno_t createIndex();
int clear(pgno_t page);
int destroy(pgno_t page);

int reorganize();
int getCookies();
int setCookies(int cookies);

/*
 * 事务相关
 */

int transaction(int *transactionId);
int commit(int transactionId);
int rollback(int transactionId);

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

struct TableMetadata *getTableMetadata(char *tableName);
}
char **getAllTableNames();

/*
 * 对外接口
 */
void openDb(const char *dbName);
void closeDb();
#endif
