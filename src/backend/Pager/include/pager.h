/*
** page 大小
*/
#define NMSQL_PAGE_SIZE 1024
 
/*
** 用来表示 page number 的数据类型.
** 第一页称为第 1 页
** 0 用来表示“不是一个 page”的含义
*/
typedef unsigned int Pgno;

/*
** 每个打开的文件都由一个单独的 Pager 结构体的实例管理
*/
typedef struct Pager Pager;

/*
** 成功码 以及 错误码
*/
#define NMSQL_OK            0       /*成功*/
#define NMSQL_PROTOCOL      -1      /*违反了锁定逻辑*/
#define NMSQL_BUSY          -2      /*所需文件等资源被占用，无法读写*/
#define NMSQL_ERROR         -3      /*发生严重错误*/
#define NMSQL_NOMEM         -4      /*内存分配错误或不足*/
#define NMSQL_IOERR         -5      /*读写错误*/
#define NMSQL_FULL          -6      /*磁盘已满*/
#define NMSQL_CORRUPT       -7      /*其他错误，如文件内容损坏或出错，文件 header 不合理等*/
#define NMSQL_CANTOPEN      -8      /*无法打开文件*/

int nmsqlpager_open(Pager **ppPager,const char *zFilename,int nPage,int nEx);
void nmsqlpager_set_destructor(Pager*, void(*)(void*));
int nmsqlpager_close(Pager *pPager);
int nmsqlpager_get(Pager *pPager, Pgno pgno, void **ppPage);
void *nmsqlpager_lookup(Pager *pPager, Pgno pgno);
int nmsqlpager_ref(void*);
int nmsqlpager_unref(void*);
Pgno nmsqlpager_pagenumber(void*);
int nmsqlpager_write(void*);
int nmsqlpager_iswriteable(void*);
int nmsqlpager_pagecount(Pager*);
int nmsqlpager_commit(Pager*);
int nmsqlpager_rollback(Pager*);
int *nmsqlpager_stats(Pager*);

#ifdef NMSQL_TEST
void nmsqlpager_refdump(Pager*);
int pager_refinfo_enable;
#endif