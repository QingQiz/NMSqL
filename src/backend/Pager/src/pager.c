#include "pager.h"
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
 
/*
** 每个 page cache 都是一个整体，且它的状态永远只会是以下中的一种：
**
** NMSQL_UNLOCK         该 page cache 目前没有对数据库文件进行读取，
**                      内存中没有存储数据，此为默认状态
** 
** NMSQL_READLOCK       该 page cache 目前正在读取数据库文件，
**                      禁止写入，但是可以有多个用户进行读取
**
** NMSQL_WRITELOCK      该 page cache 目前正在写入数据库文件，
**                      此状态下数据库文件禁止访问，任何其他用户
**                      和进程不允许读写
** 
** 每一个 page cache 的初始状态均为 NMSQL_UNLOCK
** 当第一次调用 nmsql_page_get() 时，状态变为 NMSQL_READLOCK
** 当所有的 page 均使用 nmsql_page_unref() 释放后，状态变回 NMSQL_UNLOCK
** 当第一次调用 nmsql_page_write() 时，状态变为 NMSQL_WRITELOCK
** 在一个 pager 状态变为 NMSQL_WRITELOCK 之前，他的状态一定是 NMSQL_READLOCK
** 使用 nmsql_page_rollback() 和 nmsql_page_commit() 将状态变回 NMSQL_READLOCK
*/
#define NMSQL_UNLOCK      0
#define NMSQL_READLOCK    1
#define NMSQL_WRITELOCK   2


/*
** 内存中的每一个 page 开头都使用下面的 header
** 该 header 仅对对应 pager 可见
** 用户端只能访问到每个 header 后面跟着的 data
*/
typedef struct PgHdr PgHdr;
struct PgHdr {
  Pager *pPager;                 /* 该 page 属于哪个 pager */
  Pgno pgno;                     /* 该 page 的 page number */
  PgHdr *pNextHash, *pPrevHash;  /* page number 的哈希表 */
  int nRef;                      /* 当前使用该 page 的用户数量 */
  PgHdr *pNextFree, *pPrevFree;  /* nRef==0 时的 Freelist */
  PgHdr *pNextAll, *pPrevAll;    /* 前后所有的 page */
  char inJournal;                /* 已经写入日志文件置为 TRUE */
  char dirty;                    /* 需要写回改变置为 TRUE */
  /* 该 header 后面跟着 NMSQL_PAGE_SIZE 大小的 data */
  /* data 后面跟着 Pager.nExtra 大小的额外数据 */
};

/*
** 根据 PgHdr 的指针计算 data 的指针
** 反之亦然
*/
#define PGHDR_TO_DATA(P)  ((void*)(&(P)[1]))
#define DATA_TO_PGHDR(D)  (&((PgHdr*)(D))[-1])
#define PGHDR_TO_EXTRA(P) ((void*)&((char*)(&(P)[1]))[NMSQL_PAGE_SIZE])

/*
** 哈希表大小
*/
#define N_PG_HASH 101

/*
** 每一个活动的 page cache 都是下面结构体的一个实例
*/
struct Pager {
  char *zFilename;            /* 数据库文件名称 */
  char *zJournal;             /* 日志文件名称 */
  int fd, jfd;                /* 数据库文件和日志文件对应的文件描述符 (fd, File descriptors) */
  int dbSize;                 /* 该文件中的 page 数量 */
  int origDbSize;             /* 本次修改前的 dbSize */
  int nExtra;                 /* 在每一个内存中的 page 后增加这么多字节 */
  void (*xDestructor)(void*); /* 释放 pages 时调用 */
  int nPage;                  /* 内存中的总 page 数量 */
  int nRef;                   /* 内存中 PgHdr.nRef>0 的 page 的数量*/
  int mxPage;                 /* 每个 cache 最多能容纳的 page 数量 */
  int nHit, nMiss, nOvfl;     /* cache 命中、丢失、LRU溢出 */
  unsigned char state;        /* 状态码，NMSQL_UNLOCK, _READLOCK or _WRITELOCK */
  unsigned char errMask;      /* 错误码 */
  unsigned char *aInJournal;  /* 数据库文件中每一页都有一个 */
  PgHdr *pFirst, *pLast;      /* free pages 表 */
  PgHdr *pAll;                /* all pages 表*/
  PgHdr *aHash[N_PG_HASH];    /* PgHdr 的页数映射哈希表 */
};

/*
** 错误码
*/
#define PAGER_ERR_FULL     0x01  /* 写入失败 */
#define PAGER_ERR_MEM      0x02  /* 分配内存失败 */
#define PAGER_ERR_LOCK     0x04  /* 锁定协议错误 */
#define PAGER_ERR_CORRUPT  0x08  /* 数据库或日志文件损坏 */

/*
** 日志文件包含如下 page 记录格式
*/
typedef struct PageRecord PageRecord;
struct PageRecord {
  Pgno pgno;                     /* The page number */
  char aData[NMSQL_PAGE_SIZE];  /* pgno 页的原始数据 */
};

/*
** 日志文件开头为如下随机数列
*/
static const unsigned char aJournalMagic[] = {
  0xd9, 0xd5, 0x05, 0xf9, 0x20, 0xa1, 0x63, 0xd4,
};

/*
** 哈希计算 page number
*/
#define pager_hash(PN)  ((PN)%N_PG_HASH)

/*
** 启用 tracking
*/
#if NMSQL_TEST
int pager_refinfo_enable = 0;
  static void pager_refinfo(PgHdr *p){
    static int cnt = 0;
    if( !pager_refinfo_enable ) return;
    printf(
       "REFCNT: %4d addr=0x%08x nRef=%d\n",
       p->pgno, (int)PGHDR_TO_DATA(p), p->nRef
    );
    cnt++;   /* 此处可设断点 */
  }
# define REFINFO(X)  pager_refinfo(X)
#else
# define REFINFO(X)
#endif

/*
** 尝试给数据库文件加读锁 (wrlock==0) 或者加写锁 (wrlock==1)
** 成功返回 0，不成功返回非 0
*/
static int pager_lock(int fd, int wrlock){
  int rc;
  struct flock lock;
  lock.l_type = wrlock ? F_WRLCK : F_RDLCK;
  lock.l_whence = SEEK_SET;
  lock.l_start = lock.l_len = 0L;
  rc = fcntl(fd, F_SETLK, &lock);
  return rc!=0;
}

/*
** 解锁数据库文件
*/
static int pager_unlock(fd){
  int rc;
  struct flock lock;
  lock.l_type = F_UNLCK;
  lock.l_whence = SEEK_SET;
  lock.l_start = lock.l_len = 0L;
  rc = fcntl(fd, F_SETLK, &lock);
  return rc!=0;
}

/*
** 根据文件描述符 fd 将游标从文件开头移动到 whereto 处
*/
static int pager_seek(int fd, off_t whereto){
  /*printf("SEEK to page %d\n", whereto/NMSQL_PAGE_SIZE + 1);*/
  lseek(fd, whereto, SEEK_SET);
  return NMSQL_OK;
}

/*
** 截断给定的文件，使其正好包含 mxPg 页的数据
*/
static int pager_truncate(int fd, Pgno mxPg){
  int rc;
  rc = ftruncate(fd, mxPg*NMSQL_PAGE_SIZE);
  return rc!=0 ? NMSQL_IOERR : NMSQL_OK;
}

/*
** 从 fd 中读取 nBytes 的数据到 pBuf 中
** 如果数据不能被读取或者只读取了一部分，
** 那么 pBuf 中未读取的部分将被填充为0，
** 并返回 NMSQL_IOERR
** 如果读取完全成功，则返回 NMSQL_OK
*/
static int pager_read(int fd, void *pBuf, int nByte){
  int rc;
  /* printf("READ\n");*/
  rc = read(fd, pBuf, nByte);
  if( rc<0 ){
    memset(pBuf, 0, nByte);
    return NMSQL_IOERR;
  }
  if( rc<nByte ){
    memset(&((char*)pBuf)[rc], 0, nByte - rc);
    rc = NMSQL_IOERR;
  }else{
    rc = NMSQL_OK;
  }
  return rc;
}

/*
** 将 nBytes 的数据写入 fd 中
** 如果发生任何问题或者写入不完整，
** 返回 NMSQL_IOERR
** 完全成功后返回 NMSQL_OK
*/
static int pager_write(int fd, const void *pBuf, int nByte){
  int rc;
  /*printf("WRITE\n");*/
  rc = write(fd, pBuf, nByte);
  if( rc<nByte ){
    return NMSQL_FULL;
  }else{
    return NMSQL_OK;
  }
}

/*
** 将 pPager->errMask 中的错误码转化为一个合适的方便用于 return 的数
*/
static int pager_errcode(Pager *pPager){
  int rc = NMSQL_OK;
  if( pPager->errMask & PAGER_ERR_LOCK )    rc = NMSQL_PROTOCOL;
  if( pPager->errMask & PAGER_ERR_FULL )    rc = NMSQL_FULL;
  if( pPager->errMask & PAGER_ERR_MEM )     rc = NMSQL_NOMEM;
  if( pPager->errMask & PAGER_ERR_CORRUPT ) rc = NMSQL_CORRUPT;
  return rc;
}

/*
** 根据给定的 pgno， 在哈希表中找到一个页面
** 返回指向该页的指针，如果没有找到，则返回 NULL
*/
static PgHdr *pager_lookup(Pager *pPager, Pgno pgno){
  PgHdr *p = pPager->aHash[pgno % N_PG_HASH];
  while( p && p->pgno!=pgno ){
    p = p->pNextHash;
  }
  return p;
}

/*
** 解锁数据库并清空内存中的缓存
** 该函数会将 pager 恢复为初次打开的状态
** 所有的 page 都会从内存中清空
** 若再直接对这些 page 进行操作，可能会导致错误
*/
static void pager_reset(Pager *pPager){
  PgHdr *pPg, *pNext;
  for(pPg=pPager->pAll; pPg; pPg=pNext){
    pNext = pPg->pNextAll;
    nmsqlFree(pPg);
  }
  pPager->pFirst = 0;
  pPager->pLast = 0;
  pPager->pAll = 0;
  memset(pPager->aHash, 0, sizeof(pPager->aHash));
  pPager->nPage = 0;
  if( pPager->state==NMSQL_WRITELOCK ){
    nmsqlpager_rollback(pPager);
  }
  pager_unlock(pPager->fd);
  pPager->state = NMSQL_UNLOCK;
  pPager->dbSize = -1;
  pPager->nRef = 0;
}

/*
** 该函数被调用时，pager 会打开日志文件并确保数据库上着写锁
** 随后该函数会解开数据库的写锁，上读锁，日志文件被删除并关闭
** 
** 因为会先解开写锁再上读锁，这期间不应该有其他的进程或者用户进来影响
** 所以我们要先占用住日志文件，防止其他进程或用户进行操作，
** 因为任何进程都不应该在没有获取日志文件的情况下对数据库进行操作
** 
** 但是假如该函数失败了，则返回 NMSQL_PROTOCOL
** 成功返回 NMSQL_OK
*/
static int pager_unwritelock(Pager *pPager){
  int rc;
  PgHdr *pPg;
  if( pPager->state!=NMSQL_WRITELOCK ) return NMSQL_OK;
  pager_unlock(pPager->fd);
  rc = pager_lock(pPager->fd, 0);
  unlink(pPager->zJournal);
  close(pPager->jfd);
  pPager->jfd = -1;
  nmsqlFree( pPager->aInJournal );
  pPager->aInJournal = 0;
  for(pPg=pPager->pAll; pPg; pPg=pPg->pNextAll){
    pPg->inJournal = 0;
    pPg->dirty = 0;
  }
  if( rc!=NMSQL_OK ){
    pPager->state = NMSQL_UNLOCK;
    rc = NMSQL_PROTOCOL;
    pPager->errMask |= PAGER_ERR_LOCK;
  }else{
    rc = NMSQL_OK;
    pPager->state = NMSQL_READLOCK;
  }
  return rc;
}

/*
** 根据日志回滚数据库，恢复到开始修改之前的状态
**
** 日志文件的格式如下：
** 一个初始数列 aJournalMagic[]，用来检查日志文件的安全性
** 一个 Pgno，存放数据库更改前的页数，数据库会被截断到这个大小
** 之后是详细的 page 记录，每一个记录就是一个 PageRecord
**
** 回滚时，从后向前恢复数据库文件
** 
** 若根据初始数列发现日志文件损坏，返回 NMSQL_PROTOCOL
** 若其他地方出错，pPager->errMask 置为 PAGER_ERR_CORRUPT，返回 NMSQL_CORRUPT
** 若正常，返回 NMSQL_OK
*/
static int pager_playback(Pager *pPager){
  int nRec;                /* 记录的数量 */
  int i;                   /* 循环计数 */
  Pgno mxPg = 0;           /* 原始文件页数 */
  struct stat statbuf;     /* 确定日志大小 */
  PgHdr *pPg;              /* 缓存中的现有 page */
  PageRecord pgRec;
  unsigned char aMagic[sizeof(aJournalMagic)];
  int rc;

  /* 
  ** 读取日志开头，并将数据库文件截回原大小
  */
  assert( pPager->jfd>=0 );
  pager_seek(pPager->jfd, 0);
  rc = pager_read(pPager->jfd, aMagic, sizeof(aMagic));
  if( rc!=NMSQL_OK || memcmp(aMagic,aJournalMagic,sizeof(aMagic))!=0 ){
    return NMSQL_PROTOCOL;
  }
  rc = pager_read(pPager->jfd, &mxPg, sizeof(mxPg));
  if( rc!=NMSQL_OK ){
    return NMSQL_PROTOCOL;
  }
  pager_truncate(pPager->fd, mxPg);
  pPager->dbSize = mxPg;
  
  /* 
  ** 从结尾向开头开始读日志
  */
  if( fstat(pPager->jfd, &statbuf)!=0 ){
    return NMSQL_OK;
  }
  nRec = (statbuf.st_size - (sizeof(aMagic)+sizeof(Pgno))) / sizeof(PageRecord);

  /* 
  ** 从最后一段往前处理到第一段
  */
  for(i=nRec-1; i>=0; i--){
    /* 寻找开头段 */
    off_t ofst;
    ofst = i*sizeof(PageRecord) + sizeof(aMagic) + sizeof(Pgno);
    rc = pager_seek(pPager->jfd, ofst);
    if( rc!=NMSQL_OK ) break;
    rc = pager_read(pPager->jfd, &pgRec, sizeof(pgRec));
    if( rc!=NMSQL_OK ) break;

    /* page 的合理性检查 */
    if( pgRec.pgno>mxPg || pgRec.pgno==0 ){
      rc = NMSQL_CORRUPT;
      break;
    }

    /* 
    ** 回滚该 page
    ** 如果有的话，同时更新该 page 在内存中的副本
    */
    pPg = pager_lookup(pPager, pgRec.pgno);
    if( pPg ){
      memcpy(PGHDR_TO_DATA(pPg), pgRec.aData, NMSQL_PAGE_SIZE);
      memset(PGHDR_TO_EXTRA(pPg), 0, pPager->nExtra);
    }
    rc = pager_seek(pPager->fd, (pgRec.pgno-1)*NMSQL_PAGE_SIZE);
    if( rc!=NMSQL_OK ) break;
    rc = pager_write(pPager->fd, pgRec.aData, NMSQL_PAGE_SIZE);
    if( rc!=NMSQL_OK ) break;
  }
  if( rc!=NMSQL_OK ){
    pager_unwritelock(pPager);
    pPager->errMask |= PAGER_ERR_CORRUPT;
    rc = NMSQL_CORRUPT;
  }else{
    rc = pager_unwritelock(pPager);
  }
  return rc;
}

/*
** 分配内存
*/
void *nmsqlMalloc(size_t size){
  return malloc(size);
}

/*
** 释放内存
*/
void nmsqlFree(void* ptr){
  free(ptr);
}

/*
** 创建一个新的 page cache，并在 **ppPager 中存放一个指向该 page cache 的指针
** 需要存放进 cache 的文件这时候不需要真正存在，
** 该文件在第一次调用 nmsqlpager_get() 之前不会打开
** 该文件只有在调用 nmsqlpager_unref() 释放最后一个 page 的时候会保持打开
*/
int nmsqlpager_open(
  Pager **ppPager,         /* 返回的 Pager */
  const char *zFilename,   /* 要打开的数据库文件的名称 */
  int mxPage,              /* 内存缓存中能存储的最多的页数 */
  int nExtra               /* 附加到每个内存页上的额外字节 */
){
  Pager *pPager;
  int nameLen;
  int fd;

  *ppPager = 0;

  fd = open(zFilename, O_RDWR|O_CREAT, 0644);
  if( fd<0 ){
    return NMSQL_CANTOPEN;
  }
  nameLen = strlen(zFilename);
  pPager = nmsqlMalloc( sizeof(*pPager) + nameLen*2 + 30 );
  if( pPager==0 ){
    close(fd);
    return NMSQL_NOMEM;
  }
  pPager->zFilename = (char*)&pPager[1];
  pPager->zJournal = &pPager->zFilename[nameLen+1];
  strcpy(pPager->zFilename, zFilename);
  strcpy(pPager->zJournal, zFilename);
  strcpy(&pPager->zJournal[nameLen], "-journal");
  pPager->fd = fd;
  pPager->jfd = -1;
  pPager->nRef = 0;
  pPager->dbSize = -1;
  pPager->nPage = 0;
  pPager->mxPage = mxPage>5 ? mxPage : 10;
  pPager->state = NMSQL_UNLOCK;
  pPager->errMask = 0;
  pPager->pFirst = 0;
  pPager->pLast = 0;
  pPager->nExtra = nExtra;
  memset(pPager->aHash, 0, sizeof(pPager->aHash));
  *ppPager = pPager;
  return NMSQL_OK;
}

/*
** 设置 pager 的 destructor，如果不为 NULL 的话，
** 当 page 的 nRef 达到 0 的时候，就会调用 destructor
**
** nmsqlpager_close() 不会调用 destructor.  
** 只有 nmsqlpager_unref() 会调用 destructor.
*/
void nmsqlpager_set_destructor(Pager *pPager, void (*xDesc)(void*)){
  pPager->xDestructor = xDesc;
}

/*
** 返回 pPager 打开的文件的总页数
*/
int nmsqlpager_pagecount(Pager *pPager){
  int n;
  struct stat statbuf;
  assert( pPager!=0 );
  if( pPager->dbSize>=0 ){
    return pPager->dbSize;
  }
  if( fstat(pPager->fd, &statbuf)!=0 ){
    n = 0;
  }else{
    n = statbuf.st_size/NMSQL_PAGE_SIZE;
  }
  if( pPager->state!=NMSQL_UNLOCK ){
    pPager->dbSize = n;
  }
  return n;
}

/*
** 关闭 page cache.  释放所有内存并删除所有文件.
**
** 调用该函数时，若有正在进行的事务，该事务将回滚
** 所有内存中的 page 将会失效，并释放内存
** 之后所有尝试对这些 page 进行操作的行为都可能导致出错
*/
int nmsqlpager_close(Pager *pPager){
  PgHdr *pPg, *pNext;
  switch( pPager->state ){
    case NMSQL_WRITELOCK: {
      nmsqlpager_rollback(pPager);
      pager_unlock(pPager->fd);
      break;
    }
    case NMSQL_READLOCK: {
      pager_unlock(pPager->fd);
      break;
    }
    default: {
      /* Do nothing */
      break;
    }
  }
  for(pPg=pPager->pAll; pPg; pPg=pNext){
    pNext = pPg->pNextAll;
    nmsqlFree(pPg);
  }
  if( pPager->fd>=0 ) close(pPager->fd);
  assert( pPager->jfd<0 );
  nmsqlFree(pPager);
  return NMSQL_OK;
}

/*
** 返回指定 data 的页码
*/
Pgno nmsqlpager_pagenumber(void *pData){
  PgHdr *p = DATA_TO_PGHDR(pData);
  return p->pgno;
}

/*
** 增加 page 引用次数(nRef)
** 若该 page 在 freelist 中（引用次数==0），则将其从 freelist 中删除
*/
static void page_ref(PgHdr *pPg){
  if( pPg->nRef==0 ){
    /* 该 page 目前在 freelist 中，移除它 */
    if( pPg->pPrevFree ){
      pPg->pPrevFree->pNextFree = pPg->pNextFree;
    }else{
      pPg->pPager->pFirst = pPg->pNextFree;
    }
    if( pPg->pNextFree ){
      pPg->pNextFree->pPrevFree = pPg->pPrevFree;
    }else{
      pPg->pPager->pLast = pPg->pPrevFree;
    }
    pPg->pPager->nRef++;
  }
  pPg->nRef++;
  REFINFO(pPg);
}

/*
** 增加 page 引用次数(nRef).
** 输入指针为 page data 的指针
*/
int nmsqlpager_ref(void *pData){
  PgHdr *pPg = DATA_TO_PGHDR(pData);
  page_ref(pPg);
  return NMSQL_OK;
}

/*
** 获取一个 page
**
** 获取的第一个 page 会上一个读锁，当最后一个 page 被释放的时候，取消锁
** 如果数据库文件小于所请求的页数，则不发生实际的磁盘读取，该 page 在内存中的映像初始化为 0
** 当 page 初始化到内存中的时候，附加的额外数据总是为 0
** 
** 获取 page 有可能因为各种原因失败，在这种情况下会返回一个相应的 errcode，并将 **ppPage 设置为 NULL
**
** 和 nmsqlpager_lookup() 类似，该函数会先尝试在内存缓存中找到一个页面，如果内存中没有再在磁盘中读取
** 该函数在第一次读取磁盘的时候会上一个读锁，如果有必要的话，也可以回放一个旧日志
*/
int nmsqlpager_get(Pager *pPager, Pgno pgno, void **ppPage){
  PgHdr *pPg;

  /* 确保没有发生严重错误.
  */ 
  if( pPager==0 || pgno==0 ){
    return NMSQL_ERROR;
  }
  if( pPager->errMask & ~(PAGER_ERR_FULL) ){
    return pager_errcode(pPager);
  }

  /* 
  ** 如果该 page 是第一次被获取，上读锁
  */
  if( pPager->nRef==0 ){
    if( pager_lock(pPager->fd, 0)!=0 ){
      *ppPage = 0;
      return NMSQL_BUSY;
    }
    pPager->state = NMSQL_READLOCK;

    /* 如果存在日志文件，尝试回放 (playback).
    */
    if( access(pPager->zJournal,0)==0 ){
       int rc;

       /* 
       ** 以独占的方式打开日志文件，如果不能独占日志文件，返回 NMSQL_BUSY
       */
       pPager->jfd = open(pPager->zJournal, O_RDONLY, 0);
       if( pPager->jfd<0 || pager_lock(pPager->jfd, 1)!=0 ){
         if( pPager->jfd>=0 ){ close(pPager->jfd); pPager->jfd = -1; }
         pager_unlock(pPager->fd);
         *ppPage = 0;
         return NMSQL_BUSY;
       }

       /* 尝试给数据库加写锁 */
       pager_unlock(pPager->fd);
       if( pager_lock(pPager->fd, 1)!=0 ){
         close(pPager->jfd);
         pPager->jfd = -1;
         *ppPage = 0;
         return NMSQL_PROTOCOL;
       }

       /* 
       ** 回放并删除日志文件，删除数据库的写锁，重新上读锁
       */
       rc = pager_playback(pPager);
       if( rc!=NMSQL_OK ){
         return rc;
       }
    }
    pPg = 0;
  }else{
    /* 在缓存中寻找 page */
    pPg = pager_lookup(pPager, pgno);
  }
  if( pPg==0 ){
    /* 需要的 page 没有在缓存中. */
    int h;
    pPager->nMiss++;
    if( pPager->nPage<pPager->mxPage || pPager->pFirst==0 ){
      /* 创建一个新 page */
      pPg = nmsqlMalloc( sizeof(*pPg) + NMSQL_PAGE_SIZE + pPager->nExtra );
      if( pPg==0 ){
        *ppPage = 0;
        pager_unwritelock(pPager);
        pPager->errMask |= PAGER_ERR_MEM;
        return NMSQL_NOMEM;
      }
      pPg->pPager = pPager;
      pPg->pNextAll = pPager->pAll;
      if( pPager->pAll ){
        pPager->pAll->pPrevAll = pPg;
      }
      pPg->pPrevAll = 0;
      pPager->pAll = pPg;
      pPager->nPage++;
    }else{
      /* 回收旧 page.  首先需要找到要回收的 page
      ** 尝试找出一个不脏(dirty)并且靠近 freelist 表头的 page */
      int cnt = pPager->mxPage/2;
      pPg = pPager->pFirst;
      while( pPg->dirty && 0<cnt-- && pPg->pNextFree ){
        pPg = pPg->pNextFree;
      }
      if( pPg==0 || pPg->dirty ) pPg = pPager->pFirst;
      assert( pPg->nRef==0 );

      /* 如果要回收的页面是脏的(dirty)，就同步日志，把旧的 page 写入数据库中. */
      if( pPg->dirty ){
        int rc;
        assert( pPg->inJournal==1 );
        assert( pPager->state==NMSQL_WRITELOCK );
        rc = fsync(pPager->jfd);
        if( rc!=0 ){
          rc = nmsqlpager_rollback(pPager);
          *ppPage = 0;
          if( rc==NMSQL_OK ) rc = NMSQL_IOERR;
          return rc;
        }
        pager_seek(pPager->fd, (pPg->pgno-1)*NMSQL_PAGE_SIZE);
        rc = pager_write(pPager->fd, PGHDR_TO_DATA(pPg), NMSQL_PAGE_SIZE);
        if( rc!=NMSQL_OK ){
          rc = nmsqlpager_rollback(pPager);
          *ppPage = 0;
          if( rc==NMSQL_OK ) rc = NMSQL_FULL;
          return rc;
        }
      }

      /* 从 freelist 和哈希表中解开旧页面的链接
      */
      if( pPg->pPrevFree ){
        pPg->pPrevFree->pNextFree = pPg->pNextFree;
      }else{
        assert( pPager->pFirst==pPg );
        pPager->pFirst = pPg->pNextFree;
      }
      if( pPg->pNextFree ){
        pPg->pNextFree->pPrevFree = pPg->pPrevFree;
      }else{
        assert( pPager->pLast==pPg );
        pPager->pLast = pPg->pPrevFree;
      }
      pPg->pNextFree = pPg->pPrevFree = 0;
      if( pPg->pNextHash ){
        pPg->pNextHash->pPrevHash = pPg->pPrevHash;
      }
      if( pPg->pPrevHash ){
        pPg->pPrevHash->pNextHash = pPg->pNextHash;
      }else{
        h = pager_hash(pPg->pgno);
        assert( pPager->aHash[h]==pPg );
        pPager->aHash[h] = pPg->pNextHash;
      }
      pPg->pNextHash = pPg->pPrevHash = 0;
      pPager->nOvfl++;
    }
    pPg->pgno = pgno;
    if( pPager->aInJournal && pgno<=pPager->origDbSize ){
      pPg->inJournal = (pPager->aInJournal[pgno/8] & (1<<(pgno&7)))!=0;
    }else{
      pPg->inJournal = 0;
    }
    pPg->dirty = 0;
    pPg->nRef = 1;
    REFINFO(pPg);
    pPager->nRef++;
    h = pager_hash(pgno);
    pPg->pNextHash = pPager->aHash[h];
    pPager->aHash[h] = pPg;
    if( pPg->pNextHash ){
      assert( pPg->pNextHash->pPrevHash==0 );
      pPg->pNextHash->pPrevHash = pPg;
    }
    if( pPager->dbSize<0 ) nmsqlpager_pagecount(pPager);
    if( pPager->dbSize<pgno ){
      memset(PGHDR_TO_DATA(pPg), 0, NMSQL_PAGE_SIZE);
    }else{
      pager_seek(pPager->fd, (pgno-1)*NMSQL_PAGE_SIZE);
      pager_read(pPager->fd, PGHDR_TO_DATA(pPg), NMSQL_PAGE_SIZE);
    }
    if( pPager->nExtra>0 ){
      memset(PGHDR_TO_EXTRA(pPg), 0, pPager->nExtra);
    }
  }else{
    /* 需要的 page 在缓存中. */
    pPager->nHit++;
    page_ref(pPg);
  }
  *ppPage = PGHDR_TO_DATA(pPg);
  return NMSQL_OK;
}

/*
** 如果一个 page 已经在内存中的缓存中，则获取该 page
** 不从磁盘中读取 page
** 返回一个指向该 page 的指针
** 如果该 page 不在缓存中，返回 0
**
** 参见 nmsqlpager_get().
** 该函数与 nmsqlpager_get() 的不同之处在于：
** nmsqlpager_get() 是如果 page 不在缓存里就进入磁盘读取 page
** nmsqlpager_lookup() 是如果 page 不在缓存里或者发生了磁盘 I/O 就返回 0
*/
void *nmsqlpager_lookup(Pager *pPager, Pgno pgno){
  PgHdr *pPg;

  /* 确保不会发生任何严重的错误.
  */ 
  if( pPager==0 || pgno==0 ){
    return 0;
  }
  if( pPager->errMask & ~(PAGER_ERR_FULL) ){
    return 0;
  }
  if( pPager->nRef==0 ){
    return 0;
  }
  pPg = pager_lookup(pPager, pgno);
  if( pPg==0 ) return 0;
  page_ref(pPg);
  return PGHDR_TO_DATA(pPg);
}

/*
** 释放一个 page.
**
** 如果对该 page 的引用数量(nRef)降为 0，那么该 page 将被添加到 LRU 列表中
** 当对所有 page 的引用都被释放时，就会发生回滚，数据库的锁也会被移除
*/
int nmsqlpager_unref(void *pData){
  Pager *pPager;
  PgHdr *pPg;

  /* 减少这一 page 的引用次数
  */
  pPg = DATA_TO_PGHDR(pData);
  assert( pPg->nRef>0 );
  pPager = pPg->pPager;
  pPg->nRef--;
  REFINFO(pPg);

  /* 当一个 page 的引用数达到 0 时，
  ** 调用析构器(destructor)并将该 page 添加到 freelist 中。
  */
  if( pPg->nRef==0 ){
    pPg->pNextFree = 0;
    pPg->pPrevFree = pPager->pLast;
    pPager->pLast = pPg;
    if( pPg->pPrevFree ){
      pPg->pPrevFree->pNextFree = pPg;
    }else{
      pPager->pFirst = pPg;
    }
    if( pPager->xDestructor ){
      pPager->xDestructor(pData);
    }
  
    /* 当所有 pages 都进入 freelist时, 解除数据库的读锁
    */
    pPager->nRef--;
    assert( pPager->nRef>=0 );
    if( pPager->nRef==0 ){
      pager_reset(pPager);
    }
  }
  return NMSQL_OK;
}

/*
** 将一个 data page 标记为可写
** 如果该 page 还没有写入日志，则写入日志
** 在修改 page 前必须调用该函数
** 
** 初次调用该函数时，pager 会创建一个新的日志，并给数据库上写锁
** 如果不能上写锁，返回 NMSQL_BUSY
** 调用该函数必须检查返回值，如果不为 NMSQL_OK 则绝不允许修改 page 中的数据
** 
** 如果因为磁盘已经满了而无法写入日志文件，返回 NMSQL_FULL，并回滚
** 所有后续的写入尝试也将返回 NMSQL_FULL
** 直到使用 nmsqlpager_commit() 或 nmsqlpager_rollback() 来重置
*/
int nmsqlpager_write(void *pData){
  PgHdr *pPg = DATA_TO_PGHDR(pData);
  Pager *pPager = pPg->pPager;
  int rc = NMSQL_OK;

  if( pPager->errMask ){ 
    return pager_errcode(pPager);
  }
  pPg->dirty = 1;
  if( pPg->inJournal ){ return NMSQL_OK; }
  assert( pPager->state!=NMSQL_UNLOCK );
  if( pPager->state==NMSQL_READLOCK ){
    assert( pPager->aInJournal==0 );
    pPager->aInJournal = nmsqlMalloc( pPager->dbSize/8 + 1 );
    if( pPager->aInJournal==0 ){
      return NMSQL_NOMEM;
    }
    pPager->jfd = open(pPager->zJournal, O_RDWR|O_CREAT, 0644);
    if( pPager->jfd<0 ){
      return NMSQL_CANTOPEN;
    }
    if( pager_lock(pPager->jfd, 1) ){
      close(pPager->jfd);
      pPager->jfd = -1;
      return NMSQL_BUSY;
    }
    pager_unlock(pPager->fd);
    if( pager_lock(pPager->fd, 1) ){
      close(pPager->jfd);
      pPager->jfd = -1;
      pPager->state = NMSQL_UNLOCK;
      pPager->errMask |= PAGER_ERR_LOCK;
      return NMSQL_PROTOCOL;
    }
    pPager->state = NMSQL_WRITELOCK;
    nmsqlpager_pagecount(pPager);
    pPager->origDbSize = pPager->dbSize;
    rc = pager_write(pPager->jfd, aJournalMagic, sizeof(aJournalMagic));
    if( rc==NMSQL_OK ){
      rc = pager_write(pPager->jfd, &pPager->dbSize, sizeof(Pgno));
    }
    if( rc!=NMSQL_OK ){
      rc = pager_unwritelock(pPager);
      if( rc==NMSQL_OK ) rc = NMSQL_FULL;
      return rc;
    }
  }
  assert( pPager->state==NMSQL_WRITELOCK );
  assert( pPager->jfd>=0 );
  if( pPg->pgno <= pPager->origDbSize ){
    rc = pager_write(pPager->jfd, &pPg->pgno, sizeof(Pgno));
    if( rc==NMSQL_OK ){
      rc = pager_write(pPager->jfd, pData, NMSQL_PAGE_SIZE);
    }
    if( rc!=NMSQL_OK ){
      nmsqlpager_rollback(pPager);
      pPager->errMask |= PAGER_ERR_FULL;
      return rc;
    }
    assert( pPager->aInJournal!=0 );
    pPager->aInJournal[pPg->pgno/8] |= 1<<(pPg->pgno&7);
  }
  pPg->inJournal = 1;
  if( pPager->dbSize<pPg->pgno ){
    pPager->dbSize = pPg->pgno;
  }
  return rc;
}

/*
** 若传入的 page 之前是传递给 nmsqlpager_write() 的，
** 则返回 TRUE
** 即若 page 是可写的，返回 TRUE
*/
int nmsqlpager_iswriteable(void *pData){
  PgHdr *pPg = DATA_TO_PGHDR(pData);
  return pPg->dirty;
}

/*
** 提交对数据库的所有更改并解开写锁
**
** 如果提交失败，则尝试回滚并返回一个错误码
** 如果提交成功，返回 NMSQL_OK
*/
int nmsqlpager_commit(Pager *pPager){
  int i, rc;
  PgHdr *pPg;

  if( pPager->errMask==PAGER_ERR_FULL ){
    rc = nmsqlpager_rollback(pPager);
    if( rc==NMSQL_OK ) rc = NMSQL_FULL;
    return rc;
  }
  if( pPager->errMask!=0 ){
    rc = pager_errcode(pPager);
    return rc;
  }
  if( pPager->state!=NMSQL_WRITELOCK ){
    return NMSQL_ERROR;
  }
  assert( pPager->jfd>=0 );
  if( fsync(pPager->jfd) ){
    goto commit_abort;
  }
  for(i=0; i<N_PG_HASH; i++){
    for(pPg=pPager->aHash[i]; pPg; pPg=pPg->pNextHash){
      if( pPg->dirty==0 ) continue;
      rc = pager_seek(pPager->fd, (pPg->pgno-1)*NMSQL_PAGE_SIZE);
      if( rc!=NMSQL_OK ) goto commit_abort;
      rc = pager_write(pPager->fd, PGHDR_TO_DATA(pPg), NMSQL_PAGE_SIZE);
      if( rc!=NMSQL_OK ) goto commit_abort;
    }
  }
  if( fsync(pPager->fd) ) goto commit_abort;
  rc = pager_unwritelock(pPager);
  pPager->dbSize = -1;
  return rc;

  /* 如果提交过程中出现任何错误，就跳转到这里.
  */
commit_abort:
  rc = nmsqlpager_rollback(pPager);
  if( rc==NMSQL_OK ){
    rc = NMSQL_FULL;
  }
  return rc;
}

/*
** 回滚所有更改，数据库恢复到只读
** 缓存中的所有 page 恢复到原来的数据内容
** 日志删除
**
** 该函数理论上不会发生错误或失败，
** 除非其他进程未遵循锁定协议 (NMSQL_PROTOCOL)
** 或者其他进程导致日志文件出错 (NMSQL_CORRUPT)
** 或者之前内存分配 (malloc()) 失败 (NMSQL_NOMEM)
** 对于这些所有的情况，都有对应的错误码
** 成功，返回 NMSQL_OK
*/
int nmsqlpager_rollback(Pager *pPager){
  int rc;
  if( pPager->errMask!=0 && pPager->errMask!=PAGER_ERR_FULL ){
    return pager_errcode(pPager);
  }
  if( pPager->state!=NMSQL_WRITELOCK ){
    return NMSQL_OK;
  }
  rc = pager_playback(pPager);
  if( rc!=NMSQL_OK ){
    rc = NMSQL_CORRUPT;
    pPager->errMask |= PAGER_ERR_CORRUPT;
  }
  pPager->dbSize = -1;
  return rc;
};

/*
** 该函数仅用于测试分析
*/
int *nmsqlpager_stats(Pager *pPager){
  static int a[9];
  a[0] = pPager->nRef;
  a[1] = pPager->nPage;
  a[2] = pPager->mxPage;
  a[3] = pPager->dbSize;
  a[4] = pPager->state;
  a[5] = pPager->errMask;
  a[6] = pPager->nHit;
  a[7] = pPager->nMiss;
  a[8] = pPager->nOvfl;
  return a;
}

#if NMSQL_TEST
/*
** 打印所有被引用 page 的列表及其引用次数
*/
void nmsqlpager_refdump(Pager *pPager){
  PgHdr *pPg;
  for(pPg=pPager->pAll; pPg; pPg=pPg->pNextAll){
    if( pPg->nRef<=0 ) continue;
    printf("PAGE %3d addr=0x%08x nRef=%d\n", 
       pPg->pgno, (int)PGHDR_TO_DATA(pPg), pPg->nRef);
  }
}
#endif
