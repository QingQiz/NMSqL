#ifndef _PAGERINTERFACE_H
#define _PAGERINTERFACE_H
#include <string>

void* getMemPage(Pgno_t pgno);
void createNewPage(int size); //创建一个新的page，然后返回。
void deletePage(Pgno_t pgno); 
void writeBackPage(Pgno_t pgno);

// 分配 size 大小的空闲存储
void* pageMalloc(int size);

// TODO 可变指针，不可变指针
template<typename T>
class Pointer { };

class Pager {
public:
    Pager(std::string filename);
    ~Pager();
};

#endif
