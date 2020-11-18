#ifndef _PAGERINTERFACE_H
#define _PAGERINTERFACE_H
#include <string>

void* getMemPage(pgno_t pgno);

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
