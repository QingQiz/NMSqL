/*
** 定义了接口的头文件
*/
#ifndef _SQLITE_H_
#define _SQLITE_H_

#define SQLITE_VERSION          1.1
#define SQLITE_VERSION_STRING  "1.1"

typedef struct nmsql nmsql;

nmsql *NMSqLite_open();

void NMSqLite_close();

int NMSqLite_exec();

#endif