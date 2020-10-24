#ifndef _DBENGINEINTERFACE_H
#define _DBENGINEINTERFACE_H
#include <string>

class Cursor { };

int createTable(std::string sql);

int reorganize();

void* getMetaData(std::string tableName);

int getTableCookies(char* tableName);

char** getTableColumn(char* tableName);
#endif
