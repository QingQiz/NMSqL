//
// Created by 17110 on 2020/11/16.
//

#ifndef DBENGINE_DBENGINE_H
#define DBENGINE_DBENGINE_H

#include <string>

std::string extractString(const char *st);

struct DBECursor{
    std::string key;
    std::string tableName;
    void* cursor;
};

int findPageByName(const char* name);

int getRowSize(char* tableName);

#endif //DBENGINE_DBENGINE_H
