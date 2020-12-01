//
// Created by 17110 on 2020/11/16.
//

#ifndef DBENGINE_DBENGINE_H
#define DBENGINE_DBENGINE_H

#include <string>

std::string extractString(const char *st);

struct DBECursor{
	void* key;
	void* cursor;
	void* rootPage;
};

int findPageByName(const char* name);



#endif //DBENGINE_DBENGINE_H
