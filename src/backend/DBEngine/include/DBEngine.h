//
// Created by 17110 on 2020/11/16.
//

#ifndef DBENGINE_DBENGINE_H
#define DBENGINE_DBENGINE_H

struct DBECursor{
	void* key;
	void* cursor;
	void* rootPage;
};

#endif //DBENGINE_DBENGINE_H
