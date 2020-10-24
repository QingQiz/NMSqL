#ifndef _BTREEINTERFACE_H
#define _BTREEINTERFACE_H

class BTreeIterator {
private:
    void* currentPlace;
public:
    int find(const void* key);

    void* getKey();
    void* getValue();

    int insert(const void* key, const void* value);
    int erase();

    int next();
    int reset();
};

#endif
