# VM


## 组件

### Cursor

指向一个数据库表或索引，Cursor可以遍历或到达一个指定的位置。

相关虚拟机指令:

+ Open
+ OpenWrite
+ OpenTemp
+ Close
+ MoveTo
+ Fcnt
+ Distinct
+ Found
+ NotFound
+ NewRecno
+ Put
+ Delete
+ KeyAsData
+ Column
+ Recno
+ FullKey
+ Rewind
+ Next
+ BeginIdx
+ NextIdx
+ PutIdx
+ DeleteIdx

### Sorter

使用归并排序对一系列元素排序。 

相关虚拟机指令:

+ SortOpen
+ SortPut
+ SortMakeRec
+ SortMakeKey
+ Sort
+ SortNext
+ SortKey
+ SortCallback
+ SortClose


### Agg

Aggregator，聚合器

相关虚拟机指令:

+ AggReset
+ AggFocus
+ AggIncr
+ AggSet
+ AggGet
+ AggNext

TODO: 详细研究使用方法以及功能

### Set

集合

相关虚拟机指令:

+ SetClear
+ SetInsert
+ SetFound
+ SetNotFound

都是常见的集合操作，使用标准库中的哈希表就可以实现


### Keylist

单向链表

相关虚拟机指令:

+ ListOpen 
+ ListWrite
+ ListRewind
+ ListRead
+ ListClose

## 需要dbengine提供的接口

暂时只能想到这么多

### Cursor

类似于迭代器，提供访问数据库内容的一种方式

Cursor是有状态的

需要的接口：

#### open(indexName,flag)

打开指定数据库表dbTable的指定名为indexName的Cursor，flag为只读、可写。

### close(cursor)

关闭cursor

#### create(dbTable,indexName,indexType,indexColumn)

对指定数据库表dbTable创建指定类型indexType的名为indexName的Cursor，cursor指定的列为indexColumn，对于正常顺序访问，则列为recordNumber。

#### find(cursor,key)

将cursor位置设置为第一个key的位置，若不存在，则为Null

#### findNext(cursor)

在find后使用，找到相同key的下一个位置。

#### getKey(cursor)

当前cursor指向位置的key值，若是Null，则返回Null

#### getValue(cursor)

当前cursor指向位置的value值，若是Null，则返回Null

#### getRecordNumber(cursor)

当前cursor指向位置的recordNumber值，若是Null，则返回Null

#### insert(cursor,key,value)

向cursor指向的表插入key/value对

#### erase(cursor)

删除cursor当前指向位置的key/value对，指向下一个位置

#### pre(cursor)

cursor前移

#### next(cursor)

cursor后移

#### reset(cursor)

重置到第一个位置

### 数据库相关操作

#### create(createTableSql)

建表

#### Reorganize(dbTable)

优化数据库表结构

#### getInfo(dbTable)

获取数据库表元信息

#### getNextRecordNumber(dbTable)

获取下一个记录编号，原子操作
