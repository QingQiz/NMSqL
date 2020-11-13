# IR

## IR分类

IR大致可以分为以下几类：

1. 普通指令：包括加减乘除、跳转、比较、栈操作等指令，这些指令是一些常见虚拟机都会有的指令
2. 操作组件，例如cursor、sorter、agg、set、keylist等，这些指令需要了解组件的使用方式
3. 各类构造指令，构造需要使用key或value
4. 操作数据库指令，例如数据库创建删除、索引创建删除
5. 文件相关指令

## IR列表 

下表为IR的使用方式及含义，N表示无此项，Y表示有此项。

|id|Opcode|P1|P2|P3|含义|
|-|----|-----|----|----|----|
|1|Transaction|N|N|N|开启一个事务，事务以commit或rollback或错误结束 halt不结束事务 在事务开始时数据库被上写锁|
|2|Commit|N|N|N|提交事务 释放写锁 当还有游标开启时保留读锁|
|3|Rollback|N|N|N|回滚事务 关闭所有游标 释放所有锁|
|4|ReadCookie|N|N|N|从数据库中读取cookie并压栈|
|5|SetCookie|Y|N|N|设置数据库cookie为P1|
|6|VerifyCookie|Y|N|N|验证数据库cookie为P1|
|7|Open|Y|Y|Y|以只读形式打开数据库文件中根页为P2的表 并给他一个游标标示为P1 P1是一个尽可能小的正整数 若P2为0 则出栈一个数作为P2 当数据库有游标时 会给数据库上读锁|
|8|OpenTemp|Y|N|N|在临时数据库中打开一个指向表的临时游标 此临时文件是可读写的 当游标关闭时临时文件被删除 游标的标识符为P1|
|9|OpenWrite|Y|Y|Y|以读写打开数据库文件中根页为P2的表 并给他一个游标标示为P1 P1是一个尽可能小的正整数 若P2为0 则出栈一个数作为P2 给数据库上读锁写锁|
|10|Close|Y|N|N|关闭标识符为P1的游标|
|11|MoveTo|Y|N|N|出栈一个元素 将其作为key 将标识符为P1的游标重定位为此key所在位置 若无此key 则指向它左侧的最近的元素|
|12|Fcnt|N|N|N|压栈当前虚拟机已执行的MoveTo指令数|
|13|NewRecno|Y|N|N|获取一个记录数压栈 此记录数应该在游标P1指向的数据库表中未出现过|
|14|Put|Y|N|N|将栈顶元素作为值 将下一个元素作为键 写入游标P1 出栈两个元素|
|15|Distinct|Y|Y|N|将栈顶元素作为key 若在游标P1中不存在 则跳转到P2 游标指向此key或其左侧最近元素|
|16|Found|Y|Y|N|出栈一个元素作为key 若在游标P1中存在 则跳转到P2 游标指向此key或其左侧最近元素|
|17|NotFound|Y|Y|N|出栈一个元素作为key 若在游标P1中不存在 则跳转到P2 游标指向此key或其左侧最近元素|
|18|Delete|Y|N|N|将P1游标此时所指元素删除 游标置为下一个或前一个元素 若置为下一个元素 则下一次使用Next操作会为空|
|19|Column|Y|Y|N|读取P1指向的数据 按照MakeRecord指令输出的形式取出第P2列的值并压栈 若开启了key-as-data模式 则处理的是P1的key (再研究一下)|
|20|KeyAsData|Y|Y|N|将P1游标的key-as-data模式开启(P2==1)或关闭(P2==0)|
|21|Recno|Y|N|N|将游标P1指向key的前4个字节作为一个int压栈|
|22|FullKey|Y|N|N|将游标P1指向的key作为字符串压栈|
|23|Rewind|Y|N|N|将游标P1重置(指向数据库第一个值)|
|24|Next|Y|Y|N|将游标P1移向下一个位置 若已经是最后一个值 则跳转到P2|
|25|Destroy|Y|N|N|删除一个根page为P1的表或索引|
|26|Clear|Y|N|N|清空一个根page为P1的表或索引|
|27|CreateIndex|N|N|Y|创建一个新索引 将此索引的页编号压栈 将新索引的根页写入数据库的根页中 将新索引的根页号写入P3指定的内存中|
|28|CreateTable|N|N|Y|创建一个新表 将此表的页编号压栈 将新表的根页写入数据库的根页中 将新表的根页号写入P3指定的内存中|
|29|Reorganize|Y|N|N|压缩 优化 根页编号为P1的表或索引|
|30|BeginIdx|Y|N|N|出栈一个元素作为key 此key需要是由MakeKey产生的 将游标P1移到此记录的位置 此指令后续应不断使用NextIdx指定|
|31|NextIdx|Y|Y|N|P1游标已指向一个索引位置 此指令不断访问下一条与BeginIdx中key相同的记录 若记录已结束 则跳转到P2|
|32|PutIdx|Y|Y|Y|出栈一个元素 此元素需要是MakeIdxKey的输出 在P1中插入此key 其对应的值为Nil 若P2为1则此key必须为非重复的 若P3为非Null 则P3是报错信息|
|33|DeleteIdx|Y|N|N|出栈一个元素 此元素需要是MakeIdxKey的输出 在P1中删除此key|
|34|MemLoad|Y|N|N|将P1内存位置的元素压栈|
|35|MemStore|Y|N|N|出栈一个元素 将其存至P1内存位置|
|36|ListOpen|Y|N|N|新建一个顺序表来暂时存储记录号 P1表示此表的标识符 若已存在 则将原P1表示的表关闭|
|37|ListWrite|Y|N|N|将栈顶的数字写入P1所指向的表中|
|38|ListRewind|Y|N|N|将P1指向的临时存储中 已读部分重置为此存储开始|
|39|ListRead|Y|Y|N|从临时存储P1中读取一个数字并压栈 若P1为空 则跳转到P2 此指令会将一度部分++|
|40|ListClose|Y|N|N|关闭并清空P1所指向的临时存储|
|41|SortOpen|Y|N|N|开启一个编号为P1的sorter|
|42|SortPut|Y|N|N|将栈顶作为key 栈中第二个元素为value 将其放入sorter中 出栈此两个元素|
|43|SortMakeRec|Y|N|N|将P1个元素出栈 构造成callback的参数并压栈 TODO:研究一下|
|44|SortMakeKey|Y|N|Y|出栈(P3长度)个元素 按照一个P3字符 一个元素 一个\000 的顺序构造串(第一个元素是栈顶) 将构造的串压栈 P1在SortCallback中使用|
|45|Sort|Y|N|N|对P1标识的Sorter排序|
|46|SortNext|Y|Y|N|将Sorter中的第一个元素压栈 并移除此元素 若Sorter为空 则跳转到P2|
|47|SortKey|Y|N|N|将Sorter中第一个元素的key压栈 不改变Sorter|
|48|SortCallback|Y|Y|N|栈顶需要是SortMakeKey的结果 并且P1相同(事实上不检测) 出栈此元素 作为xCallback的参数并执行xCallback|
|49|SortClose|Y|N|N|关闭P1所指定的Sorter并清空|
|50|FileOpen|N|N|Y|以只读形式打开P3指定的文件|
|51|FileRead|Y|Y|Y|从打开的文件中读取一行 若为EOF 跳转到P2 以P3为分隔符 若分割出的元素大于P1个 则忽略多余的元素 若小于P1个 则剩余的视为空字符串|
|52|FileColumn|Y|N|N|将最近读取行的第P1列压栈|
|53|FileClose|N|N|N|关闭之前打开的文件|
|54|AggReset|N|Y|N|清空当前aggregator 并扩展至可存储P2个元素|
|55|AggFocus|N|Y|N|出栈一个元素作为aggregator key 如果已有一个使用此key的aggregator 则将其作为当前aggregator并跳转到P2 否则创建一个新aggregator作为当前aggregator aggregator操作必须是AggReset AggFocus AggNext 不能在AggReset和AggNext中使用AggFocus|
|56|AggIncr|Y|Y|N|将当前focus aggregate element的P2位加P1|
|57|AggNext|N|Y|N|将下一个aggregate作为当前aggregate 前一个aggregate被删除 若无下一个 则跳转到P2|
|58|AggSet|N|Y|N|将当前栈顶元素放入当前aggregate的P2位|
|59|AggGet|N|Y|N|将当前aggregate的P2位入栈|
|60|SetInsert|Y|N|Y|向第P1个Set插入P3 若P3位Null 则出栈一个元素插入Set|
|61|SetFound|Y|Y|N|出栈一个元素 若此元素在第P1个Set中出现 则跳转到P2|
|62|SetNotFound|Y|Y|N|出栈一个元素 若此元素在第P1个Set中没出现 则跳转到P2|
|63|SetClear|Y|N|N|清空第P1个Set|
|64|MakeRecord|Y|N|N|将栈中P1个元素出栈 构造字符串作为Record并压栈 字符串头部有P1个2字节的整数代表偏移 然后是P1个字符串化的元素(NULL不操作) 元素顺序为 后出栈的元素在字符串前部|
|65|MakeKey|Y|Y|N|将栈中P1个元素构造字符串作为key并压栈 字符串由P1个字符串化的元素(NULL占一位)组成 元素顺序为 后出栈的元素在字符串前部 若P2为非0则P1个元素不出栈 否则P1个元素出栈|
|66|MakeIdxKey|Y|N|N|将栈中P1个元素字符串化 再将下一个元素出栈 作为整数放在字符串结尾(占4字节) 作为key并压栈 字符串由P1个字符串化的元素(NULL占一位)组成 元素顺序为 后出栈的元素在字符串前部|
|67|Goto|N|Y|N|向第P2条指令跳转|
|68|If|N|Y|N|一个元素出栈 若为true则跳转到P2 string长度0为false|
|69|Halt|N|N|N|结束程序|
|70|ColumnCount|Y|N|N|设置query结果列数|
|71|ColumnName|Y|N|Y|设置query结果第P1列(以0开始)的列名为P3|
|72|Callback|Y|N|N|出栈P1个元素 调用callback callback使用相关见[others-callback函数](#callback函数)|
|73|Integer|Y|N|N|将P1的值压栈|
|74|String|N|N|Y|将字符串P3(指针)压栈|
|75|Null|N|N|N|将Null压栈|
|76|Pop|Y|N|N|将P1个值出栈|
|77|Dup|Y|N|N|将第P1个值复制一份压栈(栈顶是第0个值)|
|78|Pull|Y|N|N|将第P1个值移至栈顶|
|79|Add|N|N|N|将栈顶两个元素出栈 将他们的和压栈(字符串转化为double)|
|80|AddImm|Y|N|N|栈顶元素加P1|
|81|Subtract|N|N|N|将栈顶两个元素出栈 将(后出栈的-先出栈的)压栈(字符串转化为double)|
|82|Multiply|N|N|N|将栈顶两个元素出栈 将他们的积压栈(字符串转化为double)|
|83|Divide|N|N|N|将栈顶两个元素出栈 将(后出栈的/先出栈的)压栈(字符串转化为double) 如果除0则将Null压栈|
|84|Min|N|N|N|将栈顶两个元素出栈 将他们中较小值压栈(字符串转化为double)|
|85|Max|N|N|N|将栈顶两个元素出栈 将他们中较大值压栈(字符串转化为double)|
|86|Like|Y|Y|N|两个元素出栈 先出栈的为pattern P1若为零 则匹配跳转到P2 P1若为非零 则不匹配跳转到P2 此命令支持的pattern为 %为任意字符 _为单字符|
|87|Glob|Y|Y|N|两个元素出栈 先出栈的为pattern P1若为零 则匹配跳转到P2 P1若为非零 则不匹配跳转到P2 此命令支持的pattern为 *为任意字符 ?为单字符 [...]匹配一系列字符 [^...]不匹配一系列字符|
|88|Eq|N|Y|N|两个元素出栈 若相等 则跳转到P2|
|89|Ne|N|Y|N|两个元素出栈 若不等 则跳转到P2|
|90|Lt|N|Y|N|两个元素出栈 若(后出栈的<先出栈的) 则跳转到P2|
|91|Le|N|Y|N|两个元素出栈 若(后出栈的<=先出栈的) 则跳转到P2|
|92|Gt|N|Y|N|两个元素出栈 若(后出栈的>先出栈的) 则跳转到P2|
|93|Ge|N|Y|N|两个元素出栈 若(后出栈的>=先出栈的) 则跳转到P2|
|94|IsNull|N|Y|N|出栈一个元素 若为Null跳转到P2|
|95|NotNull|N|Y|N|出栈一个元素 若不为Null跳转到P2|
|96|Negative|N|N|N|将栈顶元素取相反数|
|97|And|N|N|N|两个元素出栈 逻辑与压栈|
|98|Or|N|N|N|两个元素出栈 逻辑或压栈|
|99|Not|N|N|N|将栈顶元素取逻辑非|
|100|Concat|Y|Y|Y|将栈顶的P1个值以P3(字符串指针)为分隔符连接(栈顶在字符串左侧以此类推) 如果P2为0则这些值出栈，为1则不出栈 将结果压栈|
|101|Noop|N|N|N|do nothing|
|102|Strlen|N|N|N|栈顶元素为字符串 将此字符串替换为其长度|
|103|Substr|Y|Y|N|当P2或P1任一为0时 此值由出栈元素获得 先处理P2 再处理P1 出栈一个字符串 压栈 从P1位置开始P2长度的子字符串(字符串开始位置为1)|

## others

### callback函数

这玩意在`sqlite_exec`函数中传入(事实上在许多执行相关的函数都有传入)，对query结果的每一行进行使用，其类型是以下形式：

```cpp
typedef int (*sqlite_callback)(void*,int,char**, char**);
```

第一个参数是`sqlite_exec`的第四个参数，第二个参数是query结果中的列数，第三个参数是每一列的值，第四个参数是每一列的名字。

callback可以NULL，若为非空，则其返回值必须是0，否则会产生错。
