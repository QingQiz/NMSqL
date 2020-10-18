# IR

## IR列表 

下表为IR的使用方式及含义，N表示无此项，Y表示有此项。

|Opcode|P1|P2|P3|含义|
|----|-----|----|----|----|
|Goto|N|Y|N|向第P2条指令跳转|
|Halt|N|N|N|结束程序|
|Integer|Y|N|N|将P1的值压栈|
|String|N|N|Y|将字符串P3(指针)压栈|
|Null|N|N|N|将Null压栈|
|Pop|Y|N|N|将P1个值出栈|
|Dup|Y|N|N|将第P1个值复制一份压栈(栈顶是第0个值)|
|Pull|Y|N|N|将第P1个值移至栈顶|
|ColumnCount|Y|N|N|TODO: 需要研究一下运行方式|
|ColumnName|Y|N|Y|TODO: 需要研究一下运行方式|
|Callback|Y|N|N|TODO: 需要研究一下运行方式|
|Concat|Y|Y|Y|将栈顶的P1个值以P3(字符串指针)为分隔符连接(栈顶在字符串左侧以此类推) 如果P2为0则这些值出栈，为1则不出栈 将结果压栈|
|Add|N|N|N|将栈顶两个元素出栈 将他们的和压栈(字符串转化为double)|
|Multiply|N|N|N|将栈顶两个元素出栈 将他们的积压栈(字符串转化为double)|
|Subtract|N|N|N|将栈顶两个元素出栈 将(后出栈的-先出栈的)压栈(字符串转化为double)|
|Divide|N|N|N|将栈顶两个元素出栈 将(后出栈的/先出栈的)压栈(字符串转化为double) 如果除0则将Null压栈|
|Max|N|N|N|将栈顶两个元素出栈 将他们中较大值压栈(字符串转化为double)|
|Min|N|N|N|将栈顶两个元素出栈 将他们中较小值压栈(字符串转化为double)|
|AddImm|Y|N|N|栈顶元素加P1|
|Eq|N|Y|N|两个元素出栈 若相等 则跳转到P2|
|Ne|N|Y|N|两个元素出栈 若不等 则跳转到P2|
|Lt|N|Y|N|两个元素出栈 若(后出栈的<先出栈的) 则跳转到P2|
|Le|N|Y|N|两个元素出栈 若(后出栈的<=先出栈的) 则跳转到P2|
|Gt|N|Y|N|两个元素出栈 若(后出栈的>先出栈的) 则跳转到P2|
|Ge|N|Y|N|两个元素出栈 若(后出栈的>=先出栈的) 则跳转到P2|
|Like|Y|Y|N|两个元素出栈 先出栈的为pattern P1若为零 则匹配跳转到P2 P1若为非零 则不匹配跳转到P2 此命令支持的pattern为 %为任意字符 _为单字符|
|Glob|Y|Y|N|两个元素出栈 先出栈的为pattern P1若为零 则匹配跳转到P2 P1若为非零 则不匹配跳转到P2 此命令支持的pattern为 *为任意字符 ?为单字符 [...]匹配一系列字符 [^...]不匹配一系列字符|
|And|N|N|N|两个元素出栈 逻辑与压栈|
|Or|N|N|N|两个元素出栈 逻辑或压栈|
|Negative|N|N|N|将栈顶元素取相反数|
|Not|N|N|N|将栈顶元素取逻辑非|
|Noop|N|N|N|do nothing|
|If|N|Y|N|一个元素出栈 若为true则跳转到P2 string长度0为false|
|IsNull|N|Y|N|栈顶为Null跳转到P2|
|NotNull|N|Y|N|栈顶不为Null跳转到P2|
|MakeRecord|Y|N|N|将栈中P1个元素出栈 构造字符串作为Record并压栈 字符串头部有P1个2字节的整数代表偏移 然后是P1个字符串化的元素(NULL不操作) 元素顺序为 后出栈的元素在字符串前部|
|MakeKey|Y|Y|N|将栈中P1个元素构造字符串作为key并压栈 字符串由P1个字符串化的元素(NULL占一位)组成 元素顺序为 后出栈的元素在字符串前部 若P2为非0则P1个元素不出栈 否则P1个元素出栈|
|MakeIdxKey|Y|N|N|将栈中P1个元素字符串化 再将下一个元素出栈 作为整数放在字符串结尾(占4字节) 作为key并压栈 字符串由P1个字符串化的元素(NULL占一位)组成 元素顺序为 后出栈的元素在字符串前部|
|Transaction|N|N|N|开启一个事务，事务以commit或rollback或错误结束 halt不结束事务 在事务开始时数据库被上写锁|
|Commit|N|N|N|提交事务 释放写锁 当还有游标开启时保留读锁|
|Rollback|N|N|N|回滚事务 关闭所有游标 释放所有锁|
|ReadCookie|N|N|N|从数据库中读取cookie并压栈|
|SetCookie|Y|N|N|设置数据库cookie为P1|
|VerifyCookie|Y|N|N|验证数据库cookie为P1|
|Open|Y|Y|Y|以只读形式打开数据库文件中根页为P2的表 并给他一个游标标示为P1 P1是一个尽可能小的正整数 若P2为0 则出栈一个数作为P2 当数据库有游标时 会给数据库上读锁|
|Open|Y|Y|Y|以读写打开数据库文件中根页为P2的表 并给他一个游标标示为P1 P1是一个尽可能小的正整数 若P2为0 则出栈一个数作为P2 给数据库上读锁写锁|
|OpenTemp|Y|N|N|在临时数据库中打开一个指向表的临时游标 此临时文件是可读写的 当游标关闭时临时文件被删除 游标的标识符为P1|
|Close|Y|N|N|关闭标识符为P1的游标|
|MoveTo|Y|N|N|出栈一个元素 将其作为key 将标识符为P1的游标重定位为此key所在位置 若无此key 则指向它左侧的最近的元素|
|Fcnt|N|N|N|压栈当前虚拟机已执行的MoveTo指令数|
|Distinct|Y|Y|N|将栈顶元素作为key 若在游标P1中不存在 则跳转到P2 游标指向此key或其左侧最近元素|
|Found|Y|Y|N|出栈一个元素作为key 若在游标P1中存在 则跳转到P2 游标指向此key或其左侧最近元素|
|NotFound|Y|Y|N|出栈一个元素作为key 若在游标P1中不存在 则跳转到P2 游标指向此key或其左侧最近元素|
|NewRecno|Y|N|N|获取一个记录数压栈 此记录数应该在游标P1指向的数据库表中未出现过|
|Put|Y|N|N|将栈顶元素作为值 将下一个元素作为键 写入游标P1 出栈两个元素|
|Delete|Y|N|N|将P1游标此时所指元素删除 游标置为下一个或前一个元素 若置为下一个元素 则下一次使用Next操作会为空|
|KeyAsData|Y|Y|N|将P1游标的key-as-data模式开启(P2==1)或关闭(P2==0)|
|Column|Y|Y|N|读取P1指向的数据 按照MakeRecord指令输出的形式取出第P2列的值并压栈 若开启了key-as-data模式 则处理的是P1的key (再研究一下)|
|Recno|Y|N|N|将游标P1指向key的前4个字节作为一个int压栈|
|FullKey|Y|N|N|将游标P1指向的key作为字符串压栈|
|Rewind|Y|N|N|将游标P1重置(指向数据库第一个值)|
|Next|Y|Y|N|将游标P1移向下一个位置 若已经是最后一个值 则跳转到P2|
|BeginIdx|Y|N|N|
|NextIdx|Y|Y|N|
|PutIdx|Y|Y|Y|
|DeleteIdx|Y|N|N|