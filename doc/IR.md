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
|Glob|Y|Y|N|两个元素出栈 先出栈的为pattern P1若为零 则匹配跳转到P2 P1若为非零 则不匹配跳转到P2 此命令支持的pattern为 *为多字符 ?为单字符 [...]匹配一系列字符 [^...]不匹配一系列字符|
|And|N|N|N|两个元素出栈 逻辑与压栈|
|Or|N|N|N|两个元素出栈 逻辑或压栈|
|Negative|N|N|N|将栈顶元素取相反数|
|Not|N|N|N|将栈顶元素取逻辑非|
|Noop|N|N|N|do nothing|
|If|N|Y|N|一个元素出栈 若为true则跳转到P2 string长度0为false|
|IsNull|N|Y|N|栈顶为Null跳转到P2|
|NotNull|N|Y|N|栈顶不为Null跳转到P2|
|MakeRecord|Y|N|N||
|MakeKey|Y|Y|N||