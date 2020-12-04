/*
** shell部分代码
*/

/*
** 预处理
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sqlite.h"
#include <unistd.h>
#include <ctype.h>

#if defined(HAVE_READLINE) && HAVE_READLINE==1
# include <readline/readline.h>
# include <readline/history.h>
#else
# define readline getline
# define add_history(X) 
#endif

#define ArraySize(X)  (sizeof(X)/sizeof(X[0]))  //定义数组元素个数

#define MODE_Line      0  //每行一个值
#define MODE_Column    1  //左对齐的列
#define MODE_List      2  //由.separator字符分隔的值
#define MODE_Html      3  //HTML的<table>代码
#define MODE_Insert    4  //TABLE表的SQL插入语句

/*
** 获取输入并将其存储在malloc的内存中并返回一个指针，分配内存失败或者输入为空则返回EOF
*/
static char *getline0(char *prompt){
    char *pLine;
    int nLine;
    int n;
    int eol;

    if(prompt && *prompt){
        printf("%s", prompt);
        fflush(stdout);
    }

    nLine = 100;
    pLine = malloc(nLine);

    if(pLine == 0){
        return 0;
    }

    n = 0;
    eol = 0;

    while(!eol){
        if(n + 100 > nLine){
            nLine = nLine * 2 + 100;
            pLine = realloc(pLine, nLine);
            if(pLine == 0){
                return 0;
            }
        }

        if(fgets(&pLine[n], nLine - n, stdin) == 0){
            if(n == 0){
                free(pLine);
                return 0;
            }

            pLine[n] = 0;
            eol = 1;
            break;
        }

        while(pLine[n]){
            n++;
        }

        if(n > 0 && pLine[n - 1] == '\n'){
            n--;
            pLine[n] = 0;
            eol = 1;
        }
    }

    pLine = realloc(pLine, n + 1);
    return pLine;
}

/*
** 检索单行输入，来自终端flag为真，并调用readline函数，否则使用上面的getline0函数
** 
** last是前面检索过的，非空则表示还有后续
*/
static char *input_one_line(const char *last, int flag){
    char *prompt;
    char *result;

    if(!flag){
        return getline0(0);
    }

    if(last && last[0]){
        prompt = "   ...> ";
    }
    else{
        prompt = "sqlite> ";
    }

    result = readline(prompt);

    if (result)
    {
        add_history(result);
    }

    return result;
    
}

/*
** 回调数据结构
*/
struct callback_data{
    nmsql *db;
    int cnt;
    FILE *out;
    int mode;
    int showHeader;
    int escape;
    char destTable[250];
    char separator[20];
    int colWidth[100];
    int actualWidth[100]
}

/*
** 判断是否是数值
*/
static int is_numeric(const char * str){
    int seen_digit = 0;
    if(*str == '-' || *str == '+'){
        str++;
    }
    while(isdigit(*str)){
        seen_digit = 1;
        str++;
    }
    if(seen_digit && *str == '.'){
        str++;
        while(isdigit(*str)){
            str++;
        }
    }
    if(seen_digit && (*str == 'e' || *str == 'E')
     && (isdigit(str[1]) || ((str[1] == '-' || str[1] == '+') &&
      isdigit(str[2])))){
          str += 2;
          while(isdigit(*str)){str++;}
      }
    return seen_digit && *str == 0;
}

/*
** 输出带引号的字符串
*/
static void output_quoted_string(FILE *out, const char *z){
    int i;
    int nSingle = 0;
    int nDouble = 0;  //初始单引号和双引号计数为0

    for(i = 0; z[i]; i++){
        if(z[i] == '\''){
            nSingle++;
        }
        else if(z[i] == '"'){
            nDouble++;
        }
    }

    if(nSingle == 0){
        fprintf(out, "'%s'", z);
    }
    else if(nDouble == 0){
        fprintf(out, "\"%s\"", z);
    }
    else{
        fprintf(out, "'");
        while(*z){
            for(i = 0; z[i] && z[i] != '\''; i++){}
            if(i == 0){
                fprintf(out, "''");
                z++;
            }
            else if(z[i] == '\''){
                fprintf(out, "%.*s", i, z);  //输出的精度为i
                z += i + 1;
            }
            else{
                fprintf(out, "%s'", z);
                break;
            }
        }
    }
}

/*
** MODE_Html
*/
static void html_output_string(FILE *out, const char *z){
    int i;
    while(*z){
        for(i = 0; z[i] && z[i] != '<' && z[i] != '&'; i++){}
        
        if(i > 0){
                fprintf(out, "%.*s", i, z);
        }
        
        if(z[i] == '<'){
            fprintf(out, "&lt;");
        }
        else if(z[i] == '&'){
            fprintf(out, "&amp;");
        }
        else{
            break;
        }

        z += i + 1;
    }
}

/*
** 回调函数
*/
static int callback(void *pArg, int nArg, char **ppArg, char **ppCol){
    int i;
    struct callback_data *p = (struct callback_data*)pArg;
    switch(p -> mode){
        case MODE_Line:{
            if(p -> cnt++ > 0){
                fprintf(p -> out, "\n");
            }
            
            for(i = 0; i < nArg; i++){
                fprintf(p -> out, "%s = %s\n", ppCol[i], ppArg[i] ? ppArg[i] : 0);
            }

            break;
        }

        case MODE_Column:{

        }
    }
}

/*
** 
*/


/*
** 帮助信息，本来打算用sqlite3.0的帮助，但是感觉数据库没这些功能并且不太好实现
*/
static char HELP[] = 
  ".dump ?TABLE? ...      Dump the database in an text format\n"
  ".exit                  Exit this program\n"
  ".explain               Set output mode suitable for EXPLAIN\n"
  ".header ON|OFF         Turn display of headers on or off\n"
  ".help                  Show this message\n"
  ".indices TABLE         Show names of all indices on TABLE\n"
  ".mode MODE             Set mode to one of \"line\", \"column\", "
                                      "\"list\", or \"html\"\n"
  ".mode insert TABLE     Generate SQL insert statements for TABLE\n"
  ".output FILENAME       Send output to FILENAME\n"
  ".output stdout         Send output to the screen\n"
  ".schema ?TABLE?        Show the CREATE statements\n"
  ".separator STRING      Change separator string for \"list\" mode\n"
  ".tables ?PATTERN?      List names of tables matching a pattern\n"
  ".timeout MS            Try opening locked tables for MS milliseconds\n"
  ".width NUM NUM ...     Set column widths for \"column\" mode\n"
;
/*
static char HELP[] =
  ".archive ...             Manage SQL archives"
  ".auth ON|OFF             Show authorizer callbacks"
  ".backup ?DB? FILE        Backup DB (default "main") to FILE"
  ".bail on|off             Stop after hitting an error.  Default OFF"
  ".binary on|off           Turn binary output on or off.  Default OFF"
  ".cd DIRECTORY            Change the working directory to DIRECTORY"
  ".changes on|off          Show number of rows changed by SQL"
  ".check GLOB              Fail if output since .testcase does not match"
  ".clone NEWDB             Clone data into NEWDB from the existing database"
  ".databases               List names and files of attached databases"
  ".dbconfig ?op? ?val?     List or change sqlite3_db_config() options"
  ".dbinfo ?DB?             Show status information about the database"
  ".dump ?TABLE? ...        Render all database content as SQL"
  ".echo on|off             Turn command echo on or off"
  ".eqp on|off|full|...     Enable or disable automatic EXPLAIN QUERY PLAN"
  ".excel                   Display the output of next command in spreadsheet"
  ".exit ?CODE?             Exit this program with return-code CODE"
  ".expert                  EXPERIMENTAL. Suggest indexes for queries"
  ".explain ?on|off|auto?   Change the EXPLAIN formatting mode.  Default: auto"
  ".filectrl CMD ...        Run various sqlite3_file_control() operations"
  ".fullschema ?--indent?   Show schema and the content of sqlite_stat tables"
  ".headers on|off          Turn display of headers on or off"
  ".help ?-all? ?PATTERN?   Show help text for PATTERN"
  ".import FILE TABLE       Import data from FILE into TABLE"
  ".imposter INDEX TABLE    Create imposter table TABLE on index INDEX"
  ".indexes ?TABLE?         Show names of indexes"
  ".limit ?LIMIT? ?VAL?     Display or change the value of an SQLITE_LIMIT"
  ".lint OPTIONS            Report potential schema issues."
  ".load FILE ?ENTRY?       Load an extension library"
  ".log FILE|off            Turn logging on or off.  FILE can be stderr/stdout"
  ".mode MODE ?TABLE?       Set output mode"
  ".nullvalue STRING        Use STRING in place of NULL values"
  ".once (-e|-x|FILE)       Output for the next SQL command only to FILE"
  ".open ?OPTIONS? ?FILE?   Close existing database and reopen FILE"
  ".output ?FILE?           Send output to FILE or stdout if FILE is omitted"
  ".parameter CMD ...       Manage SQL parameter bindings"
  ".print STRING...         Print literal STRING"
  ".progress N              Invoke progress handler after every N opcodes"
  ".prompt MAIN CONTINUE    Replace the standard prompts"
  ".quit                    Exit this program"
  ".read FILE               Read input from FILE"
  ".recover                 Recover as much data as possible from corrupt db."
  ".restore ?DB? FILE       Restore content of DB (default "main") from FILE"
  ".save FILE               Write in-memory database into FILE"
  ".scanstats on|off        Turn sqlite3_stmt_scanstatus() metrics on or off"
  ".schema ?PATTERN?        Show the CREATE statements matching PATTERN"
  ".selftest ?OPTIONS?      Run tests defined in the SELFTEST table"
  ".separator COL ?ROW?     Change the column and row separators"
  ".sha3sum ...             Compute a SHA3 hash of database content"
  ".shell CMD ARGS...       Run CMD ARGS... in a system shell"
  ".show                    Show the current values for various settings"
  ".stats ?on|off?          Show stats or turn stats on or off"
  ".system CMD ARGS...      Run CMD ARGS... in a system shell"
  ".tables ?TABLE?          List names of tables matching LIKE pattern TABLE"
  ".testcase NAME           Begin redirecting output to 'testcase-out.txt'"
  ".testctrl CMD ...        Run various sqlite3_test_control() operations"
  ".timeout MS              Try opening locked tables for MS milliseconds"
  ".timer on|off            Turn SQL timer on or off"
  ".trace ?OPTIONS?         Output each SQL statement as it is run"
  ".vfsinfo ?AUX?           Information about the top-level VFS"
  ".vfslist                 List all available VFSes"
  ".vfsname ?AUX?           Print the name of the VFS stack"
  ".width NUM1 NUM2 ...     Set column widths for "column" mode"
*/

/*
** 处理以“.”开头的
*/
static void point_command(char *pLine, nmsql *db, struct callback_data *p){
    int i = 1;
    int nArg = 0;
    int n, c;
    char *pArg[50];

    //把输入分解
    while(pLine[i] && nArg < ArraySize(pArg)){
        while(isspace(pLine[i])){
            i++;
        }
        if(pLine[i] == '\'' || pLine[i] == '"'){
            int next = pLine[i++];
            pArg[nArg++] = pLine[i++];
            while(pLine[i] && pLine[i] != next){
                i++;
            }
            if(pLine[i] == next){
                pLine[i++] = 0;
            }
        }
        else{
            pArg[nArg++] = &pLine[i];
            while(pLine[i] && !isspace(pLine[i])){
                i++;
            }
            if(pLine[i]){
                pLine[i++] = 0;
            }
        }
    }

    //处理输入行
    if(nArg == 0){
        return;
    }
    n = strlen(pArg[0]);
    c = pArg[0][0];

    if(c == 'd' && strncmp(pArg[0], "dump", n) == 0){
        char *pErrMsg = 0;
        char pSql[1000];
        if(nArg == 1){
            sprintf(pSql, "SELECT name, type, sql FROM nmsql_master "
                          "WHERE type!='meta' "
                          "ORDER BY type tbl_name, type DESC, name");
            NMSqLite_exec();//待定
        }
        else{
            int i;
            for(i = 0; i < nArg && pErrMsg == 0; i++){
                sprintf(pSql, "SELECT name, type, sql FROM nmsql_master "
                              "WHERE tbl_name LIKE '%.800s' AND type!='meta' "
                              "ORDER BY type DESC, name", pArg[i]);
                NMSqLite_exec();//待定
            }
        }
        if(pErrMsg){
            fprintf(stderr, "Error: %s\n", pErrMsg);
            free(pErrMsg);
        }
    }
    else if(c == 'e' && strncmp(pArg[0], "exit", n) == 0){
        exit(0);
    }
    else if(c == 'e' && strncmp(pArg[0], "explain", n) == 0){
        p->mode = MODE_Column;
        p->showHeader = 1;
        p->colWidth[0] = 4;
        p->colWidth[1] = 12;
        p->colWidth[2] = 5;
        p->colWidth[3] = 5;
        p->colWidth[4] = 40;
    }
    else if(c == 'h' && strncmp(pArg[0], "header", n) == 0){
        int j;
        char *z = pArg[1];
        int value = atoi(pArg[1]);
        for(j = 0; z[j]; j++){
            if(isupper(z[j])){
                z[j] = tolower(z[j]);
            }
        }
        if(strcmp(z, "on") == 0){
            value = 1;
        }
        else if(strcmp(z, "yes") == 0){
            value = 1;
        }
        p -> showHeader = value;
    }
    else if(c == 'h' && strncmp(pArg[0], "help", n) == 0){
        fprintf(stderr, HELP);
    }
    else if(c == 'i' && strncmp(pArg[0], "indices", n) == 0 && nArg > 1){
        struct callback_data data;
        char *pErrMsg = 0;
        char pSql[1000];
        memcpy(&data, p, sizeof(data));
        data.showHeader = 0;
        data.mode = MODE_List;
        sprintf(pSql, "SELECT name FROM nmsql_master "
                      "WHERE type='index' AND tbl_name LIKE '%.800s' "
                      "ORDER BY name", pArg[1]);
        NMSqLite_exec();//待定
        if(pErrMsg){
            fprintf(stderr, "Error: %s\n", pErrMsg);
            free(pErrMsg);
        }
    }
    else if(c == 'm' && strncmp(pArg[0], "mdoe", n) == 0 && nArg >= 2){
        int n2 = strlen(pArg[1]);
        if(strncmp(pArg[1], "line", n2) == 0){
            p -> mode = MODE_Line;
        }
        else if(strncmp(pArg[1], "column", n2) == 0){
            p -> mode = MODE_Column;
        }
        else if(strncmp(pArg[1], "list", n2) == 0){
            p -> mode = MODE_List;
        }
        else if(strncmp(pArg[1],"html",n2) ==0 ){
            p -> mode = MODE_Html;
        }
        else if(strncmp(pArg[1],"insert",n2) == 0){
            p -> mode = MODE_Insert;
            if(nArg>=3){
            sprintf(p -> destTable, "%.*s", (int)(sizeof(p -> zDestTable) - 1), azArg[2]);
            }
            else{
            sprintf(p -> destTable, "table");
            }
        }
    }
    else if(c == 'o' && strncmp(pArg[0], "output", n) == 0 && nArg == 2){
        if(p -> out != stdout){
            fclose(p -> out);
        }
        if(strcmp(pArg[1], "stdout") == 0){
            p -> out = stdout;
        }
        else{
            p -> out = fopen(pArg[1], "w");
            if(p -> out == 0){
                fprintf(stderr, "can't write to \"%s\"\n", pArg[1]);
                p -> out = stdout;
            }
        }
    }
    else if(c == 's' && strncmp(pArg[0], "schema", n) == 0){
        struct callback_data data;
        char *pErrMsg = 0;
        char pSql[1000];
        memcpy(&data, p, sizeof(data));
        data.showHeader = 0;
        data.mode = MODE_List;
        if(nArg > 1){
            sprintf(pSql, "SELECT sql FROM nmsql_master "
                          "WHERE tbl_name LIKE '%.800s' AND type!='meta'"
                          "ORDER BY type DESC, name",
                          pArg[1]);
        }
        else{
            sprintf(pSql, "SELECT sql FROM nmsql_master "
                          "WHERE type!='meta' "
                          "ORDER BY tbl_name, type DESC, name");
        }
        NMSqLite_exec();
        if(pErrMsg){
            fprintf(stderr,"Error: %s\n", zErrMsg);
            free(pErrMsg);
        }
    }
    else if(c == 's' && strncmp(pArg[0], "separator", n) == 0 && nArg == 2){
        sprintf(p -> separator, "%.*s", (int)ArraySize(p -> separator) - 1, pArg[1]);
    }
    else if(c == 't' && n > 1 && strncmp(pArg[0], "tables", n) == 0){
        struct callback_data data;
        char *pErrMsg = 0;
        char pSql;
        memcpy(&data, p, sizeof(data));
        data.showHeader = 0;
        data.mode = MODE_List;
        if(nArg == 1){
            sprintf(pSql,
                "SELECT name FROM nmsql_master "
                "WHERE type='table' "
                "ORDER BY name");
        }
        else{
            sprintf(pSql,
                "SELECT name FROM nmsql_master "
                "WHERE type='table' AND name LIKE '%%%.100s%%' "
                "ORDER BY name", pArg[1]);
        }
        NMSqLite_exec();
        if(pErrMsg){
            sprintf(stderr, "Error: %s\n", pErrMsg);
            free(pErrMsg);
        }
    }
    /*else if(c == 't' && n > 1 && strncmp(pArg[0], "timeout", n) == 0 && nArg >= 2){

    }*/
    else if(c == 'w' && strncmp(pArg[0], "width", n) == 0){
        int j;
        for(j = 1; j < nArg && j < ArraySize(p -> colWidth); j++){
            p -> colWidth[j - 1] = atoi(pArg[j]);
        }
    }
    else{
        fprintf(stderr, "unknown command: \"%s\". Enter \".help\" for help\n", pArg[0]);
    }
}

/*
** 主函数，打开执行关闭的接口未定，还未写完
*/
int main(int argc, char * argv[]){
    nmsql *db;
    char *errMsg = 0;
    char *argv0 = argv[0];
    struct callback_data data;

    memset(&data, 0, sizeof(data));
    data.mode = MODE_List;
    strcpy(data.separator, "|");
    data.showHeader = 0;

    while(argc >= 2 && argv[1][0] == '-'){
        if(strcmp(argv[1], "-html") == 0){
            data.mode = MODE_Html;
            argc--;
            argv++;
        }
        else if(strcmp(argv[1], "-list") == 0){
            data.mode = MODE_List;
            argc--;
            argv++;
        }
        else if(strcmp(argv[1], "-line") == 0){
            data.mode = MODE_Line;
            argc--;
            argv++;
        }
        else if(argc >= 3 && strcmp(argv[0], "-seperator") == 0){
            sprintf(data.separator, "%.*s", (int)sizeof(data.separator) - 1, argv[2]);
            argc -= 2;
            argv += 2;
        }
        else if(strcmp(argv[1], "-header") == 0){
            data.showHeader = 1;
            argc--;
            argv++;
        }
        else if(strcmp(argv[1], "-noheader") == 0){
            data.showHeader = 0;
            argc--;
            argv++;
        }
        else{
            fprintf(stderr, "%s: unknown option: %s\n", argv0, argv[1]);
            return 1;
        }
    }

    if(argc != 2 && argc != 3){
        fprintf(stderr, "Usage: %s ?OPTIONS? FILENAME ?SQL?\n", argv0);
        exit(1);
    }

    data.db = db = NMSqLite_open();  //待商定

    data.out = stdout;

    if(argc == 3){
        if()  //报错，和NMSqLite_exec()相关，待定
    }
    else{

    }

    NMSqLite_close();
    return 0;
}

