#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>

typedef int (*nmsql_callback)(void *, const int, const char *const *,
                              const char *const *);

extern void hs_init(int *, char ***);
extern void hs_exit();

extern int exec(const char *ir, nmsql_callback callback, void *args);
extern char *compile(char *sql);

extern void openDb(const char *dbName);
extern void closeDb();

static struct option long_options[] = {
    {"database-file", required_argument, 0, 'f'},
    {"help",          no_argument,       0, 'h'},
    {"sql",           required_argument, 0, 's'},
    {0,               0,                 0, 0},
};

struct NMSqL_Options {
    char *dbFile;
    char *sql;
} options;

int callback(void *pArgs, const int colNumber, const char *const *colData,
             const char *const *colNames) {
    for (int i = 0; i < colNumber; i++) {
        printf("name={%s},", colNames[i]);
        int len = ((int)colData[i][0]) << 8 | (colData[i][1]);
        int flag = colData[i][2];
        if (flag == 1) {
            int value = ((int)colData[i][4] << 24) |
                        ((int)colData[i][5] << 16) | ((int)colData[i][6] << 8) |
                        ((int)colData[i][7]);
            printf("type=int,value={%d}\n", value);
        } else if (flag == 2) {
            unsigned long long value = 0;
            for (int j = 0; j < 8; j++) {
                value = (value << 8) | (colData[i][j + 4] & 0xff);
            }
            printf("type=double,value={%f}\n", *(double *)&value);
        } else if (flag == 3) {
            printf("type=string,value={");
            for (int j = 0; j < len; j++) printf("%c", colData[i][j + 4]);
            printf("}\n");
        } else {
            printf("type=null");
        }
    }
    return 0;
}

char *ReadSQL() {
    int len = 2, i = 0;
    char *res = malloc(len + 1);

    printf("SQL> ");
    while (1) {
        char c;
        while (1) {
            c = getchar();
            if (c == -1) return 0;
            if (c == '\n') break;

            if (i >= len) {
                len *= 2;
                res = realloc(res, len + 1);
            }
            res[i++] = c;
        }

        while (1) {
            if (i == 0) break;
            if (res[i - 1] == ' ' || res[i - 1] == '\t')
                i--;
            else
                break;
        }

        if (res[i - 1] != ';') {
            res[i++] = ' ';
            printf("...> ");
        } else
            break;
    }
    res[i - 1] = '\0';
    return res;
}

void usage() {
    fputs(
"NMSqL version 0.1.0\n"
"\n"
"Usage :\n"
"NMSqL [options]\n"
"\n"
"OPTIONS\n"
" -h           --help                   Show help and exit\n"
" -f FILE      --database-file FILE     Specify to use FILE as the database file\n"
" -s SQL       --sql SQL                Specify SQL as the sql to be executed,\n"
"                                       if this option is not set, we will open a shell\n"
"\n"
"EXAMPLE\n"
" NMSqL -f tmp.db -s \"select * from xxx\"\n"
" NMSqL -f tmp.db\n"
"\n"
    ,stdout);
}

void ParseOptions(int argc, char **argv) {
    options.dbFile = NULL;
    options.sql = NULL;

    char isOptSet = 0;
    int optret, l_optidx;

    while (1) {
        optret = getopt_long(argc, argv, "hf:s:", long_options, &l_optidx);
        if (optret == -1) break;
        isOptSet = 1;

        switch (optret) {
            case 'f':
                options.dbFile = argv[optind - 1];
                break;
            case 's':
                options.sql = argv[optind - 1];
                break;
            case 'h':
            default:
                usage();
                exit(0);
        }
    }
    if (!isOptSet) {
        usage();
        exit(0);
    }
    if (options.dbFile == NULL) {
        usage();
        fputs("ERROR: database file is required.", stdout);
        exit(-1);
    }
}

int main(int argc, char **argv) {
    ParseOptions(argc, argv);

    hs_init(&argc, &argv);

    openDb(options.dbFile);

    int rt = 1;

    if (options.sql != NULL) {
        char *ir = compile(options.sql);

        if (ir[0] == 'S') {
            printf("%s\n", ir);
        } else {
            rt = exec(ir, callback, NULL);
        }
    } else {
        char *sql;
        while ((sql = ReadSQL())) {
            char *ir = compile(sql);
            if (ir[0] == 'S') {
                printf("%s\n", ir);
                break;
            }
            rt = exec(ir, callback, NULL);
        }
    }

    closeDb();
    hs_exit();
    return rt;
}
