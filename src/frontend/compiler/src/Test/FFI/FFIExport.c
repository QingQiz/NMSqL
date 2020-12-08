#include <stdio.h>

extern void hs_init(int *argc, char***argv);
extern void hs_exit();

extern char* compile(char* sql);


int main(int argc, char**argv) {
    hs_init(&argc, &argv);
    char* s = compile("select * from xxx");
    printf("%s", s);
    hs_exit();
}
