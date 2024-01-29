// void exit(int _code) {
    // printf("exit %d\n", _code);
//     return;
// // }
// #include <stdlib.h>
// void Microsoft_FSharp_Core_Operators_Exit__void(int _code) { exit(_code); }
#include "Fable.Tests.C.fs.c"


typedef struct Foo {
    void* data;
    void (*func)(void*);
} Foo;

int main() {
    // Program_duTest();
    // Program_lambda_test();
    // Program_rcTest();
    // Runtime_clear_pool();
    // Program_fs();
    // Runtime_clear_pool();
    // Foo closure = {NULL, (void (*)(void*))main};
    // Fable_Tests_C_List_listCount();
    // Program_fs();
    ____assembly_init();
    Runtime_clear_pool();
    return 0;
}