void exit(int _code) {
    // printf("exit %d\n", _code);
    return;
}
#include "Fable.Tests.C.fs.c"

int main() {
    // Program_duTest();
    // Program_lambda_test();
    Program_rcTest();
    Runtime_clear_pool();
    return 0;
}