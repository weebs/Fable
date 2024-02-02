#include "Quicktest.fs.c"

int main() {
    // QuickTest_Count* counter = QuickTest_returnsCounter();
    // void* items = QuickTest_array();
    QuickTest_effects(UNIT);
    Runtime_clear_pool();
    return 0;
}