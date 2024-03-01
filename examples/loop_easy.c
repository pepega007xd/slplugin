#include "stdlib.h"

// constructs a linked list using malloc that never fails
// list ends with dangling ptr

extern void **__safe_malloc();

int main() {
    void **nullptr = NULL;
    void **x = __safe_malloc();

    void **start = x;
    void **temp = NULL;

    int nondeterministic;
    while (nondeterministic) {
        temp = __safe_malloc();
        *x = temp;
        x = temp;
    }
    return 0;
}
