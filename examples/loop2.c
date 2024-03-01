#include "stdio.h"
#include "stdlib.h"

// constructs a linked list using fallible malloc
// list ends with NULL ptr
// then walks the list to end

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
    *x = nullptr;

    while (start != nullptr) {
        temp = *start;
        start = temp;
    }
    return 0;
}
