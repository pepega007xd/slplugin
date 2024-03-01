#include "stdlib.h"

// constructs a linked list using fallible malloc
// list ends with NULL ptr

int main() {
    void **nullptr = NULL;
    void **x = malloc(sizeof(void *));
    if (x == nullptr) {
        return 1;
    }

    void **start = x;
    int nondeterministic;
    void **temp = NULL;

    while (nondeterministic) {
        temp = malloc(sizeof(void *));

        if (temp != nullptr) {
            *x = temp;
            x = temp;
        } else {
            break;
        }
    }
    *x = nullptr;

    return 0;
}
