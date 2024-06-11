#include "stdlib.h"

// constructs a linked list using fallible malloc
// list ends with NULL ptr

int main() {
    void **start = malloc(sizeof(void *));
    if (start == NULL) {
        return 1;
    }

    { // x, temp
        void **x = start;
        int nondeterministic;
        void **temp = NULL;

        while (nondeterministic) {
            temp = malloc(sizeof(void *));

            if (temp != NULL) {
                *x = temp;
                x = temp;
            } else {
                break;
            }
        }
        *x = NULL;
    }

    free(start);

    return 0;
}
