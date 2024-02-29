#include "stdio.h"
#include "stdlib.h"
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
        temp = __safe_malloc(sizeof(void *));

        if (temp != nullptr) {
            *x = temp;
            x = temp;
        } else {
            break;
        }
    }
    return 0;
}
