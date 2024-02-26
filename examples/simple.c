#include "stdio.h"
#include "stdlib.h"
int main() {
    // int a = 5;
    void *nullptr = NULL;
    void *x = malloc(sizeof(void *));

    if (x == nullptr) {
        (void)"then";
    } else {
        (void)"else";
    }
    x = nullptr;
    return 0;
}
