#include "stdio.h"
#include "stdlib.h"
int main() {
    // int a = 5;
    void *nullptr = NULL;
    void *x = malloc(sizeof(void *));

    if (x == nullptr) {
        puts("then");
    } else {
        puts("else");
    }
    x = nullptr;
    return 0;
}
