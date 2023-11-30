#include "stdio.h"
#include "stdlib.h"

int main() {
    int *x = malloc(sizeof(int));
    int *nullptr = (void *)0;

    if (x == nullptr) {
        puts("then");
    } else {
        puts("else");
    }

    puts("after");

    x = nullptr;

    puts("end");
}
