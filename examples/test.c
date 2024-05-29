#include "stdio.h"
#include "stdlib.h"

int main() {
    int a = 5;
    int *x = NULL;
    int *y = NULL;

    // int *nullptr = NULL;
    // int *y = malloc(sizeof(int));
    while (x == NULL) {
        x = malloc(sizeof(int));
    }
    //
    // puts("");
    // int *y = malloc(sizeof(int));
    //
    // x = y;
    //
    // puts("after x = y");
    //
    // x = *y;
    //
    // puts("after x = *y");
    //
    // *x = y;
    //
    // puts("after *x = y");
}
