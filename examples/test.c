#include "stdio.h"
#include "stdlib.h"

typedef struct List {
        struct List *next;
        int data;
} List;

int main() {
    List *start = malloc(sizeof(List));
    List *end = malloc(sizeof(List));

    start->next = end;
}
