#include "stdlib.h"

typedef struct LinkedList {
        struct LinkedList *next;
        int data;
} LinkedList;

int main() {
    LinkedList *list = malloc(sizeof(LinkedList));
    LinkedList *start = list;
    list->data = 4;
    list->next = NULL;

    int nondeterministic;
    while (nondeterministic) {
        list->next = malloc(sizeof(LinkedList));
        list->data = 4;
        list = list->next;
    }

    list->next = NULL;
}
