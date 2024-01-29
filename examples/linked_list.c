#include "stdio.h"
#include "stdlib.h"

typedef struct LinkedList {
        struct LinkedList *next;
        int data;
} LinkedList;

int main() {
    LinkedList *list;
    while (list->next != NULL) {
        list = list->next;
    }
    list->next = malloc(sizeof(LinkedList));
}
