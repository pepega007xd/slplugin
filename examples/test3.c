#include <stdlib.h>

typedef struct List {
        struct List *next;
        int data;
} SLL;

void traverse_list(SLL *s) {
    while (s != NULL) {
        int d = s->data;
        s = s->next;
    }
}

int main() {
    SLL *first = malloc(1);
    if (first == NULL) {
        return NULL;
    }
    first->next = malloc(1);
    if (first->next == NULL) {
        return NULL;
    }
    first->next->next = NULL;

    SLL *other = malloc(1);
    if (other == NULL) {
        return NULL;
    }
    other->next = first->next;

    traverse_list(first);

    other->next->data = 0; // test
}
