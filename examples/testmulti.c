#include <stdlib.h>

typedef struct SLL {
        struct SLL *next;
        int data;
} SLL;

typedef struct NL {
        struct NL *top;
        struct SLL *next;
        int data;
} NL;

SLL *construct_sll() {
    SLL *start = calloc(1, 1);
    if (start == NULL) {
        return NULL;
    }

    SLL *s = start;
    int nondeterministic;
    while (nondeterministic) {
        s->next = calloc(1, 1);
        if (s->next == NULL) {
            break;
        }

        s = s->next;
    }

    s->next = NULL;
    return start;
}

NL *construct_nl() {
    NL *start = calloc(1, 1);
    if (start == NULL) {
        return NULL;
    }
    start->next = construct_sll();

    NL *s = start;
    int nondeterministic;
    while (nondeterministic) {
        s->top = calloc(1, 1);
        if (s->top == NULL) {
            break;
        }

        s = s->top;
        s->next = construct_sll();
    }

    s->top = NULL;

    return start;
}

void traverse_list(NL *s) {
    while (s != NULL) {
        int d = s->data;
        s = s->top;
    }
}

void free_list(NL *s) {
    while (s != NULL) {
        NL *next = s->top;
        free(s);
        s = next;
    }
}

int main() {
    NL *list = construct_nl();
    int a = 4;
    traverse_list(list);
    free_list(list);
}
