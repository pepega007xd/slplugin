#include <stdlib.h>

typedef struct SLL {
        struct SLL *next;
        int data;
} SLL;

typedef struct DLL {
        struct DLL *next;
        struct DLL *prev;
        int data;
} DLL;

typedef struct NL {
        struct NL *top;
        struct SLL *next;
        int data;
} NL;

int main() {
    SLL *sll = malloc(sizeof(SLL));
    sll->next = sll;
    sll->data = 42;

    DLL *dll = malloc(sizeof(DLL));
    dll->next = dll;
    dll->prev = dll;
    dll->data = 42;

    NL *nl = malloc(sizeof(NL));
    nl->top = nl;
    nl->next = sll; // singly linked list!
    nl->data = 42;

    // Tests
    dll->prev->prev = dll->next->next;

    dll->data = dll->next->data;
    {
        DLL *_1 = dll->next;
        int _out = dll->data; // special variable for non-list data
        dll->data = _out;
    }

    dll->next->prev = malloc(sizeof(DLL));

    // NULL as _nil
    dll->next = NULL;

    return 0;
}
