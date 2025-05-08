#include <stdio.h>
#include <stdlib.h>

typedef struct Node {
    struct Node *next;
    int data;
} Node;

typedef struct {
    Node *head;
    int data;
    Node *tail;
    int len;
} List;

typedef struct {
    int len;
    List *tail;
    Node *head;
} A;

typedef struct {
    A *inner;
    int len;
    List *tail;
    Node *head;
    int data;
} B;

int main() {
    Node *node = malloc(1);
    Node *node2 = malloc(1);
    node->next = node2;

    node2->next = NULL;

    List *list = malloc(1);
    list->head = node;
    list->tail = node2;
    list->len = 42;
    list->data = 43;

    A *w = malloc(1);
    w->head = node;
    w->tail = list;

    B *b = malloc(1);
    b->inner = w;
    b->tail = list;
    b->head = node2;
    b->data = 42;

    int a = 4;
}
