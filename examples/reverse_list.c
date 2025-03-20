#include <stdio.h>
#include <stdlib.h>

typedef struct Node {
    struct Node *next;
    int data;
} Node;

typedef struct {
    Node *head;
    int len;
} List;

List *get_list() { return calloc(1, sizeof(List)); }

Node *append(List *list, int data) {
    Node *node = list->head;

    if (node == NULL) {
        list->head = malloc(sizeof(Node));
        node = list->head;
    } else {
        while (node->next != NULL) {
            node = node->next;
        }
        node->next = malloc(sizeof(Node));
        node = node->next;
    }

    if (node == NULL) {
        return NULL;
    }

    node->next = NULL;
    node->data = data;
    list->len++;
    return node;
}

void reverse(List *list) {
    Node *a = list->head;

    // cannot use list->len == 1
    if (a == NULL) {
        return;
    }

    Node *b = a->next;
    if (b == NULL) {
        return;
    }

    a->next = NULL;

    while (b != NULL) {
        Node *tmp = a;
        a = b->next;
        b->next = tmp;

        // swap a,b
        tmp = a;
        a = b;
        b = tmp;
    }

    list->head = a;
}

void free_list(List *list) {
    Node *node = list->head;
    while (node != NULL) {
        Node *next = node->next;
        free(node);
        node = next;
    }
    free(list);
}

int main() {
    List *list = get_list();
    if (list == NULL) {
        return 1;
    }

    append(list, -1);
    append(list, 0);
    append(list, 1);
    append(list, 42);

    reverse(list);
    Node *n = list->head;
    while (n != NULL) {
        printf("%d\n", n->data);
        n = n->next;
    }

    free_list(list);
}
