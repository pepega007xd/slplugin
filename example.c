#include <stdlib.h>

typedef struct Node {
        struct Node *next;
        int data;
} Node;

int main() {
    Node *first = malloc(sizeof(Node));
    first->next = NULL;
    first->data = 0;

    Node *node = first;
    while (rand() % 10 > 0) {
        node->next = malloc(sizeof(Node));
        node = node->next;
        node->data = 0;
        node->next = NULL;
    }

    node = first;
    while (node != NULL) {
        node = node->next;
    }

    return 0;
}
