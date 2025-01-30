#include <stdlib.h>

typedef struct {
        int data;
        char *other_data;
        long another_data[128];
} Data;

typedef struct Node {
        struct Node *next;
        int data;
        char *other_data;
        Data *another_data;
} Node;

typedef struct {
        Node *head;
        int length;
} LinkedList;

LinkedList list_new() { return (LinkedList){.head = NULL, .length = 0}; }

Node *list_append(LinkedList *list, Data *data) {
    Node *node = list->head;

    if (node == NULL) {
        list->head = malloc(sizeof(Data));
        if (node->next == NULL) {
            return NULL;
        }

        node = list->head;
    } else {
        while (node->next != NULL) {
            node = node->next;
        }

        node->next = malloc(sizeof(Data));
        if (node->next == NULL) {
            return NULL;
        }

        node = node->next;
    }

    node->next = NULL;
    node->data = 42;
    node->other_data = "abc";
    node->another_data = data;

    return node;
}

int main() {
    LinkedList list = list_new();
    list_append(&list, NULL);
    list_append(&list, NULL);
    list_append(&list, NULL);
}
