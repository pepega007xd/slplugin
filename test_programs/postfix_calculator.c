#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// List types

typedef struct Node {
    double value;
    struct Node *next;
} Node;

typedef struct List {
    Node *head;
} List;

// List API

List *list_create() {
    List *list = malloc(sizeof(List));
    if (!list) {
        perror("Failed to create list");
        exit(EXIT_FAILURE);
    }

    list->head = NULL;
    return list;
}

void list_push(List *list, double value) {
    Node *new_node = malloc(sizeof(Node));
    if (!new_node) {
        perror("Failed to allocate node");
        exit(EXIT_FAILURE);
    }

    new_node->value = value;
    new_node->next = list->head;
    list->head = new_node;
}

double list_pop(List *list) {
    if (list->head == NULL) {
        fprintf(stderr, "Attempt to remove from an empty list\n");
        exit(EXIT_FAILURE);
    }
    // Remove the head node.
    Node *node_to_remove = list->head;
    double value = node_to_remove->value;
    list->head = node_to_remove->next;
    free(node_to_remove);
    return value;
}

// must be a macro since analysis cannot track bool values
#define list_is_empty(list) (list->head == NULL)

void list_destroy(List *list) {
    Node *current = list->head;

    while (current) {
        Node *next = current->next;
        free(current);
        current = next;
    }

    free(list);
}

// Program

#define BUFFER_SIZE 256

int main(void) {
    char input[BUFFER_SIZE];
    List *stack = list_create();

    printf("Enter a postfix expression:\n");

    while (fgets(input, BUFFER_SIZE, stdin) != NULL) {
        char *token = strtok(input, " \t\n");
        while (token != NULL) {
            if (isdigit(token[0]) || (token[0] == '-' && isdigit(token[1]))) {
                double num = atof(token);
                list_push(stack, num);
            } else {
                if (list_is_empty(stack)) {
                    fprintf(stderr, "insufficient operands for operator '%s'\n", token);
                    exit(EXIT_FAILURE);
                }

                double operand2 = list_pop(stack);

                if (list_is_empty(stack)) {
                    fprintf(stderr, "insufficient operands for operator '%s'\n", token);
                    exit(EXIT_FAILURE);
                }
                double operand1 = list_pop(stack);

                double result;
                switch (token[0]) {
                case '+':
                    result = operand1 + operand2;
                    break;

                case '-':
                    result = operand1 - operand2;
                    break;

                case '*':
                    result = operand1 * operand2;
                    break;

                case '/':
                    if (operand2 == 0) {
                        fprintf(stderr, "division by zero\n");
                        exit(EXIT_FAILURE);
                    }
                    result = operand1 / operand2;
                    break;
                default:
                    fprintf(stderr, "unknown operator '%s'\n", token);
                    exit(EXIT_FAILURE);
                }
                list_push(stack, result);
            }
            token = strtok(NULL, " \t\n");
        }

        if (!list_is_empty(stack)) {
            double final_result = list_pop(stack);
            if (!list_is_empty(stack)) {
                fprintf(stderr, "too many operands remain after evaluation\n");
            } else {
                printf("Result: %lf\n", final_result);
            }
        } else {
            fprintf(stderr, "no input provided\n");
        }

        printf("\nEnter another postfix expression:\n");
    }

    list_destroy(stack);

    return 0;
}
