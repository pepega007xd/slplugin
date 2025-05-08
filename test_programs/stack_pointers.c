typedef struct node {
    struct node *next;
    struct node *prev;
    int data;
} DLL;

void other(DLL **param, DLL *direct, int i) {
    DLL *local = *param;
    {
        DLL **nested = &local;
    }

    DLL **toplevel = &local;
}

int main(void) {
    {
        DLL *a = 0;
        DLL **b = &a;
        DLL *c = *b;
    }

    DLL *a = 0;
    other(&a, a, 42);
    return 0;
}
