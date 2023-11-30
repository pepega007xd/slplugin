## Frama-C plugin for shape analysis using separation logic

build:

```bash
dune build
```

run:

```bash
dune exec -- frama-c simple.c
```

example output:

```
[kernel] Parsing simple.c (with preprocessing)

int *x = malloc(sizeof(int));
    (emp)
int *nullptr = (int *)0;
    (star (= nullptr nil) (pto x fresh!0))
    (star (= nullptr nil) (= x nil))
if (x == nullptr) puts("then"); else puts("else");
    (star (pto x fresh!0) (= nullptr nil))
    (star (= x nil) (= nullptr nil))
puts("then");
    (star (= x nil) (= nullptr nil))
puts("else");
    (star (pto x fresh!0) (= nullptr nil))
puts("after");
    (star (pto x fresh!0) (= nullptr nil))
    (star (= x nil) (= nullptr nil))
x = nullptr;
    (star (= x nullptr) (= nullptr nil) (pto fresh!1 fresh!0))
    (star (= x nullptr) (= nullptr nil) (= fresh!2 nil))
puts("end");
    (star (= x nullptr) (pto fresh!1 fresh!0) (= nullptr nil) (= fresh!3 nullptr))
    (star (= x nullptr) (= fresh!2 nil) (= nullptr nil) (= fresh!4 nullptr))
__retres = 0;
    (star (= x nullptr) (pto fresh!1 fresh!0) (= nullptr nil) (= fresh!3 nullptr))
    (star (= x nullptr) (= fresh!2 nil) (= nullptr nil) (= fresh!4 nullptr))
return __retres;
    (star (= x nullptr) (pto fresh!1 fresh!0) (= nullptr nil) (= fresh!3 nullptr))
    (star (= x nullptr) (= fresh!2 nil) (= nullptr nil) (= fresh!4 nullptr))
```
