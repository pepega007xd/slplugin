#import "template.typ": *

#show: project

#page(
  margin: 0pt,
  image("titlepage.svg"),
)

#page(
  margin: 0pt,
  image("assignment.svg"),
)

#stack(
  // TODO: adjust after finalizing abstracts
  spacing: 10%,
  [
    #heading(numbering: none)[Abstract]

    This thesis describes the design and implementation of a verification tool for C programs, focusing on programs using linked lists. The idea is to derive the shape of the data structures that exist in the program's heap during execution. This is then used to prove correctness of all memory accesses in the program, and to detect possible invalid memory accesses. The approach is based on representing abstract program states using formulae of separation logic. It is implemented in the Frama-C framework, utilizing its dataflow analysis template, and value analysis for semantic constant propagation. The tool was benchmarked on the linked lists subset of SV-COMP benchmarks and compared with similar verification tools. While not reaching the performance of top competitors in this category, it is on par with most verifiers.
  ],
  [
    #heading(numbering: none)[Abstrakt]

    // TODO: translate when eng abstract is finished
  ],
  [
    #heading(numbering: none)[Keywords]

    verification, static analysis, separation logic, Astral, Frama-C, SV-COMP
  ],
  [
    #heading(numbering: none)[Klíčová slova]

    verifikace, statická analýza, separační logika, Astral, Frama-C, SV-COMP
  ],
  [
    #heading(numbering: none)[Reference]

    BRABLEC, Tomáš. _Static Analysis of Heap-Manipulating programs using Separation Logic._ Bachelor's Thesis. Tomáš DACÍK (supervisor). Brno: Brno University of Technology, Faculty of Information Technology, 2025.
  ],
)

#pagebreak()

#heading(numbering: none)[Rozšířený abstrakt]

Existuje nekolik zpusobu spravy pameti v programovacich jazycich. Tyto zpusoby se v zasade deli na automatickou spravu pameti, ktera vyuziva garbage collector (GC) k uvolnovani jiz nedosazitelnych alokaci, a manualni spravu pameti, kde musi kod programu explicitne uvolnovat alokace. Tento zpusob spravy pameti s sebou nese riziko nekolika typu chyb, kterych se muze programator pri psani kodu dopustit. Jednou z chyb je tzv. use-after-free, neboli pristup na adresu v jiz uvolnene alokaci. Dale jde napr. o double-free -- uvolneni jiz uvolnene alokace, nebo unik pameti, kdy nedojde k uvolneni alokace vubec. Nektere z techto chyb mohou vytvorit bezpecnostni zranitelnosti v softwaru, zneuzitelne napr. ke spusteni kodu.

// TODO: terminologie staticka analyza/verifikace/formalni verifikace -> vymylset jak to teda pouzivat
K odstranovani techto chyb se pouziva rada technik. Jednim pristupem je testovani a dynamicka analyza programu, napr. pomoci instrumentace kodu prekladacem. Dalsim je staticka analyza programu a formalni verifikace. Formalni verifikace umoznuje overit korektnost programu, pricemz se typicky zameruje na overeni nejake konkretni vlastnosti programu. Z hlediska pametove bezpecnosti nas zajima prave korektnost vsech pristupu do pameti.

Jednou z metod staticke analyzy je analyza toku dat (dataflow), ktera spociva v prochazeni grafu toku rizeni (CFG) programu a postupne aktualizaci moznych hodnot promennych programu. V kontextu analyzy prace s pameti jsou pro nas relevantni ukazatelove promenne. K reprezentaci hodnot ukazatelu se v techto analyzach casto vyuzivaji formule separacni logiky, ktera nam umoznuje efektivne reprezentovat datove struktury v dynamicke pameti.

// TODO: spatial predicate -> prostorovy predikat???
Cilem teto prace je vytvorit nastroj pro statickou analyzu programu v jazyce C, ktery bude schopen verifikovat korektnost prace s dynamicky alokovanou pameti se zamerenim na linearni seznamy. K tomu je vyuzita metoda analyzy toku dat, a stavy programu jsou reprezentovany pomoci formuli separacni logiky. K implementaci analyzy je vyuzit framework Frama-C, ktery poskytuje zjednodusenou formu AST analyzovaneho programu, knihovny pro transformace tohoto AST a pro implementaci dataflow analyzy nad timto AST. K vyhodnocovani splnitelnosti formuli separacni logiky je pouzit solver Astral. Overeni splnitelnosti formuli je nutne k nalezeni abstraktniho stavu pameti platneho pro cykly, ktery je nutne najit, aby mohla analyza skoncit. K tomu se vyuziva seznamovych predikatu v separacni logice, ktere popisuji ukazatelove retezce obecne delky.

Samotna implementace se sklada z nekolika casti. Prvne je to predzpracovani analyzovaneho AST, ktere slouzi nekolika ucelum. Jednim ucelem je zjednodusit komplexni vyrazy v prikazech prirazeni, argumentech funkci a podminkach. Dalsim ucelem je analyzovat struktury definovane v programu a urcit, ktery typ seznamu popisuji. Toto se provadi na zaklade heuristiky, ktera odvodi typ seznamu podle typu polozek struktury.

Dalsi casti implementace je analyza jednotlivych prikazu. Pri dosazeni prikazu je treba upravit formule reprezentujici stav programu pred vykonanim prikazu tak, aby popisovaly stav po vykonani prikazu. Tyto prikazy zahrnuji nekolik druhu dereferenci a pristupu na polozky struktury, alokace a uvolnovani pameti, a volani funkci.

// TODO: ...seznam o (obecne/neomezene/nezname) delce?
Implementace dale obsahuje zjednodusovani a abstrakce formuli na prechodu mezi dvema prikazy. Do teto kategorie spada napriklad odstranovani nedosazitelnych prostorovych predikatu z formuli (tyto jsou pak hlaseny jako unik alokace), odstranovani nepotrebnych promennych z ekvivalenci, nebo prejmenovani promennych, kterym v programu skoncil rozsah platnosti. Abstrakce spociva v nalezeni ukazatelovych predikatu ve formuli, ktere tvori retezec, a jejich spojeni do seznamovych predikatu, ktere popisuji seznam o obecne delce.

// TODO: doplnit dalsi operace co vyzkouset - sort?
Analyzator byl otestovan na rucne vytvorene sade testovacich programu, ktere pracuji se vsemi podporovanymi typy seznamu. Do testovanych operaci patri alokace seznamu o nedeterministicke delce, iterace skrz prvky seznamu, pridani a odstraneni prvku ze seznamu, obraceni poradi seznamu, dealokace seznamu. Analyzator byl schopen overit korektnost vsech techto jednoduchych programu. Pri pridani chyb do techto programu byl nastroj schopny tyto chyby detekovat. Mezi detekovane chyby se radi dereference ukazatelu na dealokovanou pamet, dereference ukazatele, ktery muze mit nulovou hodnotu, unik pameti, nebo dealokace jiz uvolnene pameti.

Dalsi testovani probehlo na datasetu ze souteze SV-COMP, konkretne na podmnozine benchmarku, ktera se zameruje na linearni seznamy. Tento dataset obsahuje celkem 134 programu testujicich velke mnozstvi operaci na mnoha typech seznamu. Do techto programu patri napr. razeni seznamu, pruchody a modifikace cyklickych seznamu nebo kombinace nekolika typu seznamu v jednom programu. Na tomto datasetu je nastroj schopny analyzovat 77 programu, z toho 73 jsou overeni korektnich programu, a zbyle 4 spravne vysledky jsou detekce chyb.

Soucasna implementace analyzy ma nekolik omezeni, ktere znemoznuji dosahnout lepsich vysledku. Prvne je to chybejici podpora analyzy ukazatelove aritmetiky. V soucasnem stavu neni mozne obecne reprezentovat ciselny offset do struktury ve formulich separacni logiky. Toto znemoznuje analyzovat nekolik desitek testu z SV-COMP datasetu, ktere pouzivaji ukazatelovou aritmetiku k pristupu do uzlu seznamu. Dalsim omezenim je absence analyzy ciselnych hodnot v programu, coz znemoznuje presnejsi analyzu podminek v cyklech. Ta je nutna napriklad k presnejsi detekci chyb v programech.

Nabizi se nekolik ruznych smeru budouciho rozsireni analyzatoru. Jednou moznosti je pouzit vysledky externiho analyzatoru hodnot promennych k presnejsi analyze podminek. Tento pristup je castecne uz pouzit v podobe predzpracovani kodu. Dalsi moznosti je pridat jednoduchou analyzu ciselnych hodnot primo do nastroje. Separacni logika, tak jak ji implementuje solver Astral, umoznuje vlozit do formuli termy v SMT, ktere mohou reprezentovat hodnoty ciselnych promennych.

#pagebreak()

#heading(
  numbering: none,
  text(
    "Static Analysis of Heap-Manipulating Programs using Separation Logic",
    hyphenate: false,
  ),
)

#v(2em)

#heading(numbering: none)[Declaration]

Prohlašuji, že jsem tuto bakalářskou práci vypracoval samostatně pod vedením Ing. Tomáše Dacíka. Další informace mi poskytl prof. Ing. Tomáš Vojnar, Ph.D. Uvedl jsem všechny literární prameny, publikace a další zdroje, ze kterých jsem čerpal.

#v(2em)

#align(right)[
  #line(length: 10em, stroke: (dash: "loosely-dotted"))
  Tomáš Brablec\
  May 8, 2025
]

#v(2em)

#heading(numbering: none)[Acknowledgements]

I would like to thank my supervisor Ing. Tomáš Dacík and my advisor prof. Ing. Tomáš Vojnar, Ph.D. for introducing me to the topic of software verification, and for their guidance and help during the work on this thesis.

#pagebreak()

#outline(target: heading.where(numbering: "1.1"))

#pagebreak()

= Introduction

// TODO: pridat vysledky, popis toolu, struktura prace

// - problem of memory safety bugs in software
// - C/C++
// - source of vulnerabilities
// - possible solutions (testing/dyn analysis -> not a proof of correctness, static an., formal verification)
// - Linked lists
// - SL

// TODO: add statistics/concrete examples of vulnerabilities?
There are two basic approaches to memory management in programming languages. One approach relies on a garbage collector to reclaim unreachable allocated memory automatically. The other, manual memory management, relies on the programmer to allocate and free memory explicitly. Some languages that rely on manual memory management allow for a group of bugs, called memory safety bugs, to occur. These contain _use-after-free_ -- access to freed memory, other kinds of invalid pointer dereferences, _double-free_ -- deallocating already freed memory, or _memory leaks_ -- a loss of reference to an allocation. These bugs are often a source of security vulnerabilities in affected programs, since invalid memory accesses are considered undefined behavior in C and C++.

// TODO: add examples? valgrind, asan, fuzzing? static analysis in detail in Current Approaches
There is a number of techniques to find these bugs and ensure memory safety of programs. One approach is testing and dynamic analysis, often by code instrumentation done by the compiler. Another approach is static analysis and formal verification. Formal verification is a collection of methods to prove correctness of a program, or to prove a specific property of a program. In this case, the property is memory safety, i.e. that every memory access in the program is valid and memory de/allocation is done correctly.

One of the approaches to static analysis is so-called _dataflow analysis_, which relies on traversing the control flow graph (CFG) of a program and tracking all possible values of variables in the program. In this context, the information we are interested in are the values of pointers, and the data structures in the program's heap.

Formulae of _separation logic_ are often used to represent the shapes of these data structures. Separation logic allows us to describe structures such as linked lists of unknown size using a single formula. Models of these formulae then represent concrete configurations of the heap.

The goal of this work is to develop a tool for static analysis of C programs, that is able to verify the correctness of handling dynamically allocated memory. The work focuses on programs opeating on variants of linked lists, as this is a common data structure in low-level software, where the C language still dominates.

The analyzer utilizes dataflow analysis to track what data structures would exist during the program's runtime. The tool is implemented using the Frama-C framework, which provides a simplified representation of the input program's AST with an API for transformations, and a library for implementing the dataflow analysis itself.

The program states tracked in the dataflow analysis are represented using the formulae of separation logic. To determine which states generated during the analysis are fully covered by other states, a solver for separation logic _Astral_ is used. This is needed to find invariants for loops in the program, which is required for the analysis to terminate.

The analyzer is then tested on example programs that work with all supported types of linked lists, trying common list operations such as construction, traversal, insertion into the list, reversing, deallocation, and others. The tool is also tested on a subset of the SV-COMP benchmarks dedicated to linked lists.

= Preliminaries

== Formal Verification

== Separation Logic <sl_chapter>

Separation logic (SL) @SL @SL2 is one of the most popular formalisms used for static analysis of programs that manipulate dynamically allocated memory. A formula of separation logic allows us to describe the program's heap so that models of such a formula, called _stack-heap models_, represent concrete structures of allocations and pointers between them. The main advantage of separation logic over other formalisms used for describing dynamic memory is the ability to describe disjoint parts of the heap using the _separating conjunction_ ($*$) operator. Another key component is a set of _inductive predicates_, which describe data structures of an unbounded size in a closed form.

Due to the high expressivity of general separation logic, existing solvers usually focus on a certain subset of all SL formulae, called _fragments_. One of these fragments commonly used for static analysis is called the _symbolic heap_, but it has a limitation in the use of boolean connectives.

The rest of this chapter describes a fragment of separation logic called _boolean separation logic_ as described in @astral, which, unlike other SL fragments, allows for a limited use of boolean operators -- conjunction, disjunction, and guarded negation ($phi and_not psi$). These are needed for the representation of lists with a non-zero minimum length, and for representing a state using a disjunction of multiple formulae.

=== Syntax


Variables in SL have a _sort_ representing the type of list the variable can be a part of. There are three predefined sorts, $SS$ for singly linked lists, $DD$ for doubly linked lists, and $NN$ for nested lists. We denote $x^SS$ as a variable of sort $SS$. Additional, generic sorts can be defined along with custom fields $upright(f_1), ..., upright(f_n)$ in its points-to predicate. In the grammar, this sort is denoted as $GG_n$. There is also a special variable nil without a sort.

The syntax of SL formulae $phi$ is described by this grammar:

$
  p :=& x^SS |-> n\
  & | x^DD |-> fields("n": n, "p": p)\
  &| x^NN |-> fields("t": t, "n": n)\
  &| x^(GG_n) |-> fields(upright(f_1) : f_1, ..., upright(f_n) : f_n) && "(points-to predicates)"\
  i :=& "ls"(x^SS,y^SS) | "dls"(x^DD, y^DD, p^DD, n^DD) | "nls"(x^NN, y^NN, z^SS) #h(2em) && "(spatial predicates)"\
  phi_A :=& x = y | x != y | freed(x) | emp | p | i && "(atomic formulae)"\
  phi :=& phi_A | exists x. thick phi | phi * phi | phi and phi | phi or phi | phi gneg phi && "(formulae)"
$

=== Semantics

#let Vars = [*Vars*]
#let Locs = [*Locs*]
#let Fields = [*Fields*]
#let freedloc = sym.times.square
#let domh = $"dom"(h)$

Let #Vars be a set of variables, #Locs a set of locations, and #Fields a set of fields. As mentioned, the model of a SL formula is called a stack-heap model, denoted as a pair $(s,h)$. The _stack_ is a partial function $#Vars -> #Locs$, and the _heap_ is a partial function $#Locs -> (Fields -> Locs)$. The partial function $(Fields -> Locs)$ can be written as an enumeration of all fields and their corresponding locations, for example $h(ell) = fields(upright(f_1) : ell_1, upright(f_2): ell_2)$. Let #domh be the domain of the partial function $h$.

The semantics of the special variable nil is that $nil in.not domh$. There is a special location $freedloc in #Locs$, used in the definition of the _freed_ predicate, its semantics are the same as for nil, $freedloc in.not domh$.

With this, the semantics of boolean separation logic are defined as follows:

$
  &(s,h) entl x = y && "iff" s(x) = s(y) "and" domh = emptyset\
  &(s,h) entl x != y && "iff" s(x) != s(y) "and" domh = emptyset\
  &(s,h) entl freed(x) && "iff"  h = {s(x) |-> fields("n": freedloc) }\
  &(s,h) entl emp && "iff" domh = emptyset\
  &(s,h) entl x |-> fields(upright(f_1) : f_1, ..., upright(f_n) : f_n) #h(2em) && "iff" h = {s(x) |-> fields(upright(f_1) : f_1, ..., upright(f_n) : f_n)}\
  & && #h(1em) "and" s(f_n) != freedloc "for each" n\
  &(s,h) entl phi_1 and phi_2 && "iff" (s,h) entl phi_1 and (s,h) entl phi_2\
  &(s,h) entl phi_1 or phi_2 && "iff" (s,h) entl phi_1 or (s,h) entl phi_2\
  &(s,h) entl phi_1 gneg phi_2 && "iff" (s,h) entl phi_1 gneg (s,h) entl phi_2\
  &(s,h) entl exists x . thick phi && "iff there is a location" ell "such that" (s[x |-> ell], h) entl phi\
  &(s,h) entl ls(x, y) && "iff" (s,h) entl x = y, "or"\
  & && #h(1em) s(x) != s(y) "and" (s,h) entl exists x'. thick x |-> x' * ls(x', y)\
  &(s,h) entl dls(x, y, p, n) && "iff" (s,h) entl x = y * p = n, "or"\
  & && #h(1em) s(x) != s(y), s(x') != s(y'), "and"\
  & && #h(1em) (s,h) entl exists x'. thick x |-> fields("n": x', "p": p) * dls(x', y, x, n)\
  &(s,h) entl nls(x, y, z) && "iff" (s,h) entl x = y, "or"\
  & && #h(1em) s(x) != s(y) "and"\
  & && #h(1em) (s,h) entl exists t', n'. thick x |-> fields("t": t', "n": n') * ls(n', z) * nls(t', y, z)\
$

The intuitive meaning behind the semantics is that:

Equality and inequality of variables behaves as expected, but note that they require the heap to be empty. Therefore, separating conjunction is used to join all atomic formulae instead of regular conjunction. For example, a heap with one allocation will be described as $x |-> y * y = nil$. The formula $x |-> y and y = nil$ would be unsatisfiable, since the atom $y = nil$ describes an empty heap.

The $freed(x)$ atom says that the variable $x$ is not allocated. It has been added for the purpose of this analysis to explicitly mark freed variables. The key property is that adding $freed(x)$ to a formula containing $x$ discards its models where $x$ aliases with another allocated variable. For example, the formula $x |-> y$ has two models, one where $x$ and $y$ are distinct variables and another where they alias:
$ s = {x |-> ell_0, y |-> ell_1}, h = { ell_0 |-> fields("n": ell_1)} $
$ s = {x |-> ell_0, y |-> ell_0}, h = { ell_0 |-> fields("n": ell_0)} $

Changing the formula to $x |-> y * freed(y)$ removes the second model.

The semantics of the points-to predicate and boolean atoms is as expected, note that the definition of the points-to predicate guarantees that no atom can explicitly point to the special location #freedloc. Otherwise, $x |-> y and freed(x)$ would have a model, where $s(y) = freedloc$.

// TODO: mozna pridat grafovy obrazky modelu?

The inductive predicates describe chains of pointers of an unbounded length:

- $ls(x,y)$ describes a singly linked list of length zero or more. In the first case, the the list is equivalent to $x = y$. Length one is equivalent to a simple pointer $x |-> y * x != y$. All other models contain unnamed allocated memory locations in the chain between $s(x)$ and $s(y)$. Note that $y$ itself is not allocated in any model.

- $dls(x, y, p, n)$ represents a doubly linked list of allocations. Unlike in the previous case, both $x$ and $y$ are allocated. The variables $p$ and $n$ represent the "p" and "n" field of the first and last allocation respectively. Similar to the singly linked list, the zero-length case is equivalent to $x = n * y = p$, and the length one is equivalent to a single points-to atom.

// TODO: mozna vysvetlit ten hack s vyjadrovanim nls2+ uz tady?
- $nls(x, y, z)$ describes a nested list. The top-level list leads through the "t" field from $x$ to $y$, but there is a sublist from each node's "n" field leading to $z$. The sublist itself is not a nested list, but a singly linked list. Just like the other lists, the list can be empty (equivalent to $x = y$), but note that even the case of length one contains a sublist of an unbounded length.

== Frama-C <frama_c>

// Describe Cil AST -> or not? maybe just stmt/exp representation

Frama-C @frama_c_paper is a framework for building analysis tools aimed at C programs. Frama-C itself is written mainly in the OCaml programming language. Unlike other tools, which focus purely on finding bugs using heuristics, Frama-C specializes in verification tools. The framework itself is composed of a kernel, multiple plugins, and a GUI to present the results of analyses. The kernel provides common functionality for multiple plugins. The main component is an adapted form of the _C Intermediate Language_ (CIL) @cil, constructed by the Frama-C kernel for use within plugins, as well as an API for its manipulation. CIL has the form of a abstract syntax tree (AST) of the input source code, with extra semantic information added. This includes types of variables, whether a variable is initialized at a certain node, and other information. Frama-C also transforms the input code, making operations like type casts explicit, and otherwise making the code more suitable for static analysis. For example, all `return` statements in a function are replaced with `goto` statements leading to a single `return` at the end of the function. `for` and `while` loops are replaced with a simple infinite loop with an exit condition inside. A complete description of CIL can be found in the module `Frama_c_kernel.Cil_types`. Modules `Cil`, `Cil_datatype`, `Cil_printer`, and `Ast_info` inside contain other useful functions for AST manipulation.

One of the functions of Frama-C is to help with the development of custom analyses in the form of plugins. Frama-C handles command-line argument parsing, reporting results and intermediate data from the analysis, and it exposes APIs for access to the input CIL.

=== Dataflow analysis <dataflow>

Besides a general framework for implementing analyses, Frama-C provides generic implementations of common algorithms used for static analysis. One of these is dataflow analysis, implemented in module `Dataflow2`. The purpose of dataflow analysis is to calculate all possible values of some kind for each node of the control flow graph (CFG) in the analyzed program. Nodes in a control flow graph correspond to statements in the original program, and directed edges represent all possible jumps between these statements.

Dataflow analysis starts by assigning an initial state to each node of the CFG, and then progressively updates the values stored in nodes using an implementer-provided _transfer function_, following the edges between them. The transfer function takes the statement of the node, and the newly calculated state of the previous node, and produces a new state for the current node. In Frama-C, this function is called `doInstr`.

When a node is reached more than once, the data for this node is computed again based on the data from the previous node, and then joined with the previous data stored for the node. The function that joins the old state with the new state is also provided by the implementer. The name of this function in Frama-C is `combinePredecessors`.

The implementer must also provide the following functions:

- `doStmt` -- this function is called before calling the transfer function, it receives the statement itself. The implementer has the option to stop the analysis of this statement or continue normally.
- `doGuard` -- this function is called when an `if` statement is reached. It receives the state from the previous node and the condition expression, and generates two states, each to be used in one of the branches.
- `doEdge` -- called between analyzing two statements. The function receives both statements, and the current state of the first statement. The function can modify this statebefore continuing with the transfer function of the second statement.

Note that this list is not exhaustive, but the implementation of other functions needed by the `Dataflow2` module is trivial and not relevant to the analysis itself.

=== Visitor mechanism <visitors>

Frama-C provides a convenient way to modify the AST of the analyzed program using a user-provided visitor object. The plugin constructs an object inheriting from a class `Visitor.frama_c_inplace`, and overrides some of the methods corresponding to the AST node that it visits. For example, when the plugin overrides the `vexpr` method, the method will be called at each expression of the AST. The method returns a value of type `Cil_types.visitAction`, which allows the visitor to either leave the node as is, change it, or continue with the visits of its children. It is also possible to visit all statements, variables, and even types.

=== Ivette <ivette_chapter>

Ivette is a desktop application written in TypeScript using a client-server architecture. Ivette, the client, asynchronously polls the server (Frama-C plugin) for data and displays it. The server has to first register what data has to be shown. To achieve real-time diplay of information when running an analysis, the plguin must occasionally call `Async.yield` to let the Ivette synchronize the data in the GUI application with the data generated by the running analysis. The API for registering and using Ivette can be found in the module `Server`, inside the library `frama-c-server.core`.

= Current Approaches

// verifiers from svcomp
// - compare [SL?] in predator/cpachecker to ours),
// - other analyzers? forester automata something something -> do not explain, just mention
// - infer, MS slayer
// other approachees using SL -> does anyone else use a dedicated SL solver? is there a SL solver other than Astral?

= Analysis

// no subchapters

// 1 or 2 pages with highlevel overview
// - Dataflow over CFG/CFA (is it worth to explain frama-c bs implementation in detail?)
// - Symbolic execution on SL formulae
// - Astral decides entailments in join and conditions on ifs (leave out other uses for now)
// - Abstraction leads to finding fixpoints for loops
// - Simplification helps to find fixpoints faster (or at all)
// - difference between program/fresh vars

The analysis itself consists of multiple steps. First, the analyzed program is preprocessed into a form more suitable for analysis. Then the dataflow analysis over the modified AST is executed.

== Preprocessing

The preprocessing consists of an external semantic constant propagation pass along with loop unrolling, followed by multiple custom passes over the AST. This external preprocessing is described in @const_prop. First, all AST nodes not relevant for the analysis are removed. This includes arithmetic and binary operations, type casts, integer comparison operations, and others. Then, variable initialization is replaced by simple assignments. Next, function call arguments with types irrelevant to the analysis are removed.

At this point, the AST should ideally only contain analyzable AST nodes. However, to simplify the implementation of the dataflow analysis, it is necessary to also split expressions in assignments, function call arguments, and conditions into a series of elementary assignments. Each complex statement is replaced by a block, containing temporary variables to which parts of the original expression are assigned. This is described in detail in @stmt_split.

After this, all assignments into variables with irrelevant types are removed or replaced by a check that the dereferenced variable is allocated. After this pass, all _instructions_ (statements not affecting control flow) should be one of the basic instruction types defined in #plugin_link("src/preprocessing/instruction_type.ml").

This is followed by a type analysis. As described in @sl_chapter, when introducing a variable into a formula, we need to provide a sort. This sort is derived from the type of the corresponding C variable. For types which form one of the supported kinds of lists (see @sl_chapter), a special predefined sort from Astral must be used, so that points-to atoms from these variables can later be abstracted to list atoms. For all other structure types, a new sort must be declared and registered in the solver instance. See @type_analysis for a detailed description.

Lastly, a list of all variables containing a pointer to another variable on the stack is collected. This is used later in the analysis since dereferences of these variables need special handling compared to variables pointing to the heap.

== Shape Analysis

The analysis itself is implemented using the `Dataflow2` module provided by Frama-C (see @dataflow), and it is composed of these main parts. First, there is the transfer function implemented for the basic instructions described in @stmt_split. Then there are the simplifications applied to the formulae between instructions. There is also the logic for the join operation. Finally, there is the specialization of formulae for each branch of a condition.

The analysis starts at the first statement of the `main` function, first assigning the

The transfer function takes formulae describing the state before executing an instruction and changes them to reflect the state after the execution.

= Implementation Details

== Program and Logic Variables <program_and_logic_vars>

== Preprocessing

// collecting stack allocated vars?

This chapter describes the details of AST preprocessing done externally by Frama-C and internally using the Cil Visitor mechanism (see @visitors). Many of the simple preprocessing passes are implemented directly in #plugin_link("src/preprocessing/preprocesssing.ml"). This module also contains the function `preprocess`, which runs all of the preprocessing passes in order. This is called directly from `main` before running the analysis.

Some of the preprocessing passes replace some expressions with special variables, namely `_nil` and `_const`. These are used to simplify the AST, instead of checking for multiple AST nodes such as a variable, a constant (zero), or cast of a constant into a pointer type (`NULL`), all expressions use only variables. `_nil` is used to represent a `NULL` pointer, whereas `_const` replaces any expression that cannot be analyzed, such as arithmetic expressions, or nondeterministic conditions. One other use of `_const` is that assignments into it are analyzed as "allocation check" of the assigned variable. For example, `_const = var;` is not interpreted as a literal assignment into the variable `_const`, but as a check that `var` is allocated. These assignments are created during preprocessing, when a field access to a non-pointer variable is found. For example, the following statement:

```c
int data = list->data;
```

will get converted to

```c
_const = list;
```

This will not cause any change to the state formulae during dataflow analysis, but each formula will be checked that the variable `list` is allocated.

=== Semantic Constant Propagation <const_prop>

The only kind of condition the analysis is able to process are equalities and inequalities of variables. This turned out to be a problem in many of the SV-COMP benchmarks (for example in `c/list-simple/sll2c_update_all.c`), because they often contained a variation of the following code:

```c
int len = 5;
...
while (len) {
  append_node(&list, data);
  len--;
}
...
access(list);
```

Because all other conditions are considered nondeterministic (see @conditions), the analysis will include the possibility of the number of loop iteations being zero. This means that the state after the loop will contain a formula describing the list having zero length. The problem is that the code below the loop will rightfully assume the list has non-zero length, accessing n-th node in the list without checking that the pointer to it is not `NULL`. The analysis will then detect a potential invalid dereference, and because a nondeterministic condition had been reached, it will return an unknown result.

The solution to this is to run an analysis of integer values on the program and statically determine the number of iterations over the loop. Integrating analysis of integer values into the dataflow pass itself would be a large change to the project late in development, so it was decided to use the _Semantic constant folding_ (SCF) plugin @scf_plugin shipped with Frama-c, which in turn uses the Eva @eva_user_manual plugin for value analysis. This is used in combination with Frama-C's loop unrolling setting `ulevel` to unroll a few iterations of every loop. This preprocessing pass will produce roughly the following code when used with setting `-ulevel=2`:

```c
int len = 5;

if (!5)
    goto unfolding_2_loop;
append_node(&list, data);
len = 4;

if (!4)
    goto unfolding_2_loop;
append_node(&list, data);
len = 3;

while (len) {
    append_node(&list, data);
    len--;
}

unfolding_2_loop:;
```

As you can see, the first two iterations of the loop have been unrolled before the loop body, and the variable `len` inside the unrolled conditional jumps was replaced by its value at that moment by the SCF plugin. These constant conditions are then removed altogether, see @conditions.

=== Preprocessing of Conditions <conditions>

Conditions are preprocessed in the following passes:

- removing constant conditions -- this is done before the removal of all constants from the AST, and therefore conditions left by the SCF pass (see @const_prop) are still intact. Using the `remove_const_conditions` visitor in #plugin_link("src/preprocessing/preprocessing.ml"), all condition expressions that can be folded to a constant using `Cil.constFoldToInt` are evaluated and removed from the code, replacing the original `If` AST node with the `Block` node of the branch that would be executed.

- removing the `!` operator -- using the visitor `remove_not_operator` in #plugin_link("src/preprocessing/preprocessing.ml"), the following replacements are made:
  - negation of negation is replaced by the inner expression
  - negation of equality is replaced by inequality
  - negation of inequality is replaced by equality
  These replacements are done because the evaluation of many macros in the SV-COMP programs creates exactly these structures, and simply removing them is easier than implementing the analysis to cover all of these.

- splitting the condition expressions -- the expression inside the condition is split the same way as other expressions described in @stmt_split. This pass is implemented in #plugin_link("src/preprocessing/condition_split.ml"). This pass also converts implicit pointer comparisons to explicit ones. Specifically, these replacements are made:
  - `(var)` is converted to `(var != _nil)`
  - `(!var)` is converted to `(var == _nil)`
  Conditions that cannot be preprocessed into a pointer comparison are replaced with the special constant `_const` instead. These are then considered nondeterministic during analysis.

After these passes, all analyzable conditions have the form of an equality or an inequality of two pointer variables. All other conditions simply contain the constant `_const`.

=== Splitting Complex Statements <stmt_split>

To simplify the implementation of the transfer function in the dataflow analysis, it is convenient to only implement it for a small set of simple instructions. Other, complex instructions then have to be replaced by blocks of these simple instructions with the same semantics.

These simple instructions are represented by the type `instr_type` in #plugin_link("src/preprocessing/instruction_type.ml"). These are the simple instructions:

// TODO: zapsat jako typst rovnici? jakoze Type := | Simple (var, var) | LHS_field (var, field, var) | ...

- assignment of a variable into a variable, eg. `var = var2;`
- assignment of a variable's field dereference into a variable, eg. `var = var2->field;`
- assignment of a variable into a dereferenced field of another variable, eg. `var->field = var2;`
- assignment of a variable into a pointer dereference, eg. `*var = var2;`
- assignment of a pointer dereference into a variable, eg. `var = *var2;`
- assignment of a reference to a variable into a variable, eg. `var = &var2;`
- function call with an optional assignment into a variable, with all arguments being variables. eg. `fun(var, var2);` or `var = fun(var2);`

To split any supported instruction into a series of simple instructions, this pass must introduce new variables to hold temporary values. Because the analysis handles program and logic variables differently (see @program_and_logic_vars), it is more convenient to give these variables as short of a lifetime as possible. Each split statement is therefore replaced with a block that holds the necessary temporary variables.

The preprocessing pass itself is implemented in #plugin_link("src/preprocessing/stmt_split.ml"), but the logic behind recursively splitting a single expression into a series of simple assignments, yielding a single variable, is implemented in #plugin_link("src/preprocessing/block_builder.ml"). This logic is also used by the preprocessing pass for conditions, see @conditions.

The splitting itself is done by storing the result of every single dereference into a temporary variable, using the previously created temporary variables in the latter statements. For example, the following statement:

```c
var1->field1->field2 = (*var2)->field3;
```

will be split into this block:

```c
{
  tmp1 = *var2;
  tmp2 = tmp1->field3; // tmp2 contains the value of the assigned expression
  tmp3 = var1->field1;
  tmp3->field2 = tmp2;
}
```

Note that it is not possible to extract the last l-value `tmp3->field2` into a temporary variable, and to assign the value of the expression into this variable, because that would not change the value in memory pointed to by the `tmp3` pointer.

=== Type Analysis <type_analysis>

The purpose of the type analysis is to determine, which C types implement one of the supported list variants, and to store this information for later use. This analysis pass is implemented in #plugin_link("src/preprocessing/types.ml").

The pass first iterates through all relevant types in the code and applies the following heuristic to determine if the type is one of the supported linked lists. The result of this heuristic is one of the following:

- Singly linked list
- Doubly linked list
- Nested list
- Generic struct

First, all relevant fields of the structure are recursively analyzed to get their heuristic result. Then,

- If the structure contains exactly one pointer to the structure itself and no pointers to structures marked as a singly linked list, it is itself marked as a singly linked list.
- If the structure contains two pointers to itself and no pointers to singly linked lists, it is marked as a doubly linked list.
- If the structure contains one pointer to itself and one pointer to a singly linked structure, it is marked as a nested list.
- Otherwise, it is marked as a generic structure. A new sort and Astral struct definition is created for its fields.

Then, each type is connected to its corresponding sort and struct definition in the `type_info` table. This is then used to retrieve sorts when adding new variables to formulae during analysis.

This method of analysis has a drawback. Not all implementations of supported linked lists will use one of these structures. For example, in SV-COMP benchmark #svcomp_link("c/forester-heap/sll-01-1.c"), the nested list structure is defined as follows:

```c
typedef struct TSLL
{
	struct TSLL* next;
	struct TSLL* inner;
} SLL;
```

During the program's runtime, the same structure is used to represent both the top-level list, and the nested sublists, which always set their `inner` field to NULL. Since this will be wrongly detected as a doubly linked list, the analysis will not be able to do abstraction on the generated formulae. Fixpoint for the list creation loop will not be found and the analysis will timeout.

There are a few possible ways to fix this. One option would be to rely on the user to provide the correct list type and field types manually using Frama-C annotations. Another option might be to detect the correct list type dynamically during analysis, based on the shapes of the data structures that are appearing in the formulae.

== Analysis

// describe order of calls doStmt, doInstr, doEdge, combinePredecessors, doGuard

== Representation of SL Formulae

// formula.ml
// program/fresh variables
// conversion to astral

== Abstraction

// describe the algorithm, possible improvements (configurable bounds)

== Simplification

// removing out of scope vars
// removing leaked spatial atoms
// removing unneeded fresh vars (substitution with nil/program vars)
// removing irrelevant fresh vars (distinct/freed only)
// removing single eq, empty lists
// canonicalization, fresh var names
// generalization of formulae (single atom bound difference)

// List.filter Astral_query.check_sat
// List.map (remove_ptos_from_vars end_of_scope_stack_vars)
// List.map (convert_vars_to_fresh end_of_scope_locals)
// List.map remove_leaks
// List.map reduce_equiv_classes
// do_abstraction
// List.map remove_irrelevant_vars
// List.map remove_single_eq
// List.map remove_empty_lists
// List.map (Formula.remove_spatial_from Formula.nil)
// deduplicate atoms syntactically *)
// List.map (List.sort_uniq compare)
// Formula.canonicalize_state
// List.map Formula.standardize_fresh_var_names
// Common.list_map_pairs generalize_similar_formulas
// deduplicate formulas syntactically *)
// List.sort_uniq compare
// deduplicate formulas semantically *)
// deduplicate_states

== Function Summaries

// mention anchor vars

== Join

// mention different sorting variants of formulae in [deduplicate_state]

== Reference Operator

// implementation
// removing references when going out of scope

== Configurability

// -sl-astral-debug    Print info about queries to Astral (opposite option is
//                     -sl-no-astral-debug)
// -sl-astral-encoding <Bitvectors | Sets>  Which location encoding should
//                     Astral use, default: Bitvectors
// -sl-backend-solver <Auto | Bitwuzla | CVC5 | Z3>  Which solver should be used
//                     by Astral, default: Auto
// -sl-benchmark-mode  Enables features needed to run benchmarks (opposite
//                     option is -sl-no-benchmark-mode)
// -sl-catch-exceptions  Catch exceptions in main function (disable for
//                     benchmarks) (set by default, opposite option is
//                     -sl-no-catch-exceptions)
// -sl-dump-queries    Dump Astral queries to 'astral_queries' directory.
//                     (opposite option is -sl-no-dump-queries)
// -sl-edge-abstraction  Do abstraction on every edge between stmts (default:
//                     abstraction is done on loop return) (opposite option is
//                     -sl-no-edge-abstraction)
// -sl-edge-deduplication  Deduplicate states using join (entailments) on every
//                     edge between stmts (opposite option is
//                     -sl-no-edge-deduplication)
// -sl-max-loop-cycles <N>  If set, the analysis will traverse loops only N
//                     times
// -sl-print-sort      Print sort of variables along with their names (opposite
//                     option is -sl-no-print-sort)
// -sl-simple-join     Compute join of states using entailment on single
//                     formulas (opposite option is -sl-no-simple-join)

= Results

// explain running with different ulevel settings

== Manual Testing

// maybe create a simple meaningful program and show that it can be analyzed -> postfix expr calculator?

== SV-COMP Benchmarks

= Conclusion

== Future Work

// better detection of list types -> acc to structures created during the analysis

// use freed(x!0) to represent newly allocated memory?

#pagebreak()

#bibliography("references.bib")
