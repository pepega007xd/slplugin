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
  spacing: 1fr,
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
There is a number of techniques to find these bugs and ensure memory safety of programs. One approach is testing and dynamic analysis, often by code instrumentation done by the compiler. Another approach is static analysis and formal verification. Formal verification is a collection of methods to prove correctness of a program, or to prove a specific property of a program. In this case, the property is memory safety, i. e. that every memory access in the program is valid and memory de/allocation is done correctly.

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


Variables in SL have a _sort_ representing the type of list the variable can be a part of. There are three predefined sorts, $SS$ for singly linked lists (SLS), $DD$ for doubly linked lists (DLS), and $NN$ for nested lists (NLS). We denote $x^SS$ as a variable of sort $SS$. Additional, generic sorts can be defined along with custom fields $upright(f_1), ..., upright(f_n)$ in its points-to predicate. In the grammar, this sort is denoted as $GG_n$. There is also a special variable nil without a sort.

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

We write $phi[b\/a]$ for the formula $phi$ in which all occurences of $a$ are replaced by $b$.

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
  &(s,h) entl freed(x) && "iff" h = {s(x) |-> fields("n": freedloc) }\
  &(s,h) entl emp && "iff" domh = emptyset\
  &(s,h) entl x |-> fields(upright(f_1) : f_1, ..., upright(f_n) : f_n) #h(2em) && "iff" h = {s(x) |-> fields(upright(f_1) : f_1, ..., upright(f_n) : f_n)}\
  & && #h(1em) "and" s(f_n) != freedloc "for each" n\
  &(s,h) entl phi_1 and phi_2 && "iff" (s,h) entl phi_1 and (s,h) entl phi_2\
  &(s,h) entl phi_1 or phi_2 && "iff" (s,h) entl phi_1 or (s,h) entl phi_2\
  &(s,h) entl phi_1 gneg phi_2 && "iff" (s,h) entl phi_1 gneg (s,h) entl phi_2\
  &(s,h) entl exists x . thick phi && "iff there is a location" ell "such that" (s[x |-> ell], h) entl phi\
  &(s,h) entl ls(x, y) && "iff" (s,h) entl x = y, "or"\
  & && #h(1em) s(x) != s(y) "and" (s,h) entl exists x'. thick x |-> x' * ls(x', y)\
  &(s,h) entl dls(x, y, p, n) && "iff" (s,h) entl x = n * y = p, "or"\
  & && #h(1em) s(x) != s(n), s(y) != s(p), s(p) in.not domh, "and"\
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

- $ls(x, y)$ describes a singly linked list of length zero or more. In the first case, the the list is equivalent to $x = y$. Length one is equivalent to a simple pointer $x |-> y * x != y$. All other models contain unnamed allocated memory locations in the chain between $s(x)$ and $s(y)$. Note that $y$ itself is not allocated in any model.

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

// TODO: popsat proc se to dela
// presunout basic instr sem, detaily o preprocessingu dolu do 5.x

The preprocessing consists of an external semantic constant propagation pass along with loop unrolling, followed by multiple custom passes over the AST. This external preprocessing is described in @const_prop. First, all AST nodes not relevant for the analysis are removed. This includes arithmetic and binary operations, type casts, integer comparison operations, and others. Then, variable initialization is replaced by simple assignments. Next, function call arguments with types irrelevant to the analysis are removed. _Relevant types_ are described in @type_analysis.

At this point, the AST should ideally only contain analyzable AST nodes. However, to simplify the implementation of the dataflow analysis, it is necessary to also split expressions in assignments, function call arguments, and conditions into a series of elementary assignments. Each complex statement is replaced by a block, containing temporary variables to which parts of the original expression are assigned. This is described in detail in @stmt_split.

After this, all assignments into variables with irrelevant types are removed or replaced by a check that the dereferenced variable is allocated. After this pass, all _instructions_ (statements not affecting control flow) should be one of the basic instruction types defined in #plugin_link("src/preprocessing/instruction_type.ml").

This is followed by a type analysis. As described in @sl_chapter, when introducing a variable into a formula, we need to provide a sort. This sort is derived from the type of the corresponding C variable. For types which form one of the supported kinds of lists (see @sl_chapter), a special predefined sort from Astral must be used, so that points-to atoms from these variables can later be abstracted to list atoms. For all other structure types, a new sort must be declared and registered in the solver instance. See @type_analysis for a detailed description.

Lastly, a list of all variables containing a pointer to another variable on the stack is collected. This is used later in the analysis since dereferences of these variables need special handling compared to variables pointing to the heap.

== Shape Analysis

The analysis starts at the first statement of the `main` function, first assigning the formula $emp$ to the first statement, and then running the analysis from this point. The formulae used by the analysis are not actually represented by the types provided by Astral, but using a custom, flattened formula type that is simpler to work with. The details are described in @formulae.

Variables in formulae have two variants, _program variables_ and _logic variables_. Program variables directly correspond to C variables currently in scope by having the same name. Logic variables correspond to memory locations which currently have no name in the analyzed program. In SL formulae, they are existentially quantified. More on this in @join_operation. In this text, we refer to logic variables as $f'_n$.

The analysis itself is implemented using the `Dataflow2` module provided by Frama-C (see @dataflow), and it is composed of these main parts. First, there is the transfer function implemented for the basic instructions described in @stmt_split. Then there are the simplifications applied to the formulae between instructions. There is also the logic for the join operation. Finally, there is the specialization of formulae for each branch of a condition.

=== Transfer Function

// TODO: popsat materializaci + odkazat, popsat ze se tam muze detekovat chyba

The transfer function takes formulae describing the state before executing an instruction and changes them to reflect the state after the execution. The transfer function always processes the input formulae one by one, applying the operation described below to each input formula (here called $phi$) separately. The transfer function is implemented for each of the 7 basic instructions:

- Assignment into the special `_const` variable `_const = a;` means that the formula is not changed, but $a$ is checked to be allocated. If not, an invalid dereference is reported. This corresponds to accesses to non-pointer fields in the original source code, see @vars_preprocessing for more details.

- For simple assignment `a = b;`, the resulting formula is $phi[f0\/b] * a = b$ where #f0 is a new logic variable. The renaming is done because the memory location originally referred to by $b$ still has to be represented in the formula.

- For an assignment with a field access on the right-hand side `a = b->f;`, the variable $b$ is first materialized in the formula, and then the target of $b$ at field $f$ is found (here called $t_b$). Then a regular assignment is done with $t_b$ as the assigned variable. The resulting formula is $phi[f'_0\/a] * a = t_b$.

- When processing an assignment to a field `a->f = b;`, $a$ is first materialized, then the points-to atom from $a$ is found and its field $f$ is changed to $b$. No substitution is done in this case because we are not overwriting the value of any variable.

- `a = *b;` is processed the same way as `a = b->f;`, except the "field" being dereferenced has a special name `_target`. Pointers to pointers are represented as pointers to structs with a single field `_target`.

// TODO: prepsat
- Since pointers to pointers are assumed to be allocated on the stack, `*a = b;` is interpreted as an assignment of $b$ into the variable pointed to by $a$. The target of $a$, called $t_a$ is found , and the formula is transformed as a regular assignment of $b$ into it. After that, the points-to atom from $a$ is changed to $a |-> fields("_target": t_a)$ (it was affected by the renaming done during the assignment).

- `a = &b;` is processed differently based on whether $a$ is already allocated (there is a points-to atom from $a$ already in the formula). If so, its `_target` field is simply set to $b$. If not, this atom is added. This is done to prevent adding a second points-to atom from $a$, which would make the formula unsatisfiable.

The remaining instruction is a function call. If the function is from the allocation API (`malloc`, `calloc`, and `free`), it is handled differently than a user-defined function.

- processing `a = malloc(size);` yields two formulae, one representing the successful allocation with the fields set to new logic variables: $phi * a |-> fields("f"_0: f0, "f"_1: f1, ...)$, the other one representing an allocation failure: $phi * a = nil$.

- `a = calloc(size);` is handled the same as `malloc`, except all fields of the allocated points-to are set to nil instead of new logic variables.

- `free(a);` is handled by materializing $a$ and then removing the points-to atom starting at $a$.

All other functions are handled by explicitly by splitting the formula to two parts by reachability from the function arguments, and then running the dataflow analysis recursively on the function, using the reachable subformula as the initial state. The resulting state of analyzing the function is taken from its return statement. To improve the performance, a cache of _function summaries_ is used, the input to the cache is a modified initial state, the output is a modified output state. The details of implementing this are in @summaries.

=== Simplifications

Between analyzing instructions, the state formulae are simplified and abstracted to ensure the termination of the analysis. The full description of some of these simplifications is done in @simplifications. The following simplifications are applied in this order:

// |> List.map (remove_ptos_from_vars end_of_scope_stack_vars)
// |> List.map (convert_vars_to_fresh end_of_scope_locals)
// |> List.map remove_leaks
// |> List.map reduce_equiv_classes
// |> List.map do_abstraction
// |> List.map remove_irrelevant_vars
// |> List.map remove_empty_lists
// |> Formula.canonicalize_state
// |> Common.list_map_pairs generalize_similar_formulas
// |> List.sort_uniq compare
// |> deduplicate_states

// TODO: mozna nechat jenom zajimavy simplifikace, a technikality jako odstranovani points-to na stack nechat na impl detaily?
- Program variables going out of scope by the transition between instructions are substituted with logic variables.
- Points-to atoms representing pointers to variables going out of scope are removed.
- Spatial atoms unreachable from program variables are removed and reported as memory leaks.
- Logic variables equivalent to program variables are removed from the formula by substituting them with these program variables.
- Chains of spatial predicates are abstracted to list predicates. This step ensures the analysis of loops will terminate. More on this in @abstraction.
- Inequalities and freed atoms containing logic variables only present in these atoms are removed. These atoms do not hold any information useful for the analysis.
- List atoms known to be empty are removed or replaced with equivalences. These are the products of previous simplifications and must be removed to not accumulate meaningless atoms in formulae. List atoms are assumed to be empty when the start and the end of the list are syntactically equivalent in the formula.
- The formulae are canonicalized, which involves sorting variables in equivalence classes, and sorting atoms in variables. This is done to make the next steps more likely to succeed.
- Formulae that differ only in the length bound of a single spatial atom are merged, taking the lower bound of the two as the new bound. Points-to atoms are treated as lists with the length bound set to one for this simplification.
- Formulae are deduplicated by syntactical comparison, and then semantically using the procedure described in @join_operation.

=== Join Operation <join_operation>

#let old = $phi_"old"$
#let new = $phi_"new"$
#let out = $phi_"out"$

When the analysis reaches a statement for the second or subsequent times, there is already a state associated with the statement. A new state is generated using the transfer function as usual, and then these states are joined to produce either a new state to be stored for the statement, or information that the original state covers the new state. By covering, we mean that all models of the new state are already included in the models of the original state. In another words, it must hold that $new entl old$.

As an optimization, this entailment is not evaluated as-is, but is broken into a series of smaller entailments. Because the evaluation of satisfiability and entailment checks is cached, this improves the efficiency of the caching because smaller entailemnts have a higher chance of multiple times during analysis.

The states #old and #new are first split into their formulae, and these lists are concatenated. This list of old and new formulae is first sorted (see @deduplication_sort) and then deduplicated using the following algorithm. A list of deduplicated formulae #out is created empty, and then, one by one, the formulae are added to this list. Before adding a formula $phi_0$ into $out = [phi_1, phi_2, phi_3, ...]$, entailments $phi_0 entl phi_1$, $phi_0 entl phi_2$, ... are tried and only if none of them succeed, $phi_0$ is added to #out.

After the deduplication, we must decide if the new state #out is covered by #old. Again, the entailment is split into a series of smaller entailments, each formula from #out is tested separately. The test of $phi_0$ from #out against $old = [phi_1, phi_2, ...]$ is done formula by formula, if at least one of the entailments $phi_0 entl phi_1$, $phi_0 entl phi_2$, ... succeeds, $phi_0$ is covered by the old state. Each formula from #out is tested this way, and if all of them are covered, the join opeation reports that #old covers the new state and the analysis does not continue beyond this statement.

=== Condition Evaluation

// TODO: mozna tomu proste rikat positive/negative branch?

When reaching a conditional statement, the analysis must decide whether to visit the "then" and "else" branch of the conditional, and how to modify the states used in the analysis of these branches. The only analyzed conditions are the equality and inequality of two pointer variables, all other conditions are treated as nondeterministic -- both branches are analyzed with the unchanged input state.

When reaching a condition `a == b`, the equality of $a$ and $b$ is added to the formulae for the "then" branch using `add_eq` described in @eq_distinct. Unsatisfiable formulae are then filtered out. If there are any remaining formulae, the analysis continues into the "then" branch with these. If not, the analysis does not continue into the branch at all. For the "else" branch, the algorithm is the same, only the inequality of $a$ and $b$ is added to the original formulae instead of equality. The function `add_distinct` is used for adding the inequality, as described in @eq_distinct.

When reaching a condition `a != b`, the approach is the same, except that the "then" and "else" branches are interchanged.

= Implementation Details

== Preprocessing

// collecting stack allocated vars?

This chapter describes the details of AST preprocessing done externally by Frama-C and internally using the Cil Visitor mechanism (see @visitors). Many of the simple preprocessing passes are implemented directly in #plugin_link("src/preprocessing/preprocesssing.ml"). This module also contains the function `preprocess`, which runs all of the preprocessing passes in order. This is called directly from `main` before running the analysis.

=== Preprocessing of variables <vars_preprocessing>

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

To split any supported instruction into a series of simple instructions, this pass must introduce new variables to hold temporary values. Because the analysis handles program and logic variables differently (see @formulae), it is more convenient to give these variables as short of a lifetime as possible. Each split statement is therefore replaced with a block that holds the necessary temporary variables.

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

The only _relevant types_ for the analysis are pointers to structures (`struct X *`). Pointers to these pointers (`struct X **`, `struct X ***`, etc.) are also supported, but only as pointers to variables on the stack. This is a limitation given only by the current implementation, as the support for pointers to variables on the stack was added late during development because many SV-COMP tests relied on this feature. However, SV-COMP does not contain any tests where pointers to pointers are allocated on the heap, so there was little reason to work on this specific feature over others.

The pass first iterates through all relevant types in the code and applies the following heuristic to determine if the type is one of the supported linked lists. The result of this heuristic is one of the following:

- Singly linked list
- Doubly linked list
- Nested list
- Generic struct

First, all relevant fields of the structure are recursively analyzed to get their heuristic result. Then,

- If the structure contains exactly one pointer to the structure itself and no pointers to structures marked as a singly linked list, it is itself marked as a singly linked list.
- If the structure contains two pointers to itself and no pointers to singly linked lists, it is marked as a doubly linked list.
- If the structure contains one pointer to itself and one pointer to a singly linked structure, it is marked as a nested list.
- Otherwise, it is marked as a generic structure. A new sort and Astral struct definition is created.

Then, each type is connected to its corresponding sort and struct definition in the `type_info` table. The struct definition contains the names and sorts of all fields in the structure.

This method of analysis has a drawback. Not all implementations of supported linked lists will use one of these structures. For example, in SV-COMP benchmark #svcomp_link("forester-heap/sll-01-1.c"), the nested list structure is defined as follows:

```c
typedef struct TSLL
{
	struct TSLL* next;
	struct TSLL* inner;
} SLL;
```

During the program's runtime, the same structure is used to represent both the top-level list, and the nested sublists, which always set their `inner` field to NULL. Since this will be wrongly detected as a doubly linked list, the analysis will not be able to do abstraction on the generated formulae. Fixpoint for the list creation loop will not be found and the analysis will timeout.

There are a few possible ways to fix this. One option would be to rely on the user to provide the correct list type and field types manually using Frama-C annotations. Another option might be to detect the correct list type dynamically during analysis, based on the shapes of the data structures that are appearing in the formulae.

== Representation of SL Formulae <formulae>

// formula.ml
// program/fresh variables
// conversion to astral

The type used to represent formulae in Astral has a tree structure that requires pattern matching to access every term. This makes sense for a solver that needs to support formulae of all shapes on its input, but for the purpose of this analysis, we need a flattened representation of the exact shape of formulae we use. This will avoid having to pattern match terms for which we already know the type, and it will allow us to define shorthands for predicates which have to be expressed by a more complex structure in Astral's type. The formula type along with a series of functions for manipulating these formulae are implemented in #plugin_link("src/formula.ml").

The type itself is defined as follows:

```ocaml
type var = SL.Variable.t
type ls = { first : var; next : var; min_len : int }
type dls = { first : var; last : var; prev : var; next : var; min_len : int }
type nls = { first : var; top : var; next : var; min_len : int }

type pto_target =
  | LS_t of var
  | DLS_t of var * var
  | NLS_t of var * var
  | Generic of (string * var) list

type atom =
  | Eq of var list
  | Distinct of var * var
  | Freed of var
  | PointsTo of var * pto_target
  | LS of ls
  | DLS of dls
  | NLS of nls

type formula = atom list

type state = formula list
```

State (type used by the dataflow module to represent the analysis state in each CFG node) is a list of formulae, and each formula is a list of atoms. `atom` is an enumeration of the predicates used by the analysis.

Variables used by the formulae are represented using the original Astral type. Program variables have the same name as their corresponding C variables, logic variables are distinguished by having names that contain the character `!`. When adding a logic variable to a formula, we need to give it a fresh name that does not appear anywhere in the formula. This is handled by Astral using `Variable.mk_fresh base_name sort`, which holds a counter of usages of each name, and adds a unique number `n` to the name in the form of `base_name!n`. Therefore, it was convenient to use the `!` character for distinguishing logic variables.

=== Spatial atoms

`PointsTo` represents all variants of points-to predicates. Because we need to distinguish between pointers of the three basic list types and all other pointers, the target(s) of the pointer are represented in a separate structure `pto_target`, with the three basic list variants, and a `Generic` variant which simply associates field names with variables.

List predicates are represented using three atoms, with a _length bound_ `min_len` indicating the minimum length of the list predicate. For singly linked lists, the maximum valid bound is 2+, and for doubly linked lists it is 3+. This is given by the limitations of encoding list predicates into Astral formulae, see @conversion_to_astral.


The modules also provides functions for manipulating spatial atoms, such as getting, changing, and changing the target of a points-to atom, checking that a variable is allocated in the formula, or converting points-to atoms to lists. To represent the type of field of a points-to atom, a `field_type` defined in type is used. It is defined in #plugin_link("src/preprocessing/types.ml") in the following way:

```ocaml
type field_type = Next | Prev | Top | Other of string | Data
```

The `Other` variant is used for representing arbitrary fields using their name.

Another operation implemented for formulae is _materialization_ of a variable inside a formula. This operation unrolls a single points-to predicate from a list predicate, so that subsequent operations can work with the points-to atom directly. By definition, it asserts that the materialized variable is allocated. Materialization involves adding a logic variable serving as the new start of the list predicate. When materialization is done on a variable that is already a source of a points-to atom, the formula is returned unchanged.

The function `materialize` is implemented recursively and returns a list of formulae, since materialization can produce more than one formula. When materializing a list with length bound 1+ or more, the length bound is simply decremented, and the points-to atom is added to the formula. When the list atom has the length bound 0+, a case split is performed:
- either the list had a minimum length at least one, in this case the length bound is incremented and `materialize` is called recursively using this modified formula,
- or the list atom had length zero, in this case it is replaced with an equality and `materialize` is called recursively.

Results of evaluating both cases are then concatenated and returned as a list of materialized formulae.

For example, materializing the variable $x$ in $ls(bound: 2, x, y)$ yields a single formula $x |-> f0 * ls(bound: 1, f0, y)$. Materializing $x$ in $ls(bound: 0, x, y) * ls(bound: 1, y, z)$ yields a list of two formulae:

$
  x |-> f0 * ls(bound: 0, f0, y) * ls(bound: 1, f0, y)\
  x = y * x |-> f0 * ls(bound: 0, f0, z)
$

The first formula represents the case where the list $ls(bound: 0, x, y)$ had length of at least one, the second represents the case of the list being empty. Note that materialization also handles explicitly making $x$ the source of the points-to atom.

Materialization of DLS variables is done similarly, with the difference that DLS lists can be materialized from any side. In $dls(x, y, p, n)$, either $x$ or $y$ can be materialized. During the materialization in NLS lists, an additional logic variable representing the start of the SLS sublist is introduced. For example, materializing $x$ in $nls(bound: 1, x, y, z)$ yields

$ x |-> fields("t": f0, "n": f1) * ls(f1, z) * nls(bound: 0, f0, y, z) $

where #f1 is the additional SLS variable.

=== Equality and Inequality <eq_distinct>

`Eq` represents a list of variables that form an equivalence class. Equivalences in formulae are not handled arbitrarily as pairs of equivalent variables, but a minimal list of disjoint equivalence classes is maintained throughout the analysis. When adding a new equivalence into a formula, the `add_eq lhs rhs` function is used. This function first checks if any of the newly equivalent variables is present in any of the existing equivalent classes, then:

- if both variables are already in a single equivalence class, nothing happens
- if both variables are in different equivalence classes, these classes are merged into one
- if just one variable is in an existing equivalence class, the other variable is added into it
- else, a new equivalence class with the two variables is added into the formula

`Ne` represents the inequality of two variables. Unlike equality, there is no need to represent a list of variables all distinct from each other. Nonetheless, there is a function `add_distinct lhs rhs` for adding distincts with special behavior. If there is a list atom in the formula with its source and destination equivalent to `lhs` and `rhs` of the inequality, the length bound of the list is increased to reflect this instead of adding an explicit inequality. Specifically,

- if $x != y$ is added to $ls(bound: 0, x, y)$, it becomes $ls(bound: 1, x, y)$
- if $x != n$ or $y = p$ is added to $dls(bound: 0, x, y, p, n)$, it becomes $dls(bound: 1, x, y, p, n)$
- if $x != y$ is added to $dls(bound: 0, x, y, p, n)$ or $dls(bound: 1, x, y, p, n)$, it becomes $dls(bound: 2, x, y, p, n)$
- if $x != y$ is added to $nls(bound: 0, x, y, z)$, it becomes $nls(bound: 1, x, y, z)$

This comes from the fact that all zero-length list predicates require the source and the target to be equal, by adding this inequality we essentially restrict the possible cases to those, where the list has length at least one. Similarly, in the third case, if the first and last allocated locations in a DLS differ, the list must have length at least two.

The reason for doing this is to prevent false detections of invalid dereferences when analyzing code that asserts a list to be nonempty using a condition, and then accesses this list. For example, consider the following code:

```c
list = construct_list();
if (list != NULL) {
  list->data = 42;
}
```

The state after the call to `construct_list` might be $ls(bound: 0, "list", nil)$. When analyzing condition, the inequality $"list" != nil$ will be added. If we simply added the inequality atom to the formula, we would detect a possible invalid dereference at the assignment. The code checking that a variable is allocated simply materializes the variable and checks that there is a spatial atom with its source equivalent to said variable. This check would fail for the formula $ls(bound: 0, "list", nil) * "list" != nil$. Increasing the bound on the list predicate solves this issue.

=== Translation to Astral Types <conversion_to_astral>

The translation of equality, inequality and `freed` atoms to the types of Astral's formulae is trivial, as is the translation of points-to atoms of the three predefined list types. These atoms have builtin constructors in Astral's modules `SL` and `SL_builtins`. Translating points-to atoms with sorts defined at runtime involves adding a struct definition for the variable's sort created during type analysis (see @type_analysis).

Translating list predicates involves expressing the length bounds implicitly, because Astral does not have a way to explicitly set the minimum length of a list predicate. Therefore, only $ls(bound: 0, x, y)$, $dls(bound: 0, x, y, p, n)$ and $nls(bound: 0, x, y, z)$ are translated directly to their corresponding list predicates in Astral's types. The rest is translated according to the following table:

#table(
  columns: 2,
  column-gutter: 2em,
  row-gutter: 1em,
  align: horizon,
  table.header(
    "Our formula",
    "Astral formula",
  ),

  $ls(bound: 1, x, y)$, $ls(x, y) * x != y$,
  $ls(bound: 2, x, y)$, $(ls(x, y) * x != y) gneg (x |-> y)$,
  $dls(bound: 1, x, y, p, n)$, $dls(x, y, p, n) * x != n$,
  $dls(bound: 2, x, y, p, n)$, $dls(x, y, p, n) * x != n * x != y$,
  $dls(bound: 3, x, y, p, n)$,
  $(dls(x, y, p, n) * x != n * x != y)\
  #h(5em) gneg\
  (x |-> fields("p": p, "n": y) * y |-> fields("p": x, "n": n))$,

  $nls(bound: 1, x, y, z)$, $nls(x, y, z) * x != y$,
  $nls(bound: 2, x, y, z)$,
  $(nls(x, y, z) * x != y)\
  #h(5em) gneg\
  (exists f0. thick x |-> fields("t": y, "n": f0) * ls(f0, z))$,
)

// TODO: popsat co presne je nls2+

The idea behind this translation is to force the list to be non-empty by adding an inequality for the 1+ cases, and explicitly discard the length one model using guarded negation ($gneg$) for the 2+ cases. Note that translating $nls(bound: 2, x, y, z)$ requires adding new logic variable #f0 existentially quantified _under_ the negation. If it was placed before the negation, the meaning would be different -- negation changes existential quantifiers to universal and vice versa.

== Interprocedural Analysis <summaries>

The analysis of a function call is implemented in #plugin_link("src/func_call.ml") and consists of multiple steps.

=== Reachability Split

First, the input formula is split to two subformulae by reachability from the function arguments. The reachable subformula is found by first finding all reachable spatial atoms. The algorithm starts with a single variable and finds the spatial atom leading from it. This atom is added to the set of reachable atoms, and the algorithm continues recursively with all targets of the spatial atom. This algorithm is ran for each of the argument variables.

After this, the set of all reachable variables is collected from the reachable spatials. The arguments themselves and nil are also included in this set. All equivalence classes are then filtered so that only reachable variables remain. Finally, inequalities are filtered so that only those in which both variables are reachable remain. Similarly, freed atoms are filtered.

The reachable subformula is a merge of reachable spatials, filtered equivalence classes, and filtered inequalities and freed atoms. The unreachable subformula is then composed of all atoms of the original formula not present in the reachable subformula.

This is done to simplify the analysis of the called function as much as possible by not including irrelevant atoms and variables in the formulae. It also increases the efficiency of the summaries cache because the cache input does not contain irrelevant data that would prevent cache hits with slightly different input formulae.

=== Anchor Variables

After extracting the reachable subformula, it is necessary to rename variables in the formula from argument names to parameter names. However, simply renaming the variables on function entry and then back on function return would also change the values of variables _after_ the call, for example:

```c
void set_to_null(List *x) {
  x = NULL;
  return;
}

...

a = allocate();
set_to_null(a);
assert(a != NULL); // "a" would be overwritten
```

For this reason, _anchor variables_ are added to the input formula before the renaming. Anchor variables are treated as regular program variables, otherwise simplifications might remove them from the formula. In the case of the program above, assuming the input formula is $ls(bound: 1, a, nil)$, the anchor variable $A_a$ will first be added as equal to $a$, and then $a$ will be renamed to the parameter name $x$. The resulting formula will then be $ls(bound: 1, x, nil) * x = A_a$.

After the analysis of the function, we will get the formula $ls(bound: 1, f0, nil) * f0 = A_a * x = nil$, in which the $A_a$ anchor will be renamed back to $a$, yielding the original formula $ls(bound: 1, a, nil)$ along with atoms that will be removed by subsequent simplification passes.

=== Function Summaries and Function Analysis

To avoid recomputing the analysis of a function call every time that the function is reached, a cache of _function summaries_ is maintained. A function summary is the mapping of an input formula to an output state. Input to the cache is a pair of the called function and the processed input formula after adding anchor variables and renaming arguements to parameters. Caching cannot be done on the original input formula, because arguemnt names will differ in different call sites. The conversion of the output state will assure that a summary created at one call site will be valid at any other call site.

If a summary is not found in the cache, the analysis of the function begins. Unfortunately, the implementation of dataflow analysis in Frama-C does not directly support interprocedural analysis, so we must manually backup the analysis context of the current function, and create a new context for the called function. This context consists of one hashmap for the analysis results and a second hashmap for storing the number of loop iterations (see @underapproximation_mode for more details). The context is represented by the type `function_context` in a global variable.

After creating the new context, the inital state for the first statement in the called function is set to the modified input formula, and the analysis is started on this statement. After the analysis finishes, the result state is read out from the result hashmap, and the original context is restored.

The result state must be processed before it is returned from the transfer function, each formula of the state is processed separately. First, the anchor variables are renamed back to the original argument names, then the unreachable subformula is added back to the resulting formula. After this, if the call instruction assigns the call result to a variable, the variable returned by the `return` statement from within the called function is assigned to the result variable. Frama-C preprocesses all functions to have a single `return` statement with a simple variable as the returned expression.

Finally, all points-to atoms representing pointers to variables on the stack created inside the function are removed, and then all program variables from inside the function are subsitituted with logic variables, to be removed by the subsequent simplification passes. Formulae processed this way are returned from the transfer function.

== Simplifications <simplifications>

// |> List.map (remove_ptos_from_vars end_of_scope_stack_vars)
// |> List.map (convert_vars_to_fresh end_of_scope_locals)
// |> List.map remove_leaks
// |> List.map reduce_equiv_classes
// |> do_abstraction
// |> List.map remove_irrelevant_vars
// |> List.map remove_empty_lists
// |> Formula.canonicalize_state
// |> Common.list_map_pairs generalize_similar_formulas
// |> List.sort_uniq compare
// |> deduplicate_states

// TODO: mozna rozdelit do podkapitol?

Most of the simplification passes are implemented in #plugin_link("src/simplification.ml") and called from the `doEdge` function inside #plugin_link("src/analysis.ml").

Substitution of program variables going out of scope by logic variables is done to enable the abstraction to work. Abstraction can only remove logic variables from the formula, so if variables created withing loops were not turned to logic variables at the end of the loop body, it would never have the option to introduce list predicates into the formula. As an example, consider the following code:

```c
List *a = allocate_node();
while (rand()) {
  List *b = allocate_node();
  a->next = b;
  b->next = NULL;
}
```

The formula describing the state at the end of the first loop iteration might be $a |-> b * b |-> nil$. Because the variable $b$ is going out of scope at the end of the loop body, the formula will be simplified to $a |-> f0 * f0 |-> nil$, which will enable the abstraction to $ls(bound: 2, a, nil)$ before the analysis returns to the start of the loop.

Spatial atoms starting at a logic variable are removed if the logic variable does not appear in any other spatial or equality atom. It can appear in distinct atoms, because inequalities do not make the logic variable reachable from any program variable in the formula. The removal of a spatial atom is reported as a memory leak, except when removing a list with the length bound of zero. These are not reported because it would cause false detections of memory leaks. For example, when unrolling a nested list, an SLS atom with the length bound zero is added, no matter the original sublist length.

Removing logic variables in equivalence classes is done by first filtering out equivalence classes with less than two variables. Then, in each equivalence class, a substitution target variable is found according to the following rules in order:
- If the equivalence class contains nil, it is used as the substitution target.
- If the equivalence class contains a program variable, it is used.
- Otherwise, a logic variable is used.

All logic variables are then substituted with this subsitution target. After this, all equivalence classes are deduplicated, getting rid of the variables duplicated during substitution. Finally, equivalence classes with less than two variables are again removed.

Formula canonicalization is done in these steps:
- All variables of the formula are collected and sorted, and then explicitly set as the source of spatial atoms. Variables that are not a source of any spatial atom are skipped.
- Variables in equivalence classes and inequalities are sorted.
- Atoms in the formula are sorted.
- All logic variables in the formula are in order renamed to a sequence of names with the form `!0`, `!1`, `!2`, ...

This is certainly not an ideal way to canonicalize formulae, but it proved sufficient for the next step to work.

Generalization of similar formulae is done only when two formulae differ in the length bound of a single spatial atom. This comparison is run for every pair of formulae in the state. If it succeeds, the pair is replaced with the generalized formula, otherwise it is left unchanged. The length bound in the generaized formula is the smaller of the original two. If two formulae differ in a single atom and one of the differing atoms is a points-to atom, it is converted to a list atom with the length bound of one. If the generalization fails, this change is not propagated to the original state. This simplification solves the common problem where an allocation loop produces multiple formulae describing different lengths of a list.

```c
a = NULL;
while (rand()) {
  alloc_node(&a);
}
```

This analysis might produce the following state after the loop:

$
  a = nil \
  a |-> nil\
  ls(bound: 2, a, nil)
$

This generalization will merge the latter formulae into one:

$
  a = nil \
  ls(bound: 1, a, nil)
$

This generalization could be extended to turn the resulting state into $ls(bound: 0, a, nil)$ that would cover all cases, but it would require a more complex method than to just compare a single pair of differing atoms, because the equality of $a$ and nil could exist inside a larger equivalence class.

== Abstraction <abstraction>

// describe the algorithm, possible improvements (configurable bounds)
The abstraction operates on each formula of the state separately, the abstraction of each list type is done in a separate pass. In general, the point of abstracting chains of points-to predicates into list predicates is to create invariants for loops. Consider the following code:

```c
a = alloc_node();
b = a;

while (rand()) {
  b->next = alloc_node();
  b = b->next;
}
```

If there was no abstraction in place, the analysis would generate longer and longer points-to chains forever:

// TODO: mozna odebrat zarovnani?
$
  a |-> nil &* a = b\
  a |-> b &* b |-> nil\
  a |-> f0 &* f0 |-> b &&* b |-> nil\
  a |-> f0 &* f0 |-> f1 &&* f1 |-> b * b |-> nil\
  \
  dots.c
$

#let abstr = $ls(bound: 2, a, b) * b |-> nil$

The abstraction will instead turn the third formula into #abstr, which will abstract over all possible lengths of the points-to chain that would otherwise be generated. This formula (along with the first two generated formulae) will serve as the fixpoint for this loop (state that is valid in every iteration of the loop). This is because all possible lengths of the points-to chain have models that together make the set of models of the abstracted formula:

$
  a |-> f0 &* f0 |-> b &&* b |-> nil &entl abstr\
  a |-> f0 &* f0 |-> f1 &&* f1 |-> b * b |-> nil &entl abstr\
  a |-> f0 &* f0 |-> f1 &&* f1 |-> f2 * f2 |-> b * b |-> nil #h(2em) &entl abstr\
  \
  dots.c
$

=== Singly Linked Lists

Abstraction of SLS lists is done by iterating through all spatial atoms with the appropriate sort, and trying to find a second spatial atom starting at the end of the first one. Points-to atoms are again treated as list atoms with length bound equal to one. For example, when processing $ls(bound: 0, x, y)$, the algorithm would look for a spatial stom starting at $y$. If found, the following conditions must be met to proceed with the abstraction:

- The middle variable must be a logic variable not reachable from the rest of the formula. Joining $ls(bound: 0, x, y)$ and $ls(bound: 1, y, z)$ would loses information about the variable $y$. Reachability is checked simply by counting the occurenes of the variable in all spatial and equivalence atoms.

- The source variable of the first list must be distinct from the target variable of the second list. In the previous example, this would mean ensuring that $x != z$. This is done using the solver, by adding an equality of the two variables into the formula and checking satisfiability. If the formula is satisfiable, the variables are not distinct and the abstraction cannot be done, because the predicate $ls(x, y)$ describes an _acyclic_ chain of pointers.

If the conditions are met, the two atoms are replaced with a single list atom of length $min(l_1 + l_2, 2)$ where $l_1$ and $l_2$ are the lengths of the original lists. For example: $x |-> f0 * ls(bound: 1, f0, nil)$ will be abstracted to $ls(bound: 2, x, nil)$.

=== Doubly Linked Lists

The abstraction of DLS lists is done similarly, except the conditions needed for abstraction are slightly different. For abstracting $dls(bound: 2, x, f0, nil, f1) * dls(bound: 2, f1, y, f0, nil)$ into $dls(bound: 3, x, y, nil, nil)$, the following conditions must hold:

- Either the first and the last allocated variable of both atoms must be the same (that signifies the lists have length at most one), or the last allocated variable of the first list (#f0) and the first allocated variable of the second list (#f1) must be logic variables unreachable from the rest of the formula, like in the SLS case.

- The "p" field of the second list must point back to the last allocated variable of the first list.

- The "p" and "n" fields of the first allocated variable of the first list and the last allocated variable of the last list respectively cannot point back into the list

- Like in the SLS case, the list must not be cyclic -- this is checked using the solver both forward and backward.

The length bound of the joined list will be $min(l_1 + l_2, 3)$, because we have a way to translate DLS lists with minimum length of three.

=== Nested Lists


NLS lists are abstracted similarly to SLS lists, except that an additional check must be done that the SLS sublists end up in the same variable. SLS abstraction is run first, so we can expect the sublists to be abstracted into a single list atom. The algorithm for joining two NLS atoms differs based on the type of atom:

When joining two points-to atoms, $x |-> fields("t": f0, "n": f1) * f0 |-> fields("t": y, "n": f2)$, the following options are tried in order:
- if #f1 is equal to #f2, the atoms are joined as-is into $nls(bound: 2, x, y, f1)$
- if the formula contains $ls(f1, z)$ and $z$ is equal to #f2, the sublist from #f1 is removed and the atoms are joined into $nls(bound: 2, x, y, z)$
- if the formula contains $ls(f1, z_1) * ls(f2, z_2)$ and $z_1$ is equal to $z_2$, the sublists from #f1 and #f2 are removed and the atoms are joined into $nls(bound: 2, x, y, z_1)$

When joining a list atom $nls(x, f0, z) * f0 |-> fields("t": y, "n": f1)$, these options are tried in order:
- if $z$ is equal to #f1, the atoms are joined as-is into $nls(x, y, z)$
- if the formula contains $ls(f1, f2)$ and $f2$ is equal to $z$, the sublist from #f1 is removed and the atoms are joined into $nls(x, y, z)$

The only way to join two list atoms $nls(x, f0, z_1) * nls(f0, y, z_2)$ into $nls(x, y, z)$ is when $z_1$ and $z_2$ are already equal.

All equivalences are checked syntactically. All variables at the start of the deleted sublists must be logic variables unreachable from the rest of the formula. The reason for checking this many options instead of just looking for sublists everywhere is that an $nls(x, y, z)$ atom already contains sublists leading to $z$. For example, it is not possible to join $nls(x, f0, f1) * ls(f1, z) * f0 |-> fields("t": y, "n": z)$ into $nls(x, y, z)$ because that would violate the semantics of the NLS atom, in that all sublists leading into $z$ must be disjoint.

== Deduplication Sort <deduplication_sort>

// mention different sorting variants of formulae in [deduplicate_state]

// TODO: mozna vytahnout do vlastni kapitoly?

Because the deduplication algorithm processes formulae in order, it is beneficial to first sort these formulae so that the chances of finding duplicate formulae are as high as possible. Specifically, it is always better to have a higher length bound on the list atoms of the formula on the left side of the entailment. For example, the entailment $ls(bound: 0, x, y) entl ls(bound: 2, x, y)$ will not succeed, but $ls(bound: 2, x, y) entl ls(bound: 0, x, y)$ will.

The sorting is done by calculating an integer score for each formula and then sorting the formulae based on this score.

The sorting is done by defining a comparison function that accepts two formulae and puts them to the correct order. This function first calculates the lowest number of allocations for each of the formulae and them puts the higher of the two on the left side of the entailment. This makes sense because the formula with the higher number of allocations can have a subset of the formula with the lower number of allocations, but not the other way around. For example, this entailment will hold:

$ ls(bound: 2, x, y) entl ls(bound: 1, x, y) $

But this one cannot:

$ ls(bound: 1, x, y) entl ls(bound: 2, x, y) $

The score for each formula is calculated by adding up all length bounds on list atoms, points-to atoms are counted as 1. Additionally, if a formula contains any number of list atoms, its score is decreased by one. This is done to prevent a points-to atom from being on the right side of an entailment:

$
  x |-> y &entl ls(bound: 1, x, y)\
  1 &> 0
$

This method even works for some combinations of points-to and list predicates:

$
  x |-> f0 * ls(bound: 0, f0, y) &entl ls(bound: 0, x, y)\
  0 &> -1
$

And for multiple list atoms in one formula:

$
  ls(bound: 1, x, f0) * ls(bound: 2, f0, y) &entl ls(bound: 2, x, y)\
  2 &> 1
$

If this comparison does not decide the order of the entailments, a second score is calculated. The algorithm is the same, except that points-to atoms are not counted at all. In this case, the formula with the lower score is placed on the left side of the entailment This will order formulae such as these:

$
  x |-> f0 * ls(bound: 1, f0, y) &entl ls(bound: 2, x, y)\
  1 &gt.not 1 #h(2em) "(first score)"\
  0 &< 1 #h(2em) "(second score)"
$

The second scoring does not affect whether the entailment will succeed or not, but it will keep the simpler, more abstracted formula in the deduplicated list instead of the materialized form.

== Underapproximation Mode <underapproximation_mode>

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

= Results <results>

The analysis tool was evaluated on a small set of custom programs and on a subset of the SV-COMP benchmarks @svcomp.

We use the Benchexec framework @benchexec for both sets of benchmarks. Benchexec is a benchmarking framework that automatically evaluates a program on a set of benchmarks, while constraining system resources. Benchexec requires a tool definition in the form of a Python module that tells it how to execute the analyzer on a given test program. This module is provided in #plugin_link("bench/slplugin/slplugin.py"). The benchmarks are then executed using a benchmark run definition, an XML file that defines the set of programs on which to test the analyzer and the resource limits to apply. Benchexec then runs the benchmarks while tracking resource usage. Results of all the benchmarks can then be rendered to an HTML table for viewing or to CSV for further processing.

#figure(
  caption: "Benchmark results rendered by Benchexec",
  image("benchexec.png"),
)

All benchmarks were run on an AMD EPYC 9124 processor with the following resource limits for each benchmark:
- 1500 MB of memory
- 3 minutes of total run time

The amount of memory was chosen as small as possible such that no tests would run out of memory and so that the testing could be paralelized into 8 processes on a VM with 8 CPU cores and 16 GB of memory available. This limit has a wide margin since the largest amount of allocated memory during a benchmark was under 600 MB. In the case of maximum test time, the three minutes were chosen as the smallest value such that no additional tests would timeout compared to a run with 15 minutes of maximum test time.

The evaluated version of the analyzer is marked with the Git tag `final_code`.

In the SV-COMP dataset, one of the analyzed properties programs is called `MemSafety`, which requires the following subproperties to hold:
- `valid-free` -- all memory deallocations are valid (no pointers other than those allocated with `malloc` and others are passed to the `free` function).
- `valid-deref` -- all pointer dereferences are valid.
- `valid-memtrack` -- all memory allocations are tracked (no memory is leaked by losing a pointer to it or left allocated after the end of the `main` function).

The benchmarked programs all have an expected result. The expected result can either be a violation of one of the properies, or `true` signifying that all properties are valid for the program.

== Custom Benchmarks

The custom benchmarks verify that all basic parts of the analyzer work as intended. On the evaluated version of the analyzer, all of the custom benchmarks are passing. Since these programs are purposefully written to test the analyzer, they do not contain any code that requires the external preprocessing described in @const_prop, and it is therefore not enabled for these tests. The test programs are located in #plugin_link("test_programs") and can be divided to these groups.

The first group contains programs that test a specific part of the analysis, these serve as a quick check of specific functionality during development. The only property being tested here is that the analysis completes successfully.
- `all_list_types.c` -- list and field type recognition, type heuristic
- `generic_structs.c` -- handling of structs not recognized as lists
- `stack_pointers.c` -- handling of pointers to variables on the stack

The second group tests checks the analysis of a correct program that allocates a list of nondeterministic size, traverses the list from start to end, and deallocates the list. These benchmarks test allow us to compare the run time of an equivalent program for different list types (programs named `*_full.c`). These tests also verify that all basic parts of the analysis (abstraction, simplifications) work as intended.
- `ls_full.c`
- `ls_full_return.c`
- `ls_full_single_function.c`
- `ls_cyclic.c`
- `dls_full.c`
- `nls_full.c`

#table(
  columns: 2,
  table.header(
    "Program",
    "CPU Time",
  ),

  `ls_full.c`, "1.05 s",
  `dls_full.c`, "13.1 s",
  `nls_full.c`, "17.0 s",
)

As you can see in the table above, the analysis of programs using doubly linked lists and nested lists is roughly an order of magnitude longer then the analysis of singly linked lists.

The third group tests the detection of different types of bugs. The only property being tested is that the errors are reported as the correct type of error, since for example the `valid-free` property is not violated in any of the SV-COMP benchmarks of the `LinkedLists` subset.
- `ls_null_deref.c` -- detection of a possible null-pointer dereference (`valid-deref` property)
- `ls_use_after_free.c` -- detection of an access to a freed allocation (`valid-deref` property)
- `dls_double_free.c` -- detection of a double-free (`valid-free` property)
- `nls_memory_leak.c` -- detection of a memory leak (`valid-memtrack` property)

The last group tests some other interesting operations on singly linked lists. The benchmarked programs are correct.
- `ls_merge_lists.c` -- program that constructs two lists and then joins one on the end of the other.
- `reverse_list.c` -- program that reverses the order of a list.
- `postfix_calculator.c` -- program that evaluates simple mathematical expressions in Reverse Polish (postfix) notation. The stack used to hold operands is implemented using a linked list.

#table(
  columns: 2,
  table.header(
    "Program",
    "CPU Time",
  ),

  `ls_merge_lists.c`, "7.89 s",
  `reverse_list.c`, "2.36 s",
  `postfix_calculator.c`, "1.01 s",
)

As can be seen on the analysis times, the complexity of the program itself does not really affect the run time of the analysis. Even though the calculator is a larger program (147 lines of code) than most of the other tested programs, the analysis takes only a second. The complexity of the operations on the linked lists is what dictates the analysis time. In the case of the calculator, the only operations on the list are simple `push` and `pop` operations that do not even require the traversal of the list.

== SV-COMP Benchmarks

SV-COMP is a yearly competition for verification tools composed of many categories, each verifying a different property of programs. One of these categories is `MemSafety`, in which analyzers verify the properties described previously. However, the `MemSafety` category includes many kinds of programs, such as those working with arrays, on which our analyzer cannot succeed. Therefore, we restricted the set of benchmarks only to the `LinkedLists` subset of this category.

When trying different settings of the loop unrolling (see @const_prop), it became clear that some tests will only finish when run with a higher count of unrolled iterations and other tests will only finish with a lower count. After some trial and error, we chose to run the analysis for one third of the maximum time with the the unrolling level set to 3. If the analysis timeouts, the second third is run with unrolling level of 2, and for the last third, no unrolling is done at all.

Of the 134 total benchmarks, this tool correctly analyzes 80 programs. The full results can be seen in the table below, compared to PredatorHP @predatorHP (the best-performing analyzer of SV-COMP 2025 in the `LinkedLists` subcategory) and EVA @eva (static analyzer built into Frama-C).

#table(
  columns: 4,
  table.header(
    [Tests (134 total)],
    [This work],
    [PredatorHP],
    [EVA],
  ),

  [Correct], [80], [124], [56],
  [Correct (true)], [74], [96], [50],
  [Correct (false)], [6], [28], [6],
  [Incorrect (true)], [0], [0], [6],
  [Incorrect (false)], [0], [0], [48],
  [Timeout], [11], [10], [4],
  [Unknown], [46], [0], [15],
)

In the table below, you can see that our analyzer outperforms all but two other tools in the `LinkedLists` subcategory:

#table(
  columns: 2,
  table.header("Analyzer", "Correct Results"),
  [PredatorHP], [124],
  [CPAchecker], [118],
  [nacpa (CPAchecker)], [118],
  [*This work*], [*80*],
  [symbiotic], [79],
  [ESBMC], [78],
  [CBMC], [77],
  [Bubaak], [66],
  [Graves-CPA], [64],
  [PeSCo], [64],
  [2LS], [60],
  [Mopsa], [48],
  [DIVINE], [42],
  [sv-sanitizers], [17],
  [ULTIMATE Automizer], [12],
  [ULTIMATE Taipan], [8],
  [ULTIMATE Kojak], [6],
  [Goblint], [1],
  [SVF-SVC], [0],
  [Theta], [0],
)

The failing benchmarks can be divided into a the following categories.

#table(
  columns: 2,
  table.header("Failure reason", "Number of\nbenchmarks"),
  [Unsupported language features], [25],
  [Timeout], [11],
  [Unknown result], [18],
)

First, there are benchmarks that use code from the Linux kernel. This includes all benchmarks from the `ddv-machzwd` group and some benchmarks from `memsafety-broom`. These fail immediately since the code contains many types of pointer arithmetic which is unsupported. Some other tests outside of the kernel code also use unsupported language features and therefore fail at the preprocessing level.

In the group of benchmarks that timeout, we can find a few problems. Most of these programs use structs that will not be detected as the correct list type. This is the case in #svcomp_link("forester-heap/dll-01-2.c"), #svcomp_link("list-ext3-properties/sll_of_sll_nondet_append-1.c") and others. Some programs implement lists that are simply not covered by the supported list types, for example #svcomp_link("heap-manipulation/tree-1.c"). In all these programs, the abstraction will never be able to create list predicates because the structures in the heap will be different than expected. A few programs that timeout do not have a fundamental reason why the analysis cannot succeed, such as #svcomp_link("memsafety-broom/sll-nested-sll-inline.c"). These will require further optimizations of the analyzer to pass.

The rest of failed benchmarks are those, in which a property violation is detected, but because a nondeterministic condition has been reached during the analysis, an unknown result is reported. This problem is described in detail in @const_prop. However, constant propagation and loop unrolling cannot solve all of these cases, especially not those where relevant non-pointer data is stored in the list nodes themselves. This includes most red-black list benchmarks (such as #svcomp_link("forester-heap/dll-rb-cnstr_1-1.c")), or benchmarks where integer indices are used to manipulate list nodes (#svcomp_link("list-ext3-properties/sll_nondet_insert-2.c")).

The analysis of most correct results is generally fast, 49 od the 80 correct results are analyzed under a second, 67 under 10 seconds. Because the total analysis time is split to thirds, in which the tool runs with different settings, you can see a group of results appear after one minute.

#figure(
  caption: [Number of correct results (x-axis) reached by this tool\
    with increasing analysis time in seconds (y-axis)],
  image("svcomp.png"),
)

Compared to PredatorHP which reaches 119 of its 124 correct results in under a second, the analysis time is overall worse. However, CPAchecker does not produce any results in less than 7 seconds, so compared to this tool, our analyzer is overall faster.

#figure(
  caption: [Number of correct results (x-axis) reached by PredatorHP\
    with increasing analysis time in seconds (y-axis)],
  image("predator_svcomp.png"),
)

== Astral Queries

// measure how much time is spent evaluating formulae in astral

Our analyzer is able to track how much time of each run is spent in the Astral solver. The table below shows three fastest and slowest SV-COMP benchmarks. Because the analyzer is killed during a timeout, it does not have a chance to print out the Astral time, therefore this table is only showing benchmarks that were completed during the first minute.

#table(
  columns: 4,
  table.header("Benchmark", "Total\ntime (s)", "Astral\ntime (s)", "Analyzer\ntime (s)"),
  [`sll2c_append_unequal.c`], [0.50], [0.00], [0.50],
  [`sll_shallow_copy-1.c`], [0.51], [0.00], [0.51],
  [`dll2c_prepend_equal.c`], [0.51], [0.02], [0.49],
  table.cell(colspan: 4, $ dots.c $),
  // [`dll_nullified-2.c`], [5.21], [4.68], [0.53],
  // [`dll_circular_traversal-1.c`], [9.91], [9.41], [0.50],
  // [`dll_circular_traversal-2.c`], [11.02], [10.52], [0.50],
  // [`sll-shared-sll-after.c`], [11.36], [10.84], [0.52],
  // [`sll-shared-sll-before.c`], [13.08], [12.46], [0.62],
  [`dll-rb-cnstr_1-2.c`], [16.29], [15.64], [0.65],
  [`sll-sorted-2.c`], [31.79], [31.65], [0.14],
  [`dll-simple-white-blue-2.c`], [48.61], [48.13], [0.48],
)

As you can see from the table, the time spent in the analyzer itself is basically independent of the total run time. This includes initialization of the framework, preprocessing, and all work done during the dataflow analysis. Both the average and the median time spent in the analyzer is approximately 0.53 seconds. This clearly shows that to achieve faster analysis, it is pointless to optimize the code of the analyzer itself. The only thing that can meaningfully improve run times is a better usage of the solver through simplifications of formulae.

There is usually a small number of queries that take up most of the time in spent in the solver. For example, during the analysis of `sll-shared-sll-after.c`, the top 8 slowest queries take 18.2 seconds of the total 19.9 seconds spent in the solver.

#figure(
  caption: [The times of evaluating queries when analyzing `sll-shared-sll-after.c`],
  image("queries.svg"),
)

These queries are usually entailments, especially those with logic variables on the right side, as these must be existentially quantified. The longest query in this example (3.60 seconds) is the following entailment:

$
  nil = a * b != nil * A_b != nil * freed(b) * freed(A_b) \
  entl\
  a != nil * A_a != nil * freed(a) * freed(A_a) * f0 -> nil * ls(bound: 0, b, f0)
$

The other slow queries are similar to this one in fact that they are entailments with list predicates and logic variables on the right side.

= Conclusion


== Future Work

// better detection of list types -> acc to structures created during the analysis

// use freed(x!0) to represent newly allocated memory?

#pagebreak()

#bibliography("references.bib")

// TODO: nil a emp renderovat jaklo monospace
// plugin_link musi obsahovat na konkretni tag/commit -> mozna gitea?
// popsat tu vyjimku s verifier_nondet_int pro podminky
// zkontrolovat/zmenit git tag co se zminuje v results
