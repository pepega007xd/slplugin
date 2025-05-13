#import "template.typ": *
#import "@preview/diagraph:0.3.3" as graph

#show: project

#page(
  margin: 0pt,
  image("titlepage.svg"),
)

#page(
  margin: 0pt,
  image("assignment.svg"),
)

#set heading(numbering: none)

#stack(
  spacing: 4em,
  [
    = Abstract

    This thesis introduces the KTSN static analyzer that aims to verify the memory safety of C programs. The method focuses on programs that manipulate linked lists. Our tool is able to prove the correctness of handling dynamically allocated memory and detect invalid pointer dereferences such as use-after-free, memory leaks, and other bugs. The approach is based on dataflow analysis, and it uses separation logic to represent abstract memory states. The analysis uses Astral, a dedicated solver for SL based on translation to SMT. KTSN is implemented as a plugin to the Frama-C framework. The tool was tested on the linked lists subset of the SV-COMP benchmarks and compared to other verification tools. While it does not reach the top competitor, PredatorHP, it outperforms most verifiers in this category, including EVA, a value analyzer of Frama-C. Thus, our method shows promise of integration into EVA, greatly improving the analysis of linked lists in Frama-C.
  ],
  [
    = Abstrakt
    #show: czech_text

    Tato práce představuje statický analyzátor KTSN zaměřený na verifikaci bezpečnosti práce s pamětí programů v jazyce C. Naše metoda se soustředí na programy, které pracují s lineárními seznamy. Náš analyzátor je schopen verifikovat správnost manipulace s dynamicky alokovanou pamětí a detekovat neplatné dereference ukazatelů (např. use-after-free), úniky paměti a další chyby. Náš přístup je založen na analýze toku dat a využívá separační logiku pro reprezentaci abstraktních stavů paměti. Nástroj je implementovaný ve frameworku Frama-C. Analýza využívá dedikovaný solver Astral pro rozhodování separační logiky založený na překladu do SMT. Nástroj KTSN byl otestován na podmnožině SV-COMP benchmarků zaměřené na lineární seznamy a porovnán s dalšími verifikačními nástroji ve své kategorii. Přestože nedosahuje úrovně nejlepšího nástroje PredatorHP, překonává většinu verifikačních nástrojů v této kategorii, včetně analyzátoru EVA, který je součástí frameworku Frama-C. Naše metoda rovněž vykazuje potenciál pro integraci do analyzátoru EVA, čímž by se výrazně vylepšila analýza lineárních seznamů ve Frama-C.
  ],
  [
    = Keywords

    verification, static analysis, separation logic, Astral, Frama-C, SV-COMP, KTSN
  ],
  [
    = Klíčová slova

    verifikace, statická analýza, separační logika, Astral, Frama-C, SV-COMP, KTSN
  ],
  [
    = Reference

    BRABLEC, Tomáš. _Static Analysis of Heap-Manipulating Programs using Separation Logic._ Bachelor's Thesis. Tomáš DACÍK (supervisor). Brno: Brno University of Technology, Faculty of Information Technology, 2025.
  ],
)

#pagebreak()

= Rozšířený abstrakt
#czech_text[
  Existuje několik způsobů správy paměti v programovacích jazycích. Tyto způsoby se dělí na automatickou správu paměti, která využívá garbage collector k uvolňování již nedosažitelných alokací, a manuální správu paměti, kde musí kód programu explicitně uvolňovat alokace. Manuální správa paměti s sebou nese riziko několika typů chyb, kterých se může programátor při psaní kódu dopustit. Jednou z těchto chyb je tzv. use-after-free, neboli přístup do již uvolněné alokace. Dále jde např. o double-free -- uvolnění již uvolněné alokace, nebo únik paměti, kdy dojde ke ztrátě všech ukazatelů na alokaci bez jejího uvolnění. Některé z těchto chyb mohou vytvořit bezpečnostní zranitelnosti v softwaru, zneužitelné např. ke spuštění kódu útočníkem.

  K nalezení těchto chyb se používá řada technik. Jedním přístupem je testování a dynamická analýza programu, např. pomocí instrumentace kódu překladačem. Dalším je statická analýza programu a formální verifikace. Formální verifikace umožňuje ověřit korektnost programu, přičemž se typicky zaměřuje na ověření nějaké konkrétní vlastnosti. Z hlediska paměťové bezpečnosti nás zajímá právě korektnost všech přístupů do paměti.

  Jednou z metod statické analýzy je analýza toku dat (dataflow), která spočívá v procházení grafu toku řízení (CFG) programu a postupné aktualizaci informací o programu asociovaných s každým uzlem v CFG. V kontextu analýzy práce s pamětí jsou pro nás relevantní hodnoty ukazatelů a datové struktury v dynamické paměti. K jejich reprezentaci se v těchto analýzách často využívají formule separační logiky, která nám umožňuje efektivně reprezentovat datové struktury o neomezené velikosti.

  Tato práce popisuje tvorbu statického analyzátoru KTSN pro programy v jazyce C, který bude schopen verifikovat korektnost práce s dynamicky alokovanou pamětí se zaměřením na lineární seznamy. K tomu je využita metoda analýzy toku dat, a stavy programu jsou reprezentovány pomocí formulí separační logiky. K implementaci analýzy je využit framework Frama-C, který poskytuje zjednodušenou formu AST analyzovaného programu, rozhraní pro transformace tohoto AST a framework pro implementaci dataflow analýzy. K vyhodnocování splnitelnosti formulí separační logiky je použit solver Astral. Ověření splnitelnosti formulí je třeba k nalezení abstraktního stavu paměti popisujícího cykly (tzv. fixpoint), což je nutné k ukončení analýzy.

  Samotná implementace se skládá z několika částí. Prvně je to předzpracování analyzovaného AST, které slouží několika účelům. Jedním účelem je zjednodušit komplexní výrazy v příkazech přiřazení, argumentech funkcí a podmínkách. Dalším účelem je analyzovat struktury definované v programu a určit, který typ seznamu popisují. Toto se provádí na základě heuristiky, která odvodí typ seznamu podle typu položek struktury.

  Další částí implementace je analýza jednotlivých příkazů. Při dosažení příkazu je třeba upravit formule reprezentující stav programu před vykonáním příkazu tak, aby popisovaly stav po vykonání příkazu. Tyto příkazy zahrnují několik druhů dereferencí a přístupů na položky struktury, alokace a uvolňování paměti, a volání funkcí.

  Implementace dále obsahuje zjednodušování a abstrakce formulí na přechodu mezi dvěma příkazy. Zjednodušování formulí slouží primárně ke zkrácení času, který potřebuje solver k ověřování jejich splnitelnosti. Do této kategorie spadá například odstraňování nedosažitelných prostorových predikátů z formulí (tyto jsou pak hlášeny jako úniky paměti), odstraňování nepotřebných proměnných z ekvivalencí, nebo přejmenování proměnných, kterým v programu skončil rozsah platnosti. Abstrakce spočívá v nalezení ukazatelových predikátů ve formuli tvořících řetězec a jejich spojení do seznamových predikátů, které popisují lineární seznam o neomezené délce. Tento krok je nutný k nalezení fixpointu pro cykly.

  Analyzátor byl otestován na ručně vytvořené sadě testovacích programů, které pracují se všemi podporovanými typy seznamů. Do testovaných operací patří alokace a uvolnění seznamu o nedeterministické délce, iterace skrz prvky seznamu, přidání a odstranění prvků ze seznamu nebo obrácení pořadí seznamu. Analyzátor byl schopen ověřit korektnost všech těchto jednoduchých programů. Při přidání chyb do těchto programů byl nástroj schopný tyto chyby detekovat. Mezi detekované chyby se řadí dereference ukazatelů na dealokovanou paměť, dereference ukazatele, který může mít nulovou hodnotu, únik paměti, nebo dealokace již uvolněné paměti.

  Další testování proběhlo na datasetu ze soutěže SV-COMP, konkrétně na podmnožině programů pracujících s lineárními seznamy. Tento dataset obsahuje celkem 134 programů testujících velké množství operací na mnoha typech seznamů. Do těchto programů patří např. řazení seznamů, průchody a modifikace cyklických seznamů nebo kombinace několika typů seznamů v jednom programu. Na tomto datasetu je KTSN schopný správně analyzovat 80 programů, z toho 74 jsou ověření korektních programů a zbylých 6 jsou úspěšné detekce chyb. Tento výsledek překonává všechny analyzátory v dané kategorii kromě dvou (PredatorHP se 124 správnými výsledky a CPAchecker se 118 správnými výsledky). Analyzátor EVA, postavený nad frameworkem Frama-C, dokáže správně analyzovat 56 programů.

  Současná implementace analýzy má několik omezení, která znemožňují dosáhnout lepších výsledků. Prvně je to chybějící podpora analýzy ukazatelové aritmetiky. V současném stavu není možné obecně reprezentovat číselný offset do struktury ve formulích separační logiky. Toto znemožňuje analyzovat 25 testů z SV-COMP datasetu, které používají ukazatelovou aritmetiku k přístupu do uzlů seznamu. Tyto testy selžou už při předzpracování kódu.

  Dalším omezením je absence analýzy číselných hodnot v programu, což znemožňuje přesnější analýzu podmínek v cyklech. Ta je nutná například k přesnější detekci chyb v programech. Neschopnost analyzovat jiné než ukazatelové podmínky způsobí, že 18 SV-COMP testů skončí neznámým výsledkem, jelikož se nepodaří prokázat přítomnost chyby.

  Zbylých 11 selhávajících testů skončí překročením časového limitu pro analýzu, například kvůli tomu, že implementují nepodporovaný typ seznamu, nebo protože heuristika pro odvození typu seznamu vrátila špatný výsledek.

  Nabízí se několik možných směrů budoucího rozšíření analyzátoru. Jednou možností je přidat jednoduchou analýzu číselných hodnot přímo do nástroje KTSN. Separační logika v podobě, v jaké ji implementuje solver Astral, umožňuje vložit do formulí termy v SMT, které mohou reprezentovat hodnoty číselných proměnných. Zajímavějším směrem vývoje je integrace naší metody přímo do analyzátoru EVA. To by nám umožnilo přímo v průběhu analýzy získávat informace o číselných proměnných a poskytovat přesnější informace o ukazatelových proměnných zpět do nástroje EVA. Kombinace obou nástrojů by pak byla schopná analyzovat programy, které ani jeden z nich aktuálně analyzovat nedokáže.
]

#pagebreak()

= #text("Static Analysis of Heap-Manipulating Programs using Separation Logic", hyphenate: false)

#v(2em)

= Declaration

I hereby declare that this Bachelor’s thesis was prepared as an original work by the author under the supervision of Ing. Tomáš Dacík. Supplementary information was provided by prof. Ing. Tomáš Vojnar, Ph.D. I have listed all literary sources, publications, and other resources used during the preparation of this thesis.

#v(2em)

#align(right)[
  #line(length: 10em, stroke: (dash: "loosely-dotted"))
  Tomáš Brablec\
  May 12, 2025
]

#v(2em)

= Acknowledgements

I would like to thank my supervisor, Ing. Tomáš Dacík, and my advisor, prof. Ing. Tomáš Vojnar, Ph.D., for introducing me to the topic of software verification and for their guidance during the preparation of this thesis.

#pagebreak()

// Start of the main text

// enable page numbering from one
#set page(numbering: "1", number-align: center)
#counter(page).update(1)

// enable heading numbers
#set heading(numbering: "1.1")

// generate table of contents
#outline(depth: 2, target: heading.where(numbering: "1.1"))

= Introduction

There are two basic approaches to memory management in programming languages. One approach relies on a garbage collector to reclaim unreachable allocated memory automatically. The other, manual memory management, relies on the programmer to allocate and free memory explicitly. Some languages that rely on manual memory management allow for a group of bugs, called memory safety bugs, to occur. These include _use-after-free_ -- access to already freed memory, null-pointer dereferences, _double-free_ -- deallocating already freed memory, or _memory leaks_ -- a loss of reference to an allocation. These bugs are often a source of security vulnerabilities in affected programs, since invalid memory accesses are undefined behavior in C and C++ @chrome_bugs @microsoft_bugs.

There are a number of techniques to find these bugs and ensure the memory safety of programs. One approach is testing and dynamic analysis, often by code instrumentation done by the compiler. Another approach is static analysis and formal verification. Formal verification is a collection of methods to prove the correctness of a program or its specific property. In this case, the property is memory safety, i.e., that every memory access in the program is valid and memory (de)allocation is done correctly. Methods of static analysis are described in @static_analysis.

One of the approaches to static analysis is so-called _dataflow analysis_, which relies on traversing the control flow graph (CFG) of a program and tracking certain information at each node of the CFG. The _Frama-C framework_ described in @frama_c provides a generic framework for implementing a dataflow analysis of C programs.

Formulae of _separation logic_ are often used to represent the shapes of data structures for the purpose of static analysis. Separation logic allows us to describe structures such as linked lists of unbounded size in a single formula. Models of these formulae then represent concrete configurations of the heap. Separation logic is explained in detail in @sl_section.

@existing_approaches describes existing approaches to static analysis of programs with a focus on proving the memory safety of programs. Analyzers that use separation logic are also mentioned.

@analysis_section introduces our new static analyzer, KTSN, aimed at verifying the memory safety of C programs. The tool is focused on programs operating on linked lists, as this is a common data structure in low-level software. The analyzer is based on dataflow analysis and uses formulae of separation logic to represent abstract memory states. To determine which states generated during the analysis are fully covered by other states, a solver for separation logic _Astral_ is used. This, along with the abstraction of pointer chains into list predicates, is needed to find invariants for loops in the program, which is required for the analysis to terminate.

@implementation_details describes the implementation details of the analyzer. KTSN is implemented using the Frama-C framework, which also provides a simplified representation of the input program's AST with an API for its transformation. This is used to preprocess the original AST before the analysis.

In @results, the analyzer is tested on crafted programs that work with all supported types of linked lists, trying common list operations such as construction, traversal, insertion into the list, reversal, deallocation, and others. The tool is also tested on a subset of the SV-COMP benchmarks dedicated to linked lists and compared to state-of-the-art tools in this category. The time needed to evaluate queries to the solver during the analysis is also benchmarked.

@conclusion summarizes this work and discusses possible future improvements of the analysis, including the integration of our method into EVA, a value analyzer of the Frama-C framework.

= Preliminaries

This section describes concepts and technologies needed to understand the design and implementation of our analysis.

== Static Analysis of Programs <static_analysis>

Static analysis of programs is a collection of methods to determine some properties of programs without executing them. Static analysis aims to detect bugs in software, ensure code quality, enable optimizations during compilation, or prove the correctness of programs. The main advantage of static analysis compared to dynamic analysis, which involves running the program in different configurations, is that it can reason about all possible program executions at once and can therefore prove properties of programs. Some of the methods used for static analysis fall into the category of formal verification -- methods designed to prove the correctness of programs.

There are numerous approaches to formal verification. One of these is _abstract interpretation_, a method that simulates program execution while over-approximating program values on some abstract domain (for example, using intervals for integer values). Formulae of separation logic are one of these abstract domains, especially useful for verifying the memory safety of programs.

Another approach is _model checking_, which verifies that a finite-state model of a system satisfies some specification. A typical use case might be the verification of concurrent systems. Temporal logics are often used for this purpose.

_Symbolic execution_ is another common approach that simulates the execution of a program, but uses symbolic values instead of concrete values, generating constraints for different paths through the program. Solving these constraints can then be used to, for example, discover inputs that trigger bugs in programs.

== Separation Logic <sl_section>

Separation logic (SL) @SL @SL2 is one of the most popular formalisms used for analysis of programs that manipulate dynamically allocated memory. A formula of separation logic allows us to describe the program's heap so that models of such a formula, called _stack-heap models_, represent concrete structures of allocations and pointers connecting them. The main advantage of separation logic over other formalisms used for describing dynamic memory is the ability to describe disjoint parts of the heap using the _separating conjunction_ ($*$) operator. Another key component is a set of _inductive predicates_, which describe data structures of an unbounded size in a closed form.

Due to the high expressivity of general separation logic, existing solvers usually focus on a certain subset of all SL formulae, called _fragments_. One of these fragments commonly used for static analysis is called the _symbolic heap fragment_, which has a limitation in the use of boolean connectives.

The rest of this section describes a fragment of separation logic called _boolean separation logic_ as defined in @astral, which, unlike other SL fragments, allows for a limited use of boolean operators -- conjunction, disjunction, and guarded negation ($phi and_not psi$). Boolean connectives are needed for the representation of lists with a non-zero minimum length and for representing a state using a disjunction of multiple formulae.

=== Syntax

Variables in SL have a _sort_ representing the type of list the variable can be a part of. There are three predefined sorts, $SS$ for singly-linked lists (SLS), $DD$ for doubly-linked lists (DLS), and $NN$ for nested singly-linked lists (NLS). We denote $x^SS$ as a variable of sort $SS$. Additionally, generic sorts can be defined along with custom fields $upright(f_1), ..., upright(f_n)$ for its points-to predicate. In the grammar, these sorts are denoted as $GG$. There is also a special variable #nil without a sort.

The syntax of SL formulae $phi$ is described by this grammar:

$
  pi_p := &x^SS |-> n\
  | &x^DD |-> fields("n": n, "p": p)\
  | &x^NN |-> fields("t": t, "n": n)\
  | &x^(GG) |-> fields(upright(f_1) : f_1, ..., upright(f_n) : f_n) && "(points-to predicates)"\
  pi_l :=& ls(x^SS, y^SS) | dls(x^DD, y^DD, p^DD, n^DD) | nls(x^NN, y^NN, z^SS) #h(2em) && "(list predicates)"\
  phi_a :=& x = y | x != y | freed(x) | emp | pi_p | pi_l && "(atomic formulae)"\
  phi :=& phi_a | exists x. thick phi | phi * phi | phi and phi | phi or phi | phi gneg phi && "(formulae)"
$

We write $phi[b\/a]$ for the formula $phi$ in which all occurrences of $a$ are replaced by $b$.

=== Semantics

#let Vars = `Vars`
#let Locs = `Locs`
#let Fields = `Fields`
#let freedloc = sym.times.square
#let dom = `dom`
#let domh = $dom(h)$

Let #Vars be a set of variables, #Locs a set of locations, and #Fields a set of fields. The model of an SL formula, called a stack-heap model, is denoted as a pair $(s,h)$. The _stack_ is a partial function $#Vars -> #Locs$, and the _heap_ is a partial function $#Locs -> (Fields -> Locs)$. The partial function $(Fields -> Locs)$ can be written as an enumeration of all fields and their corresponding locations, for example $h(ell) = fields(upright(f_1) : ell_1, upright(f_2): ell_2)$. Let #domh be the domain of the partial function $h$.

The semantics of the special variable #nil is that $s(nil) in.not domh$. There is a special location $freedloc in #Locs$, used in the definition of the #freed predicate. Its semantics are the same as for #nil, $freedloc in.not domh$.

The semantics of boolean separation logic is defined in @sl_semantics.

#figure(
  caption: "The semantics of separation logic",
  placement: auto,
  $
    &(s,h) entl x = y && "iff" s(x) = s(y) "and" domh = emptyset\
    &(s,h) entl x != y && "iff" s(x) != s(y) "and" domh = emptyset\
    &(s,h) entl freed(x) && "iff" h = {s(x) |-> fields("n": freedloc) }\
    &(s,h) entl emp && "iff" domh = emptyset\
    &(s,h) entl x |-> fields(upright(f_1) : f_1, ..., upright(f_n) : f_n) #h(2em) && "iff" h = {s(x) |-> fields(upright(f_1) : f_1, ..., upright(f_n) : f_n)}\
    & && #h(1em) "and" s(f_i) != freedloc "for each" i\
    &(s,h) entl phi_1 and phi_2 && "iff" (s,h) entl phi_1 "and" (s,h) entl phi_2\
    &(s,h) entl phi_1 or phi_2 && "iff" (s,h) entl phi_1 "or" (s,h) entl phi_2\
    &(s,h) entl phi_1 gneg phi_2 && "iff" (s,h) entl phi_1 "and" (s,h) tack.double.not phi_2\
    &(s,h) entl exists x . thick phi && "iff there is a location" ell "such that" (s[x |-> ell], h) entl phi\
    &(s,h) entl ls(x, y) && "iff" (s,h) entl x = y, "or"\
    & && #h(1em) s(x) != s(y) "and" (s,h) entl exists x'. thick x |-> x' * ls(x', y)\
    &(s,h) entl dls(x, y, p, n) && "iff" (s,h) entl x = n * y = p, "or"\
    & && #h(1em) s(x) != s(n), s(y) != s(p), s(p) in.not domh, "and"\
    & && #h(1em) (s,h) entl exists x'. thick x |-> fields("n": x', "p": p) * dls(x', y, x, n)\
    &(s,h) entl nls(x, y, z) && "iff" (s,h) entl x = y, "or"\
    & && #h(1em) s(x) != s(y) "and"\
    & && #h(1em) (s,h) entl exists t', n'. thick x |-> fields("t": t', "n": n') * ls(n', z) * nls(t', y, z)\
  $,
) <sl_semantics>

Equality and inequality of variables are defined as expected, but note that they require the heap to be empty. Therefore, separating conjunction is used to join all atomic formulae instead of regular conjunction. For example, a heap with one allocation will be described as $x |-> y * y = nil$. The formula $x |-> y and y = nil$ would be unsatisfiable, since the atom $y = nil$ describes an empty heap.

The $freed(x)$ atom says that the variable $x$ is not allocated. It has been added to Astral for the purpose of our analysis to explicitly mark freed variables. The key property is that adding $freed(x)$ to a formula containing $x$ discards its models where $x$ aliases with another allocated variable. For example, the formula $x |-> y$ has two models, one where $x$ and $y$ are distinct locations and another where they alias:

$ s_1 = {x |-> ell_0, y |-> ell_1}, h_1 = { ell_0 |-> fields("n": ell_1)} $
$ s_2 = {x |-> ell_0, y |-> ell_0}, h_2 = { ell_0 |-> fields("n": ell_0)} $

Changing the formula to $x |-> y * freed(y)$ removes the second model because it adds the implied inequality $x != y$. The #freed atom works the same for a variable of any sort; the specific fields in the semantics of the #freed atom are irrelevant.

The semantics of the points-to predicate and boolean atoms is as expected. Note that the definition of the points-to predicate guarantees that no atom can point to the special location #freedloc. Otherwise, $x |-> y and freed(x)$ would have a model where $s(y) = freedloc$.

The inductive predicates describe chains of pointers of an unbounded length:

- $ls(x, y)$ describes a singly-linked acyclic list of length zero or more. In the first case, the list is equivalent to $x = y$. Length one is equivalent to a simple pointer $x |-> y * x != y$. All other models contain unnamed allocated memory locations in the chain between $s(x)$ and $s(y)$. Note that $y$ itself is not allocated in any model.

- $dls(x, y, p, n)$ represents a doubly-linked acyclic list of allocations. Unlike in the previous case, both $x$ and $y$ are allocated. The variables $p$ and $n$ represent the `p` and `n` fields of the first and last allocation, respectively. Similar to the singly-linked list, the zero-length case is equivalent to $x = n * y = p$, and the length one is equivalent to a single points-to atom.

- $nls(x, y, z)$ describes a nested acyclic list. The top-level list leads through the `t` field from $x$ to $y$, and there is a sublist from each node's `n` field leading to $z$. The sublist itself is not a nested list, but a singly-linked list. Like the other lists, NLS can be empty (equivalent to $x = y$), but note that even the case of length one contains a sublist of an unbounded length.

#let divider = line(length: 15%, angle: 90deg, stroke: 0.5pt + gray)

#let node(name) = move(dy: -0.3em, text(30pt, name))

#figure(
  caption: [Supported types of lists with the allocated nodes colored blue:\
    singly-linked list, doubly-linked list, nested list],
  align(
    horizon,
    stack(
      dir: ltr,
      spacing: 1em,
      graph.raw-render(
        width: 30%,
        ```
        digraph LinkedList {
            rankdir=LR
            fontsize=40
            node [shape=square style="filled,rounded" fillcolor=lightblue label="" fixedsize=true width=0.6]

            node1 [label="x"]
            node1 -> node2 -> node3 -> end

            end [style="filled,rounded" fillcolor=white]
        }
        ```,
        labels: (
          "node1": node($x$),
          "end": node($y$),
        ),
      ),

      divider,

      graph.raw-render(
        width: 30%,
        ```
        digraph LinkedList {
            node [shape=square style="filled,rounded" fillcolor=lightblue label=""]

            { rank=same; end_l; node1; node2; node3; end_r }
            end_l -> node1 [dir="back"]
            node1 -> node2 -> node3 -> end_r
            node3 -> node2 -> node1

            end_l [style="filled,rounded" fillcolor=white]
            end_r [style="filled,rounded" fillcolor=white]
        }
        ```,
        labels: (
          "end_l": node($p$),
          "node1": node($x$),
          "node3": node($y$),
          "end_r": node($n$),
        ),
      ),

      divider,

      graph.raw-render(
        width: 20%,
        ```
        digraph NestedLinkedList {
            node [shape=square style="filled,rounded" fillcolor=lightblue label=""]

            node1 -> node2 -> node3 -> end
            { rank=same; node1; node2; node3; end }

            end [style="filled,rounded" fillcolor=white]
            subend [style="filled,rounded" fillcolor=white]

            node1 -> sub1a -> sub1b -> subend
            node2 -> sub2a -> sub2b -> subend
            node3 -> sub3a -> sub3b -> subend
        }
        ```,
        labels: (
          "node1": node($x$),
          "end": node($y$),
          "subend": node($z$),
        ),
      ),
    ),
  ),
)

== Frama-C <frama_c>

Frama-C @frama_c_paper @frama_c_book is a framework for building analysis tools aimed at C programs. Frama-C itself is written mainly in the OCaml programming language. Unlike other tools, which focus purely on finding bugs using heuristics, Frama-C specializes in verification. The framework itself is composed of a kernel, multiple plugins, and a GUI to present the results of analyses. The kernel provides common functionality for multiple plugins. The main component is an adapted form of the _C Intermediate Language_ (CIL) @cil, constructed by the Frama-C kernel for use within plugins, as well as an API for its manipulation. CIL has the form of an abstract syntax tree (AST) of the input source code, with extra semantic information added. This includes types of variables, whether a variable is initialized at a certain node, and other information. Frama-C also transforms the input code, making operations like type casts explicit and otherwise making the code more suitable for static analysis. For example, all `return` statements in a function are replaced with `goto` statements that lead to a single `return` at the end of the function. `for` and `while` loops are replaced with a simple infinite loop with an exit condition inside.

One of the goals of Frama-C is to help with the development of custom analyses in the form of plugins. Frama-C handles command-line argument parsing, reports analysis results, and exposes APIs for access to the input CIL.

=== Dataflow Analysis <dataflow>

Besides a general framework for implementing analyses, Frama-C provides generic implementations of common algorithms used for static analysis. One of these is dataflow analysis, implemented in module `Dataflow2`. The purpose of dataflow analysis is to compute certain information for each node of the control flow graph (CFG) in the analyzed program. Nodes in a control flow graph correspond to statements in the original program, and directed edges represent all possible jumps between these statements.

Dataflow analysis starts by assigning an initial state to each node of the CFG, and then progressively updates the values stored in nodes using an implementer-provided _transfer function_, following the edges between them. The transfer function takes the statement of the node and the newly calculated state of the previous node, and produces a new state for the current node. In Frama-C, this function is called `doInstr`.

When a node is reached more than once, the data for this node is computed again based on the data from the previous node, and then joined with the previous data stored for the node. The function that joins the old state with the new state is also provided by the implementer. The name of this function in Frama-C is `combinePredecessors`.

#pagebreak(weak: true)

The implementer must also provide the following functions:

- `doStmt` -- This function is called before the transfer function. The plugin has the option to stop the analysis of this statement or continue normally.
- `doGuard` -- This function is called when a conditional statement is reached. It receives the state from the previous node and the condition expression, and generates two states, each to be used in one of the branches.
- `doEdge` -- This function is called between the analysis of two statements. The function receives both statements and the current state of the first statement. The function can modify this state before continuing with the transfer function of the second statement.

This list is not exhaustive, but the implementation of other functions needed by the `Dataflow2` module is not relevant to the analysis itself.

=== Visitor Mechanism <visitors>

Frama-C provides a convenient way to modify the AST of the analyzed program using the visitor pattern. The plugin constructs an object and defines methods corresponding to the AST node it wants to visit. The method decides whether to leave the node as is, change it, or continue with the visits of its children. This visitor object is then applied to the AST. It is possible to visit AST nodes for expressions, statements, variables, types, and other constructs.

=== Ivette <ivette_section>

Ivette @ivette is a desktop application for displaying the results of analyses, for which it uses a client-server architecture. Ivette, the client, asynchronously polls the server (Frama-C plugin) for data and displays it. The server first has to register the data that has to be shown. To achieve real-time display of information when running an analysis, the plugin must occasionally call `Async.yield` to let Ivette synchronize the data in the GUI application with the data generated by the running analysis. The API for registering and using Ivette can be found in the module `Server`, inside the library `frama-c-server.core`.

= Existing Approaches <existing_approaches>

This section lists some static analysis tools that focus on the memory safety of programs with dynamically allocated data structures, and on tools that use separation logic in their analysis. Both research prototypes and tools used in production are mentioned.

_Predator_ @predatorHP is a verification tool aimed at programs with dynamic data structures. It supports many variants of lists: singly and doubly-linked, nested, cyclic, and others. It uses abstract interpretation over _Symbolic Memory Graphs_ as its domain. The analyzer is able to prove the memory safety of programs -- it proves the absence of invalid pointer dereferences, double-free bugs, and memory leaks. PredatorHP, a parallelized version of the tool running in different configurations, consistently dominates the `LinkedLists` subcategory of the SV-COMP competition (see @results). Recently, the SMG abstraction used by Predator was reimplemented @cpachecker_smg in the CPAchecker analyzer @cpachecker.

_EVA_ @eva is a verifier based on the Frama-C framework, which uses abstract interpretation to detect many kinds of undefined behavior in C programs. It is a general-purpose verifier that tracks the values of both numeric and pointer variables. It can detect invalid memory accesses such as null-pointer dereferences or buffer overruns, reads of uninitialized memory, integer overflows, divisions by zero, and other errors. Its abstract domain uses enumerations and intervals for numeric values and sets of addresses for pointers. However, it does not have the capability to represent the shape of dynamic data structures.

_Infer_ @infer is a production-grade verifier focusing on the memory safety of programs. It uses bi-abductive analysis to infer pre-conditions and post-conditions for functions without the full context of the program, and utilizes separation logic to represent abstract memory states. It uses interprocedural analysis to analyze large codebases and can analyze dynamic data structures such as many variants of lists and trees.

_Broom_ @broom is a verification tool based on bi-abduction and separation logic aimed at verifying the memory safety of low-level programs. It is focused on programs working with dynamic data structures that use advanced pointer-manipulating operations, such as pointer arithmetic, address alignment, and other pointer operations that often appear in the Linux kernel. The tool is able to analyze freestanding code fragments without knowing their full context, which improves the scalability of the analysis.

= Analysis <analysis_section>

The analysis method is an extension of the approach described in @shape_analysis_1 and is inspired by the approach in @shape_analysis_2. The analysis itself consists two main steps. First, the analyzed program is preprocessed into a form more suitable for analysis. Then, the dataflow analysis over the modified AST is executed.

== Preprocessing

The preprocessing starts with an external semantic constant propagation pass with loop unrolling. The purpose of this is to avoid a problem with too loose bounds on the lengths of lists, described in @const_prop.

The next few preprocessing passes convert the program's instructions into a small set of _basic instructions_. _Instruction_ is any statement that does not affect the control flow of the current function. The specific preprocessing passes are described in @conversion_to_basic_instr. This is the set of basic instructions, for which the rest of our analysis is implemented:

- `_const = var;` -- assertion that `var` is an allocated variable (`_const` is only a placeholder),
- `var = var2;` -- assignment of a variable into a variable,
- `var = var2->field;` -- assignment of a variable's field dereference into a variable,
- `var->field = var2;` -- assignment of a variable into a dereferenced field of another variable,
- `*var = var2;` -- assignment of a variable into a pointer dereference,
- `var = *var2;` -- assignment of a pointer dereference into a variable,
- `var = &var2;` -- assignment of a reference to a variable into a variable,
- `fun(var1, ..., varN);` or `var = fun(var1, ..., varN);` -- function call with an optional assignment into a variable, with all arguments being variables.

This is followed by an analysis of C types. As described in @sl_section, when introducing a variable into a formula, we need to provide its sort. In our analysis, this sort is derived from the type of the corresponding C variable. For types that form one of the supported kinds of lists, a special sort predefined by Astral must be used, so that points-to atoms from these variables can later be abstracted to list atoms. For all other structure types, a new sort must be declared and registered in the solver instance. See @type_analysis for a detailed description.

== Dataflow Analysis

The analysis starts at the first statement of the `main` function with #emp as the initial state, and runs the dataflow analysis from this statement. The domain of the dataflow analysis is the disjunction of symbolic heaps. Symbolic heap is a formula in the form of $phi_1 * ... * phi_n$, where each $phi_i$ is an atomic formula. The formulae used by the analysis are not actually represented by the types provided by Astral, but instead use a custom, flattened formula type that is simpler to work with. The details are described in @formulae.

Variables in formulae have two variants, _program variables_ and _logic variables_. Program variables directly correspond to C variables currently in scope with the same name. Logic variables correspond to memory locations that do not currently have a name in the analyzed program. In SL formulae, they are implicitly existentially quantified. More on this in @join_operation. In this text, we refer to logic variables using "primed notation", i.e., as $f'_n$.

The analysis itself is implemented using the `Dataflow2` module provided by Frama-C (see @dataflow), and it is composed of four main components. First, there is the transfer function implemented for the basic instructions described above. Then, there are the simplifications applied to the formulae between instructions. The abstraction (over-approximation of pointer sequences by list predicates) is included in the simplifications. There is also the logic for the join operation. Finally, there is the specialization of formulae for each branch of a condition.

=== Transfer Function

The transfer function takes formulae describing the state before executing an instruction and changes them to reflect the state after the execution. The transfer function always processes the input formulae one by one, applying the operation described below to each input formula (here called $phi$) separately. When the materialization of a variable $x$ is done, the list predicate starting at $x$ is split into a points-to atom from $x$ and the rest of the list. If $x$ is not allocated in a formula during materialization, an invalid dereference is reported. The transfer function is implemented for each of the basic instructions:

- Assignment into the special `_const` variable `_const = a;` means that $a$ is checked to be allocated. If not, an invalid dereference is reported. The formula is not changed in any way. This corresponds to accesses to non-pointer fields in the original source code, see @vars_preprocessing for more details.

- For simple assignment `a = b;`, the resulting formula is $phi[f0\/a] * a = b$ where #f0 is a new logic variable. The renaming is done because the memory location originally referred to by $a$ still has to be represented in the formula.

- For an assignment with a field access on the right-hand side `a = b->f;`, the variable $b$ is first materialized in the formula, and then the target of $b$ at field `f` is found (here called $t_b$). Then, a regular assignment is done with $t_b$ as the assigned value. The resulting formula is $phi[f0\/a] * a = t_b$.

- When processing an assignment to a field `a->f = b;`, $a$ is first materialized, and then the field `f` of the points-to atom from $a$ is changed to $b$. No substitution is done in this case because we are not overwriting the value of any variable.

- `a = *b;` is processed the same way as `a = b->f;`, except that the "field" being dereferenced has a special name `_target`. Pointers to pointers are represented as pointers to structs with a single field `_target`.

- Since our analysis assumes that pointers to pointers are allocated on the stack, `*a = b;` is interpreted as an assignment of $b$ into the variable pointed to by $a$. The target of $a$, called $t_a$ is found, and the formula is transformed as a regular assignment of $b$ into it. After that, the points-to atom from $a$ is changed back to $a |-> fields("_target": t_a)$ (it was affected by the renaming done during the assignment).

- `a = &b;` is processed differently based on whether $a$ is already allocated (there is a points-to atom from $a$ already in the formula). If so, its `_target` field is simply set to $b$. If not, this atom is added. This is done to prevent adding a second points-to atom from $a$, which would make the formula unsatisfiable.

The last instruction is a function call. If the function is from the allocation API (`malloc`, `calloc`, and `free`), it is handled differently from a user-defined function.

- Processing `a = malloc(size);` yields two formulae, one representing the successful allocation with its fields set to new logic variables: $phi * a |-> fields("f"_0: f0, "f"_1: f1, ...)$, the other one representing an allocation failure: $phi * a = nil$.

- `a = calloc(size);` is handled the same as `malloc`, except that all fields of the allocated points-to are set to #nil instead of new logic variables.

- `free(a);` is handled by materializing $a$ and then removing the points-to atom starting at $a$. If the materialization fails, an invalid free is reported instead of an invalid dereference.

All other function calls are handled explicitly by splitting the formula into two parts by reachability from the function arguments, and then running the dataflow analysis recursively on the called function, using the reachable subformula as the initial state. The resulting state of analyzing the function is taken from its return statement. To improve the performance, a cache of _function summaries_ is used. The input to the cache is a modified initial state, the output is a modified output state. The implementation of the cache is described in @summaries.

=== Simplifications

Between analyzing instructions, the state formulae are simplified and abstracted to ensure the termination of the analysis. The reasoning behind some of these simplifications is provided in @simplifications. The following simplifications are applied in this order:

- Program variables going out of scope by the transition between the two instructions are substituted with logic variables.
- Points-to atoms representing pointers to variables going out of scope are removed.
- Spatial atoms unreachable from program variables are removed and reported as memory leaks.
- Logic variables equivalent to program variables are removed from the formula by substituting them with these program variables.
- Chains of spatial predicates are abstracted to list predicates. This step ensures the analysis of loops will terminate. More on this in @abstraction.
- Inequalities and #freed atoms containing logic variables only present in these atoms are removed. Such atoms do not hold any information useful for the analysis.
- List atoms known to be empty are removed or replaced with equivalences. These are the products of previous simplifications and must be removed to avoid accumulating meaningless atoms in formulae. List atoms are assumed to be empty when the start and the end of the list are syntactically equivalent in the formula.
- The formulae are canonicalized, which involves sorting variables in equivalence classes and atoms in formulae. This is done to make the next steps more likely to succeed.
- Formulae differing only in the length bound of a single spatial atom are merged, taking the lower bound of the two as the new bound.
- Formulae are deduplicated by syntactical comparison, and then semantically using the procedure described in @join_operation.

=== Join Operation <join_operation>

#let old = $phi_"old"$
#let new = $phi_"new"$
#let out = $phi_"out"$

When the analysis reaches a statement for the second or subsequent times, a state #old is already associated with the statement. A new state #new is generated using the transfer function as usual, and then these states are joined to produce either a new, combined state to be stored for the statement, or nothing, if the new state is covered by the original state. By covering, we mean that all models of the new state are already included in the models of the original state. In other words, it must hold that $new entl old$, where #old and #new are disjunctions of the individual SL formulae comprising the two states.

The evaluation of satisfiability and entailment checks is cached. As an optimization, an entailment is not evaluated as-is, but is broken down into a series of smaller entailments. This improves the efficiency of the cache because smaller entailments have a higher chance of appearing multiple times during analysis.

The states #old and #new are first split into lists of their formulae, and these lists are concatenated. This list of old and new formulae is first sorted (see @deduplication_sort) and then deduplicated using the following algorithm: a list of deduplicated formulae #out is created empty, and then, one by one, the formulae are added to this list. Before adding a formula $phi_0$ into $out = [phi_1, phi_2, ...]$, the entailments $phi_0 entl phi_1$, $phi_0 entl phi_2$, ... are checked and only if none of them succeed, $phi_0$ is added to #out.

After the deduplication, we must decide if the new state #out is covered by #old. Again, the entailment is split into a series of smaller entailments, where each formula from #out is tested separately. The test of $phi_0$ from #out against $old = [phi_1, phi_2, ...]$ is done formula by formula, if at least one of the entailments $phi_0 entl phi_1$, $phi_0 entl phi_2$, ... succeeds, $phi_0$ is covered by the old state. Each formula from #out is tested this way, and if all of them are covered, the join operation reports that #old covers the new state, and the analysis does not continue beyond this statement.

=== Condition Evaluation

When reaching a conditional statement, the analysis must decide whether to visit the positive and negative branches of the conditional, and how to modify the states used in the analysis of these branches. The only analyzed conditions are the equality and inequality of two pointer variables. All other conditions are treated as nondeterministic -- both branches are analyzed with the unchanged input state. When a nondeterministic condition is reached, any subsequent detection of error in the program is not reported as an error but as an unknown result. This must be done to prevent false detections of invalid dereferences in programs that use lists of bounded length, as is explained in @const_prop.

When reaching a condition `a == b`, the equality of $a$ and $b$ is added to the formulae for the positive branch using `add_eq` described in @eq_distinct. Unsatisfiable formulae are then filtered out. If there are any remaining formulae, the analysis continues into the positive branch with these. If not, the analysis does not continue into the branch at all. For the negative branch, the algorithm is the same, only the inequality of $a$ and $b$ is added to the original formulae instead of equality. The function `add_distinct` is used for adding the inequality, as described in @eq_distinct.

When reaching a condition `a != b`, the approach is the same, except that the positive and negative branches are swapped.

#pagebreak(weak: true)

=== Example

The example in @analysis_example shows how the analysis makes progress on finding a fixpoint of an allocation loop. After three iterations through the loop, the analysis finds the fixpoint to be the following state:

#let pass2(m) = text(red, m)
#let pass3(m) = text(blue, m)

$
  state(
    x |-> f1 * y = x,
    pass2(x |-> y * y |-> f2),
    pass3(ls(bound: 2, x, y) * y |-> f3),
  )
$

Note that after the analysis finds the fixpoint, the state is further simplified to this form through formula generalization:

$
  state(
    pass3(x |-> f1 * y = x),
    pass3(ls(bound: 1, x, y) * y |-> f2),
  )
$

#let line_no = counter("line_no")
#let c(code) = {
  line_no.step()
  context (
    text(fill: rgb("#808080"), raw(line_no.display())),
    h(0.8em),
    raw(lang: "c", code),
  ).join()
}

#v(3em)

#figure(
  gap: 2em,
  caption: [
    Simplified progress of an analysis run,\
    the color of a formula indicates when it was generated:
    #grid(
      align: (horizon, left),
      row-gutter: 0.7em,
      column-gutter: 1em,
      columns: 2,
      square(fill: black, size: 0.5em), "before loop + first loop iteration",
      square(fill: red, size: 0.5em), "second loop iteration",
      square(fill: blue, size: 0.5em), "third loop iteration + after loop",
    )
  ],
  move(
    dx: 1em,
    grid(
      align: left,
      row-gutter: 0.5em,
      columns: 2,
      "Code", "State formulae",
      line(length: 40%), line(length: 40%),
      c("Node *x = malloc(size);"), [],
      [], $x |-> f1$,
      c("Node *y = x;"), [],
      [], $x |-> f1 * y = x$,
      c("while (rand()) {"), [],
      [], $x |-> f1 * y = x$,
      [], pass2($x |-> y * y |-> f2$),
      [], pass3($ls(bound: 2, x, y) * y |-> f3$),
      c("  y->next = malloc(size);"), [],
      [], $x |-> f1 * f1 |-> f2 * y = x$,
      [], pass2($x |-> y * y |-> f2 * f2 |-> f3$),
      [], pass3($ls(bound: 2, x, y) * y |-> f3 * f3 |-> f4$),
      c("  y = y->next;"), [],
      [], $x |-> y * y |-> f2$,
      [], pass2($x |-> f4 * f4 |-> y * y |-> f3$),
      [], pass3($ls(bound: 2, x, f5) * f5 |-> y * y |-> f4$),
      c("}"), [],
      [], pass3($x |-> f1 * y = x$),
      [], pass3($ls(bound: 1, x, y) * y |-> f2$),
      c("y->next = NULL;"), [],
      [], pass3($x |-> nil * y = x$),
      [], pass3($ls(bound: 1, x, y) * y |-> nil$),
    ),
  ),
) <analysis_example>

#pagebreak(weak: true)

== Integration with Ivette

The analyzer supports two modes. It can run both in the terminal and in the graphical application _Ivette_ provided by Frama-C. Ivette starts before the analysis begins and shows the intermediate results of the analysis as they are generated. The user can see the original program (top left window in the screenshot below) and its preprocessed version (top right window). This was especially useful during development, because by hovering the cursor above a part of the original program, the equivalent part of the preprocessed code is highlighted, and vice versa. The GUI also shows a list of messages emitted by the analyzer (bottom left window). These messages are divided into categories based on their origin and can be filtered. Debug messages are emitted from the transfer function, simplifications, function call handling, Astral queries, and other sources. These messages show the state before and after the given step. When highlighting a statement in the preprocessed AST, the GUI shows the current state associated with the statement (bottom right window).

#figure(
  caption: "Ivette GUI showing the intermediate results of the analysis",
  image("ivette_screenshot.png"),
)

= Implementation Details <implementation_details>

KTSN is implemented in the OCaml programming language. This section describes various aspects of the analysis in more detail, with a focus on how certain parts of our method are implemented in the analyzer.

== Preprocessing

This section describes the details of AST preprocessing done externally by Frama-C (invoked using Frama-C command line parameters) and internally using the CIL Visitor mechanism (see @visitors). Many of the simple preprocessing passes are implemented directly in #plugin_link("src/preprocessing/preprocessing.ml"). This module also contains the function `preprocess`, which runs all of the preprocessing passes in order. This is called directly from `main` before running the analysis.

=== Preprocessing of Variables <vars_preprocessing>

Some of the preprocessing passes described below replace some expressions with special variables, namely `_nil` and `_const`. These are used to simplify the AST: instead of containing multiple AST nodes such as a constant (zero) or a cast of a constant to a pointer type (`NULL`), all expressions are simply variables. `_nil` is used to represent a `NULL` pointer, whereas `_const` replaces any expression that cannot be analyzed, such as arithmetic expressions or nondeterministic conditions. One other use of `_const` is that assignments into it are seen as an "allocation check" of the variable being assigned. For example, `_const = var;` is not interpreted as a literal assignment into the variable `_const`, but as a check that `var` is allocated. These assignments are created during preprocessing, when a field access to a non-pointer variable is found. For example, the following statement:

```c
int data = list->data;
```

will get converted to

```c
_const = list;
```

This will not cause any change to the state formulae during the dataflow analysis, but each formula will be checked to ensure that the variable `list` is allocated.

=== Conversion into Basic Instructions <conversion_to_basic_instr>

The conversion of all instructions in the program to basic instructions is done over multiple passes. First, all AST nodes irrelevant for the analysis replaced with `_const`. This includes arithmetic and binary operations, type casts, integer comparison operations, and others. Then, variable initialization is replaced by simple assignments. Next, function call arguments with types irrelevant to the analysis are removed. What constitutes a _relevant type_ is defined in @type_analysis.

At this point, the AST should ideally only contain analyzable AST nodes. However, it is also necessary to split expressions in assignments, function call arguments, and conditions into a series of elementary assignments. Each complex statement is replaced by a block containing temporary variables, to which parts of the original expression are assigned. This is described in detail in @stmt_split.

After this, all assignments into variables with irrelevant types are removed or replaced by a check that the dereferenced variable is allocated. At this point, every instruction in the program should be one of the basic instructions.

=== Semantic Constant Propagation <const_prop>

The only conditions the analysis is able to process are equalities and inequalities of variables. This turned out to be a problem in many of the SV-COMP benchmarks (for example, in #svcomp_link("list-simple/sll2c_update_all.c")), because they often contain manipulation of lists of bounded size. Take the following code as an example:

```c
int len = 5;

while (len) {
  append_node(&list, data);
  len--;
}

list->next->next->data = 42;
```

Because our analysis considers all other conditions nondeterministic (see @conditions), it will over-approximate and include the possibility of zero iterations over this loop. This means that the state after the loop will contain a formula describing a list of zero length. The problem is that the code below the loop will rightfully assume the list has non-zero length, accessing the n-th node in the list without checking that the access is valid. The analysis will then detect a potential invalid dereference, and because a nondeterministic condition had been reached, it will return an unknown result.

The solution to this is to run an analysis of integer values before our analysis and statically determine the number of iterations over the loop. Analysis of numeric values is already implemented in Frama-C in the _Semantic constant folding_ (SCF) plugin @scf_plugin, which in turn uses the EVA @eva_user_manual plugin for value analysis. This is used in combination with Frama-C's loop unrolling setting `ulevel` to unroll a few iterations of every loop. This preprocessing pass will produce roughly the following code from the example above, when used with the setting `-ulevel=2`:

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

list->next->next->data = 42;
```

As you can see, the first two iterations of the loop have been unrolled above the loop body, and the variable `len` inside the unrolled conditional jumps was replaced by its value at that moment by the SCF plugin. These constant conditions are then removed altogether, see @conditions.

#pagebreak(weak: true)

=== Preprocessing of Conditions <conditions>

Conditions are preprocessed in the following passes:

- Removing constant conditions -- this is done before the removal of all constants from the AST, and therefore the conditions left by the SCF pass (see @const_prop) are still intact. Using the `remove_const_conditions` visitor in #plugin_link("src/preprocessing/preprocessing.ml"), all condition expressions that can be folded to a constant using `Cil.constFoldToInt` are evaluated and removed from the code, replacing the original `If` AST node with the `Block` node of the branch that would be executed. This is especially useful for unfolded loop iterations.

- Removing the `!` operator -- using the visitor `remove_not_operator` in #plugin_link("src/preprocessing/preprocessing.ml"), the following replacements are made:
  - negation of negation is replaced by the inner expression,
  - negation of equality is replaced by inequality,
  - negation of inequality is replaced by equality.
  These replacements are made because the evaluation of many macros in the SV-COMP benchmarks creates exactly these types of conditions. Simply removing them is easier than implementing the analysis to cover them.

- Splitting the condition expressions -- the condition expression is split the same way as other expressions described in @stmt_split. This pass is implemented in #plugin_link("src/preprocessing/condition_split.ml"). This pass also converts implicit pointer comparisons to explicit ones. Specifically, these replacements are made:
  - `(var)` is converted to `(var != _nil)`,
  - `(!var)` is converted to `(var == _nil)`.
  Conditions that cannot be preprocessed into a pointer comparison are replaced with the special constant `_const` instead. These are then considered nondeterministic during analysis.

After these passes, all analyzable conditions have the form of an equality or an inequality of two pointer variables. All other conditions simply contain the constant `_const`.

=== Splitting Complex Statements <stmt_split>

To split any supported instruction into a series of basic instructions, this pass must introduce new variables to hold temporary values. Because the analysis handles program and logic variables differently (see @formulae), it is more convenient to give these variables as short a lifetime as possible. Each split statement is therefore replaced with a block that holds the necessary temporary variables.

The preprocessing pass itself is implemented in #plugin_link("src/preprocessing/stmt_split.ml"), but the logic behind recursively splitting a single expression into a series of simple assignments, yielding a single variable, is implemented in #plugin_link("src/preprocessing/block_builder.ml"). This logic is also used by the preprocessing pass for conditions, see @conditions.

The splitting itself is done by storing the result of every single dereference into a temporary variable and using the previously created temporary variables in the latter assignments. For example, the following statement:

```c
var1->field1->field2 = (*var2)->field3;
```

will be split into this block:

#pagebreak(weak: true)

```c
{
  tmp1 = *var2;
  tmp2 = tmp1->field3; // tmp2 contains the value of the original expression
  tmp3 = var1->field1;
  tmp3->field2 = tmp2; // this line performs the original assignment
}
```

Note that it is not possible to extract the last l-value `tmp3->field2` into a temporary variable, and to assign `tmp2` into this variable, because it would not change the value in memory pointed to by the `tmp3` pointer.

=== Analysis of C Types <type_analysis>

The purpose of this analysis is to determine which C types might implement one of the supported list variants, and to store this information for later use. Thanks to this, our analysis does not need any kind of struct annotation. This analysis pass is implemented in #plugin_link("src/preprocessing/types.ml").

The only _relevant types_ for the analysis are pointers to structures (`struct X *`). Pointers to these pointers (`struct X **`, `struct X ***`, etc.) are also supported, but only as pointers to variables on the stack. This is a limitation given only by the current implementation, as the support for pointers to variables on the stack was added late during development because many SV-COMP tests relied on this feature. However, SV-COMP does not contain any tests where pointers to pointers are allocated on the heap, so there was little reason to work on this specific feature over others.

The pass first iterates through all structure types in the code and applies the following heuristic to determine if it implements of the supported linked lists. The result of this heuristic is one of:
- singly-linked list,
- doubly-linked list,
- nested list,
- generic struct.

The algorithm of the heuristic is the following: first, all relevant fields of a structure are recursively analyzed to get their heuristic result. Then:

- If a structure contains exactly one pointer to the structure itself and no pointers to structures marked as a singly-linked list, it is itself marked as a singly-linked list.
- If a structure contains two pointers to itself and no pointers to singly-linked lists, it is marked as a doubly-linked list.
- If a structure contains one pointer to itself and one pointer to a singly-linked structure, it is marked as a nested list.
- Otherwise, it is marked as a generic structure. A new sort and Astral struct definition is created.

Then, each type is connected to its corresponding sort and struct definition in the `type_info` table. The struct definition contains the names and sorts of all fields in the structure.

This method of analysis has a drawback: not all implementations of supported linked lists will use one of these structures. For example, in the SV-COMP benchmark #svcomp_link("forester-heap/sll-01-1.c"), the nested list structure is defined as follows:

#pagebreak(weak: true)

```c
typedef struct TSLL
{
	struct TSLL* next;
	struct TSLL* inner;
} SLL;
```

During the program's runtime, the same structure is used to represent both the top-level list and the nested sublists, which always set their `inner` field to NULL. Since this will be wrongly detected as a doubly-linked list, the analysis will not be able to do abstraction on the generated formulae. Fixpoint for the list creation loop will not be found, and the analysis will timeout.

There are a few possible ways to fix this. One option would be to rely on the user to provide the correct list type and field types manually using Frama-C annotations. Another option might be to detect the correct list type dynamically during analysis, based on the shapes of the data structures that are appearing in the formulae.

== Representation of SL Formulae <formulae>

The types used to represent formulae in Astral have a tree structure that requires pattern matching to access every term. This makes sense for a solver that needs to support formulae of all shapes on its input, but for the purpose of our analysis, we need a flattened representation of the exact shape of the formulae we use. This will avoid having to pattern-match terms for which we already know their type, and it will allow us to define shorthands for predicates that have to be expressed by a more complex structure in Astral's types. The formula type, along with a series of functions for manipulating these formulae, is implemented in #plugin_link("src/formula.ml").

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

State (type used by the dataflow analysis to represent the state in each CFG node) is a list of formulae, and each formula is a list of atoms. `atom` is an enumeration of the predicates used by the analysis.

Variables used by the formulae are represented using the original type from Astral. Program variables have the same name as their corresponding C variables, while logic variables are distinguished by names containing the character `!`. When adding a logic variable to a formula, we need to give it a fresh name that does not appear anywhere else in the formula.

=== Spatial Atoms

`PointsTo` represents all variants of points-to predicates. Because we need to distinguish between pointers of the three basic list types and all other pointers, the targets of the pointer are represented in a separate structure `pto_target` with the three basic list variants and a `Generic` variant that simply associates field names with target variables.

List predicates are represented using three atoms, with a _length bound_ `min_len` indicating the minimum length of the list predicate. For singly-linked lists and nested lists, the maximum valid bound is 2+, and for doubly-linked lists, it is 3+. This is given by the limitations of encoding list predicates into Astral formulae, see @conversion_to_astral.

The module also provides functions for manipulating spatial atoms, such as finding and changing the target of a points-to atom in a formula, checking that a variable is allocated in a formula, or converting points-to atoms to lists. The type `field_type` is used to represent the type of a field in a points-to atom. It is defined in #plugin_link("src/preprocessing/types.ml") in the following way:

```ocaml
type field_type = Next | Prev | Top | Other of string | Data
```

The variant `Other` is used to represent an arbitrary field using its name.

Another operation implemented for formulae is the _materialization_ of a variable inside a formula. This operation unrolls a single points-to predicate from a list predicate, so that subsequent operations can work with the points-to atom directly. By definition, it asserts that the materialized variable is allocated. Materialization involves adding a logic variable serving as the new start of the list predicate. When materialization is performed on a variable that is already a source of a points-to atom, the formula is left unchanged.

The function `materialize` is implemented recursively and returns a list of formulae because materialization can produce more than one formula. When materializing a list with the length bound of one or higher, the length bound is simply decremented, and the unfolded points-to atom is added to the formula. When the list atom has the length bound 0+, a case split is performed:
- One option is for the list to have a minimum length of at least one. In this case, the length bound is incremented, and `materialize` is called recursively on this modified formula.
- The other option is for the list to have a length of zero. In this case, it is replaced with an equivalence, and `materialize` is called recursively.

The results of evaluating both cases are then concatenated and returned as a list of materialized formulae.

For example, materializing the variable $x$ in $ls(bound: 2, x, y)$ yields a single formula $x |-> f0 * ls(bound: 1, f0, y)$. Materializing $x$ in $ls(bound: 0, x, y) * ls(bound: 1, y, z)$ yields a list of two formulae:

$
  x |-> f0 * ls(bound: 0, f0, y) * ls(bound: 1, y, z)\
  x = y * x |-> f0 * ls(bound: 0, f0, z)
$

The first formula represents the case where the original list $ls(bound: 0, x, y)$ had a length of at least one. The second formula represents the case of the list being empty. Note that the materialization also explicitly makes $x$ the source of the unfolded points-to atom.

Materialization of DLS variables is done similarly, with the difference that DLSs can be materialized from any side. In $dls(x, y, p, n)$, either $x$ or $y$ can be materialized. During the materialization in NLSs, an additional logic variable representing the start of the SLS sublist is introduced. For example, materializing $x$ in $nls(bound: 1, x, y, z)$ yields

$ x |-> fields("t": f0, "n": f1) * ls(bound: 0, f1, z) * nls(bound: 0, f0, y, z) $

where #f1 is the additional SLS variable.

=== Equality and Inequality <eq_distinct>

`Eq` represents a list of variables that form an equivalence class. Equivalences in formulae are not handled arbitrarily as pairs of equivalent variables, but a minimal list of disjoint equivalence classes is maintained throughout the analysis. When adding a new equivalence into a formula, the `add_eq lhs rhs` function is used. This function first checks if `lhs` or `rhs` is present in any of the existing equivalence classes, and then:

- If both variables are already in a single equivalence class, nothing happens.
- If both variables are in different equivalence classes, these classes are merged into one.
- If just one variable is in an existing equivalence class, the other variable is added to it.
- Otherwise, a new equivalence class with the two variables is added to the formula.

`Ne` represents the inequality of two variables. Unlike for equality, there is no need to represent a list of variables all distinct from each other. Nonetheless, there is a function `add_distinct lhs rhs` for adding inequalities with special behavior. If there is a list atom in the formula with its source and destination equivalent to `lhs` and `rhs` of the inequality, the length bound of the list is increased to reflect this, instead of adding an explicit inequality. Specifically,

- If $x != y$ is added to $ls(bound: 0, x, y)$, it becomes $ls(bound: 1, x, y)$.
- If $x != n$ or $y != p$ is added to $dls(bound: 0, x, y, p, n)$, it becomes $dls(bound: 1, x, y, p, n)$.
- If $x != y$ is added to $dls(bound: 1, x, y, p, n)$, it becomes $dls(bound: 2, x, y, p, n)$.
- If $x != y$ is added to $nls(bound: 0, x, y, z)$, it becomes $nls(bound: 1, x, y, z)$.

This comes from the fact that all zero-length list predicates require the source and the target to be equal. By adding this inequality, we essentially restrict the possible cases to those where the list has a length of at least one. Similarly, in the third case, if the first and last allocated locations in a DLS differ, the list must have a length of at least two.

The reason for doing this is to prevent false detections of invalid dereferences when analyzing code that asserts a list to be non-empty using a condition, and then accesses this list. For example, consider the following code:

```c
a = construct_list();
if (a != NULL) {
  a->data = 42;
}
```

The state after the call to `construct_list` might be $ls(bound: 0, a, nil)$. When analyzing the condition, the inequality $a != nil$ must be added. If we simply added the inequality atom to the formula, we would detect a possible invalid dereference at the assignment. The code checking that a variable is allocated simply materializes the variable and checks that there is a spatial atom with its source equivalent to that variable. This check would fail for the formula $ls(bound: 0, a, nil) * a != nil$. Increasing the bound on the list predicate solves this issue.

=== Translation to Astral Types <conversion_to_astral>

The translation of equality, inequality, and #freed atoms to the types of Astral's formulae is trivial, as is the translation of points-to atoms of the three predefined list types. Translating points-to atoms with sorts defined at runtime involves adding a struct definition for the variable's sort that was created during the analysis of types (see @type_analysis).

Translating list predicates involves expressing the length bounds implicitly, because Astral does not have a way to explicitly set the minimum length of a list predicate. Therefore, only $ls(bound: 0, x, y)$, $dls(bound: 0, x, y, p, n)$ and $nls(bound: 0, x, y, z)$ are translated directly to their corresponding list predicates in Astral's types. The rest is translated recursively according to the following rules:

#let changeto = $ #h(1em) && -> #h(1em) $

$
  &ls(bound: 1, x, y) #h(3em) changeto ls(bound: 0, x, y) * x != y\
  &ls(bound: 2, x, y) changeto ls(bound: 0, x, y) gneg (x |-> y)\
  &dls(bound: 1, x, y, p, n) changeto dls(bound: 0, x, y, p, n) * x != n\
  &dls(bound: 2, x, y, p, n) changeto dls(bound: 1, x, y, p, n) * x != y\
  &dls(bound: 3, x, y, p, n) changeto dls(bound: 2, x, y, p, n) gneg (x |-> fields("p": p, "n": y) * y |-> fields("p": x, "n": n))\
  &nls(bound: 1, x, y, z) changeto nls(bound: 0, x, y, z) * x != y\
  &nls(bound: 2, x, y, z) changeto nls(bound: 1, x, y, z) gneg (exists f0. thick x |-> fields("t": y, "n": f0) * ls(bound: 0, f0, z))\
$

The idea behind this translation is to force the list to be non-empty by adding an inequality for the 1+ cases, and to explicitly discard the length one model using guarded negation ($gneg$) for the 2+ cases. $nls(bound: 2, x, y, z)$ describes a nested list where the _top-level list_ has at least two allocated nodes. The bound does not place any restriction on the lengths of its sublists. Translating $nls(bound: 2, x, y, z)$ requires adding a new logic variable #f0 that is existentially quantified _under_ the negation. If the quantifier were placed before the negation, the meaning would be different, because negation changes existential quantifiers to universal and vice versa.

== Interprocedural Analysis <summaries>

The analysis of a function call is implemented in #plugin_link("src/func_call.ml") and consists of multiple steps.

=== Reachability Split

First, the input formula is split into two subformulae by reachability from the function arguments. The reachable subformula is found by first finding all reachable spatial atoms. The algorithm starts with a single variable and finds a spatial atom, where this variable is a source variable. _Source variables_ of spatial atoms are variables at the position of $x$ in $ls(x, \_)$, $dls(x, x, \_, \_)$, $nls(x, \_, \_)$, and $x |-> fields(\_)$. This atom is added to the set of reachable atoms, and the algorithm continues recursively with all target variables of the spatial atom. _Target variables_ are all variables in spatial atoms that are not source variables. This algorithm is run for each of the argument variables.

After this, the set of all reachable variables is collected from the reachable spatial atoms. The arguments themselves and #nil are also included in this set. All equivalence classes are then filtered so that only reachable variables remain. Finally, inequalities are filtered so that only those, in which both variables are reachable, remain. Similarly, #freed atoms are filtered.

The reachable subformula is a merge of the reachable spatial atoms and filtered equivalence classes, inequalities, and #freed atoms. The unreachable subformula is then composed of all atoms of the original formula not present in the reachable subformula.

This is done to simplify the analysis of the called function as much as possible by not including irrelevant atoms and variables in the formulae. It also increases the efficiency of the summaries cache because the cache input does not contain irrelevant data, which would prevent cache hits with slightly different input formulae.

=== Anchor Variables

After extracting the reachable subformula, it is necessary to rename variables in the formula from argument names to parameter names. However, simply renaming the variables on function entry and then back on function return would also change the values of variables _after_ the call, for example:

```c
void set_to_null(Node *x) {
  x = NULL;
  return;
}

a = alloc_node();
set_to_null(a);
a->data = 42;
```

If the argument name `a` were simply renamed to parameter name `x` before analyzing the function `set_to_null`, the original state before calling the function $a |-> nil$ would be passed into the function as $x |-> nil$. After the analysis of the function body, we would get the state $x = nil * f0 |-> nil$, which would be simplified to just $x = nil$, and a memory leak would be reported. After returning from the function, the state at the assignment would be $a = nil$, which would falsely report an invalid dereference.

For this reason, _anchor variables_ are added to the input formula before the renaming. Anchor variables are treated as regular program variables. Otherwise, simplifications would remove them from the formula. In the case of the program above, the anchor variable $A_a$ will first be added as equal to $a$, and then $a$ will be renamed to the parameter name $x$. The resulting state used for the analysis of the function body will then be $x |-> nil * x = A_a$.

After the analysis of the function, we will get the formula $f0 |-> nil * f0 = A_a * x = nil$, in which the $A_a$ anchor will be renamed back to $a$, yielding the original formula $a |-> nil$ after subsequent simplification passes.

=== Function Summaries and Function Analysis

To avoid recomputing the analysis of a function call every time that the function is reached, a cache of _function summaries_ is maintained. A function summary is the mapping of an input formula to an output state. Input to the cache is a pair of the called function and the processed input formula after adding anchor variables and renaming arguments to parameters. Caching cannot be done on the original input formula because argument names will differ at different call sites. The conversion of the output state will ensure that a summary created at one call site will be valid at any other call site.

If a summary is not found in the cache, the analysis of the function begins. Unfortunately, the implementation of dataflow analysis in Frama-C does not directly support interprocedural analysis, so we must manually backup the analysis context of the current function and create a new context for the called function. This context consists of one table for the analysis results and a second table for storing the number of loop iterations in underapproximation mode (see @configuration for more details). The context is represented by the type `function_context` and stored in a global variable.

After creating the new context, the initial state for the first statement in the called function is set to the modified input formula, and the analysis is started on this statement. After the analysis finishes, the result state is read out from the result table, and the original context is restored.

The result state must be processed before it is returned from the transfer function. Each formula of the state is processed separately: first, the anchor variables are renamed back to the original argument names, and then the unreachable subformula is added back to the resulting formula. After this, if the call instruction assigns the call result to a variable, the variable returned by the `return` statement from within the called function is assigned to the call result variable. Frama-C preprocesses all functions to have a single `return` statement with a simple variable as the returned expression.

Finally, all points-to atoms representing pointers to variables on the stack created inside the function are removed, and then all program variables from inside the function are substituted with logic variables, to be removed by the subsequent simplification passes. Formulae processed this way are returned from the transfer function.

== Simplifications <simplifications>

Simplifications are done on the formulae of the state between the analysis of two instructions to reduce the size of formulae and the number of variables in them to speed up solver queries. Most of the simplification passes are implemented in #plugin_link("src/simplification.ml") and called from the `doEdge` function inside #plugin_link("src/analysis.ml").

Substitution of program variables going out of scope by logic variables is done to enable the abstraction to work. Abstraction can only remove logic variables from the formula, so if variables created within loops were not turned into logic variables at the end of the loop body, it would never have the option to introduce list predicates into the formula. As an example, consider the following code:

```c
List *a = allocate_node();
while (rand()) {
  List *b = allocate_node();
  a->next = b;
  b->next = NULL;
}
```

The formula describing the state at the end of the first loop iteration might be $a |-> b * b |-> nil$. Because the variable $b$ is going out of scope at the end of the loop body, the formula will be simplified to $a |-> f0 * f0 |-> nil$, which will enable the abstraction into $ls(bound: 2, a, nil)$ before the analysis returns to the start of the loop.

Spatial atoms starting at a logic variable are removed if the logic variable does not appear in any other spatial or equality atom. It can appear in distinct and #freed atoms because these do not make the logic variable reachable from any program variable in the formula. The removal of a spatial atom is reported as a memory leak, except when removing a list with a length bound of zero. Reporting these would cause false detections of memory leaks. For example, when unrolling a nested list, an SLS atom with the length bound zero is added, no matter the original sublist length.

Removing logic variables in equivalence classes is done by first filtering out equivalence classes with fewer than two variables. Then, in each equivalence class, a substitution target variable is found according to the following rules in order:
- If the equivalence class contains #nil, it is used as the substitution target.
- If the equivalence class contains a program variable, it is used.
- Otherwise, a logic variable is used.

All logic variables in each equivalence class are then substituted with its substitution target. After this, all equivalence classes are deduplicated, getting rid of the variables duplicated during substitution. Finally, equivalence classes with fewer than two variables are again removed.

Formula canonicalization is done to convert formulae into a syntactically consistent shape, to increase the efficiency of the query cache. Canonicalization is done in these steps:
- All variables of the formula are collected and sorted, and then explicitly set as the source of spatial atoms. Variables that are not a source of any spatial atom are skipped.
- Variables in equivalence classes and inequalities are sorted.
- Atoms in the formula are sorted.
- All logic variables in the formula are in order renamed to a sequence of names of the form\ `!0`, `!1`, `!2`, ...

This is certainly not an ideal way to canonicalize formulae because even a single difference in variable names between two formulae can affect their final order of atoms, but it proved sufficient for the next step to work.

Generalization of similar formulae is done only when two formulae differ in the length bound of a single spatial atom. This comparison is run for every pair of formulae in the state. If it succeeds, the pair is replaced with the generalized formula. Otherwise, it is left unchanged. The length bound in the generalized formula is the smaller of the original two. If two formulae differ in a single atom and one of the differing atoms is a points-to atom, it is converted to a list atom with the length bound of one. If the generalization fails, this change is not propagated to the original state. This simplification solves the common problem where an allocation loop produces multiple formulae describing different lengths of a list.

```c
a = NULL;
while (rand()) {
  alloc_node(&a);
}
```

This analysis might produce the following state after the loop:

$
  state(
    a = nil,
    a |-> nil,
    ls(bound: 2, a, nil),
  )
$

This generalization will merge the latter formulae into one:

$
  state(
    a = nil,
    ls(bound: 1, a, nil)
  )
$

This generalization could be extended to turn the resulting state into $ls(bound: 0, a, nil)$ that would cover all cases, but it would require a more complex method than to just compare a single pair of differing atoms, because the equality of $a$ and #nil could exist inside a larger equivalence class.

== Abstraction <abstraction>

The abstraction of each list type is done in a separate pass, separately for each formula. In general, the point of abstracting chains of points-to predicates into list predicates is to create invariants for loops. Abstraction is only done when returning from the end of a loop back to the beginning (i.e., from `b = b->next;` to `while (rand())`). Consider the following code:

```c
a = alloc_node();
b = a;

while (rand()) {
  b->next = alloc_node();
  b = b->next;
}
```

If there were no abstraction in place, the analysis would generate longer and longer points-to chains forever:

$
  &a |-> nil &&* a = b\
  &a |-> b &&* b |-> nil\
  &a |-> f0 &&* f0 |-> b &&* b |-> nil\
  &a |-> f0 &&* f0 |-> f1 &&* f1 |-> b &* b |-> nil\
  \
  &dots.c
$

#let abstr = $ls(bound: 2, a, b) * b |-> nil$

The abstraction will instead turn the third formula into #abstr, which will abstract over all possible lengths of the points-to chain that would otherwise be generated. This formula (along with the first two generated formulae) will serve as the fixpoint for this loop (a state that is valid in every iteration of the loop). This is because all possible lengths of the points-to chain together make the set of models of the abstracted formula:

$
  a |-> f0 &* f0 |-> b &&* b |-> nil &entl abstr\
  a |-> f0 &* f0 |-> f1 &&* f1 |-> b * b |-> nil &entl abstr\
  a |-> f0 &* f0 |-> f1 &&* f1 |-> f2 * f2 |-> b * b |-> nil #h(2em) &entl abstr\
  \
  dots.c
$

=== Singly-Linked Lists

Abstraction of SLSs is done by iterating through all spatial atoms with the appropriate sort and trying to find a second spatial atom starting at the end of the first one. Points-to atoms are again treated as list atoms with a length bound of one. For example, when processing $ls(bound: 0, x, y)$, the algorithm would look for a spatial atom starting at $y$. If found, the following conditions must be met to proceed with the abstraction:

- The middle variable must be a logic variable not occurring in the rest of the formula. Otherwise, joining $ls(bound: 0, x, y) * ls(bound: 1, y, z)$ into $ls(bound: 1, x, z)$ would lose information about the variable $y$. The occurrence of the middle variable in the formula is checked only in spatial and equivalence atoms, because the variable can only be reached through these.

- The source variable of the first list must be distinct from the target variable of the second list. In the previous example, this would mean ensuring that $x != z$. This is checked using the solver by adding an equality of the two variables into the formula and checking satisfiability. If such formula is satisfiable, the variables are not distinct, and the abstraction cannot be done because the predicate $ls(x, y)$ describes an _acyclic_ chain of pointers.

If the conditions are met, the two atoms are replaced with a single list atom of length $min(l_1 + l_2, 2)$ where $l_1$ and $l_2$ are the lengths of the original lists. For example: $x |-> f0 * ls(bound: 2, f0, nil)$ will be abstracted to $ls(bound: 2, x, nil)$.

=== Doubly-Linked Lists

The abstraction of DLSs is done similarly, except that the conditions needed for abstraction are slightly different. For abstracting $dls(bound: 2, x, f0, nil, f1) * dls(bound: 2, f1, y, f0, nil)$ into $dls(bound: 3, x, y, nil, nil)$, the following conditions must hold:

- Either the first and the last allocated variable of both atoms must be the same (that signifies the lists have length at most one), or the last allocated variable of the first list (#f0) and the first allocated variable of the second list (#f1) must be logic variables unreachable from the rest of the formula, like in the SLS case.

- The `p` field of the second list must point back to the last allocated variable of the first list.

- The `p` and `n` fields of the first allocated variable of the first list and the last allocated variable of the last list, respectively, cannot point back into the list.

- Like in the SLS case, the list must not be cyclic -- this is checked using the solver both forward and backward.

The length bound of the joined list will be $min(l_1 + l_2, 3)$, because unlike in SLS, there is a way to translate DLSs with a minimum length of three.

=== Nested Lists

NLSs are abstracted similarly to SLSs, except that an additional check must be done that the SLS sublists end up in the same variable. SLS abstraction is run first, so we can expect the sublists to be abstracted into a single list atom. The algorithm for joining two NLS atoms differs based on the type of atom:

When joining two points-to atoms, $x |-> fields("t": f0, "n": f1) * f0 |-> fields("t": y, "n": f2)$, the following options are tried in order:
- If #f1 is equal to #f2, the atoms are joined as-is into $nls(bound: 2, x, y, f1)$.
- If the formula contains $ls(f1, z)$ and $z$ is equal to #f2, the sublist from #f1 is removed and the atoms are joined into $nls(bound: 2, x, y, z)$.
- If the formula contains $ls(f1, z_1) * ls(f2, z_2)$ and $z_1$ is equal to $z_2$, the sublists from #f1 and #f2 are removed and the atoms are joined into $nls(bound: 2, x, y, z_1)$.

When joining a list atom $nls(x, f0, z) * f0 |-> fields("t": y, "n": f1)$, these options are tried in order:
- If $z$ is equal to #f1, the atoms are joined as-is into $nls(x, y, z)$.
- If the formula contains $ls(f1, f2)$ and $f2$ is equal to $z$, the sublist from #f1 is removed and the atoms are joined into $nls(x, y, z)$.

The only way to join two list atoms $nls(x, f0, z_1) * nls(f0, y, z_2)$ into $nls(x, y, z)$ is when $z_1$ and $z_2$ are already equal.

All equivalences are checked syntactically. All variables at the start of the deleted sublists must be logic variables unreachable from the rest of the formula. The reason for checking this many options instead of just looking for sublists everywhere is that an $nls(x, y, z)$ atom already contains sublists leading to $z$. For example, it is not possible to join $nls(x, f0, f1) * ls(f1, z) * f0 |-> fields("t": y, "n": z)$ into $nls(x, y, z)$ because that would violate the semantics of the NLS atom, in that all sublists leading into $z$ must be disjoint.

== Deduplication Sorting <deduplication_sort>

Because the deduplication algorithm in the join operation processes formulae in order, it is beneficial to first sort these formulae so that the chances of finding duplicate formulae are as high as possible. Specifically, it is always better to have a higher length bound on the list atoms on the left side of the entailment. For example, the entailment $ls(bound: 0, x, y) entl ls(bound: 2, x, y)$ will not succeed, but $ls(bound: 2, x, y) entl ls(bound: 0, x, y)$ will.

The sorting is done by defining a comparison function that accepts two formulae and puts them in the correct order. This function first calculates the lowest number of allocations for each of the formulae and then puts the higher of the two on the left side of the entailment. This makes sense because the formula with the higher number of allocations can have a subset of the models of the other formula, but not the other way around. For example, this entailment holds:

$ ls(bound: 2, x, y) entl ls(bound: 1, x, y) $

But this does not:

$ ls(bound: 1, x, y) entl ls(bound: 2, x, y) $

The score for a formula is calculated by adding up all length bounds on its list atoms, while points-to atoms are counted as 1. Additionally, if a formula contains any number of list atoms, its score is decreased by one. This is done to prevent a points-to atom from being on the right side of an entailment:

$
  x |-> y &entl ls(bound: 1, x, y)\
  1 &> 0
$

This method also works for some combinations of points-to and list predicates:

$
  x |-> f0 * ls(bound: 0, f0, y) &entl ls(bound: 0, x, y)\
  0 &> -1
$

And for multiple list atoms in one formula:

$
  ls(bound: 1, x, f0) * ls(bound: 2, f0, y) &entl ls(bound: 2, x, y)\
  2 &> 1
$

If this comparison does not decide the order of the entailments, a second score is calculated. The algorithm is the same, except that points-to atoms are not counted at all. In this case, the formula with the lower score is placed on the left side of the entailment. This will order formulae such as these:

$
  x |-> f0 * ls(bound: 1, f0, y) &entl ls(bound: 2, x, y)\
  1 &gt.not 1 #h(2em) "(first score)"\
  0 &< 1 #h(2em) "(second score)"
$

The second scoring does not affect whether the entailment will succeed or not, but it will keep the simpler, more abstracted formula in the deduplicated list instead of the materialized form.

== Configuration <configuration>

There are a few configuration options to change how the analysis performs certain operations. Here is a list of the relevant ones.

`-sl-edge-abstraction` enables the abstraction of formulae on every edge between two statements, not just at the end of a loop. This can significantly decrease the analysis times when enabled because the abstraction is done earlier, but can lead to less precise results on some programs because of earlier over-approximation.

`-sl-no-edge-deduplication` disables formula deduplication on every edge between two statements. Note that even when this option is enabled, deduplication is done during the join operation.

`-sl-no-simple-join` disables the optimization in formula deduplication. When this option is used, the entailments are computed directly using disjunctions of formulae instead of formula by formula.

`-sl-max-loop-cycles <N>` enables an underapproximation mode, where each loop is traversed at most `N` times when reached. When enabled, the analysis no longer proves the correctness of programs, but the output of the analysis can still help find bugs in programs where the normal analysis would get stuck in a loop, for which it cannot find a fixpoint.

= Results <results>

The KTSN analyzer was evaluated on a small set of crafted programs and on a larger set of SV-COMP benchmarks @svcomp to compare it with existing tools.

We use the Benchexec framework @benchexec to run experiments on both sets of benchmarks. Benchexec is a benchmarking framework that automatically evaluates a program on a set of benchmarks while constraining system resources. Benchexec requires a tool definition in the form of a Python module that tells it how to execute the analyzer on a given test program and how to extract its result. This module is provided in #plugin_link("bench/ktsn/ktsn.py"). The benchmarks are then executed using a benchmark run definition, an XML file that defines the set of programs on which to test the analyzer and what resource limits to apply. Benchexec then runs the benchmarks while tracking resource usage. The results of all the benchmarks can then be rendered to an HTML table for viewing or exported to CSV for further processing.

#figure(
  caption: [Benchmark results rendered by Benchexec, green results are correct\
    (`true` for correct programs, `false(property-name)` for bug detections),\
    purple results signify interrupted tests, incorrect results are marked red.],
  image("benchexec.png"),
)

All benchmarks were run on an AMD EPYC 9124 processor with 1500 MB of available memory and three minutes of total run time per benchmark.

The amount of memory was chosen to be as small as possible, such that no tests would run out of memory and that the testing could be parallelized into eight processes on a VM with 8 CPU cores and 16 GB of available memory. This limit has a wide margin since the largest amount of allocated memory during a benchmark was under 600 MB. In the case of maximum test time, the period of three minutes was chosen as the smallest value such that no additional tests would timeout compared to a run with 15 minutes of maximum test time.

The evaluated version of the analyzer is marked with the Git tag `bp_version` in the tool's repository at #link("https://github.com/pepega007xd/ktsn", raw("https://github.com/pepega007xd/ktsn")).

In the SV-COMP dataset, one of the analyzed properties of programs is called `MemSafety`, which requires the following subproperties to hold:
- `valid-free` -- all memory deallocations are valid (no pointers other than those allocated with `malloc` or other allocation functions are passed to the `free` function).
- `valid-deref` -- all pointer dereferences are valid.
- `valid-memtrack` -- all memory allocations are tracked (no memory is leaked by losing a pointer to it or left allocated after the end of the `main` function).

The benchmarked programs all have an expected result, which can either be a violation of one of these properties, or `true`, signifying that all properties are valid for the program.

== Crafted Benchmarks

The crafted benchmarks verify that all basic parts of the analyzer work as intended. On the evaluated version of the analyzer, all of the crafted benchmarks are passing. Since these programs are purposefully written to test the analyzer, they do not contain any code that requires the external preprocessing described in @const_prop, and it is therefore not enabled for these tests. The test programs are located in #plugin_link("test_programs") and can be divided into these groups.

The first group contains programs that test a specific part of the analysis, which serve as quick checks of specific functionality during development. The only property being tested here is that the analysis completes successfully. This group includes:
- `all_list_types.c` -- list and field type recognition, type heuristic,
- `generic_structs.c` -- handling of structs not recognized as lists,
- `stack_pointers.c` -- handling of pointers to variables on the stack.

The second group tests the analysis of a correct program that allocates a list of nondeterministic size, traverses the list from start to end, and deallocates the list. These benchmarks allow us to compare the run time of an equivalent program for different list types (programs named `*_full.c`). These tests also verify that all basic parts of the analysis (abstraction, simplification) are working as expected. The second group contains:
- `ls_full.c`,
- `ls_full_return.c`,
- `ls_full_single_function.c`,
- `ls_cyclic.c`,
- `dls_full.c`,
- `nls_full.c`.

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

As you can see in the table above, the analysis of programs using doubly-linked lists and nested lists is roughly an order of magnitude longer than the analysis of singly-linked lists.

The third group tests the detection of different types of bugs. The only property being tested is that the errors are reported as the correct type of error, since, for example, the `valid-free` property is not violated in any of the SV-COMP benchmarks of the `LinkedLists` subset. This group includes:
- `ls_null_deref.c` -- detection of a possible null-pointer dereference (`valid-deref` property),
- `ls_use_after_free.c` -- detection of an access to a freed allocation (`valid-deref` property),
- `dls_double_free.c` -- detection of a double-free (`valid-free` property),
- `nls_memory_leak.c` -- detection of a memory leak (`valid-memtrack` property).

The last group tests some other interesting operations on singly-linked lists:

- `ls_merge_lists.c` -- program that joins one list on the end of another list,
- `reverse_list.c` -- program that reverses the order of a list,
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

As can be seen from the analysis times, the complexity of the program itself does not affect the run time of the analysis. Even though the calculator is a larger program than most of the other tested programs (147 lines of code), the analysis takes only a second. The complexity of the operations on the linked lists is what dictates the analysis time. In the case of the calculator, the only operations on the list are simple `push` and `pop` operations that do not require the traversal of the list.

== SV-COMP Benchmarks

SV-COMP is a yearly competition for verification tools composed of many categories, each verifying a different property of programs. One of these categories is `MemSafety`, in which analyzers verify the properties described previously. However, the `MemSafety` category includes many kinds of programs, such as those working with arrays, for which our analyzer cannot succeed. Therefore, we restricted the set of benchmarks only to the `LinkedLists` subset of this category.

Since many analyzers do not support data structures of unbounded size, the benchmarks contain a number of programs that work with lists of small, bounded size. To improve the precision of our analysis on these benchmarks, we unroll a fixed number of iterations on loops (see @const_prop).

When trying different settings of the loop unrolling, it became clear that some tests will only finish when run with a higher count of unrolled iterations, and other tests will only finish with a lower count. After some trial and error, we chose to run the analysis for one third of the maximum time with the unrolling level set to 3. If the analysis times out, it is run with the unrolling level of 2 for another third of the time, and for the last third, no unrolling is done at all. This yielded some improvements, but a better solution would be to implement a custom loop unrolling pass that would only unroll loops with a fixed number of iterations.

Of the 134 total benchmarks, this tool correctly analyzes 80 programs. The full results can be seen in the table below, compared to PredatorHP (the best-performing analyzer of SV-COMP 2025 in the `LinkedLists` subcategory) and EVA (static analyzer built into Frama-C).

#figure(
  caption: "The results of evaluating different analyzers on the SV-COMP dataset",
  table(
    columns: 4,
    table.header(
      [Tests (134 total)],
      [KTSN],
      [PredatorHP],
      [EVA],
    ),

    [Correct], [80], [124], [56],
    [Correct (true)], [74], [96], [50],
    [Correct (false)], [6], [28], [6],
    [Incorrect (true)], [0], [0], [6],
    [Incorrect (false)], [0], [0], [48],
    [Timeout], [11], [10], [4],
    [Unknown], [43], [0], [20],
  ),
)

In the table below, you can see that our analyzer outperforms all but two other tools in the `LinkedLists` subcategory. Note that a number of tools can only analyze programs without unbounded data structures, as they are not specialized for linked lists but focus on memory safety in general. Also note that the overall winner of SV-COMP 2025, ULTIMATE Atomizer, correctly analyzes only 12 programs of this subcategory.

#figure(
  caption: [The number of correct results in the `LinkedLists`\
    subcategory for different analyzers],
  table(
    columns: 2,
    table.header("Analyzer", "Correct Results"),
    [PredatorHP], [124],
    [CPAchecker], [118],
    [nacpa (CPAchecker)], [118],
    [*KTSN*], [*80*],
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
  ),
)

#pagebreak(weak: true)

The failing benchmarks can be divided into the following categories:

#table(
  columns: 2,
  table.header("Failure reason", "Number of\nbenchmarks"),
  [Unsupported language features], [25],
  [Timeout], [11],
  [Unknown result], [18],
)

First, there are benchmarks that use code from the Linux kernel. This includes all benchmarks from the `ddv-machzwd` group and some benchmarks from `memsafety-broom`. These fail immediately since the code contains uses of pointer arithmetic, which is unsupported. Some other tests outside of the kernel code also use unsupported language features, such as the allocation of structures directly on the stack or structure types nested in other structure types, and therefore fail at the preprocessing level.

In the group of benchmarks that timeout, we can find a few problems. Most of these programs use structs that will not be detected as the correct list type. This is the case in #svcomp_link("forester-heap/dll-01-2.c"), #svcomp_link("list-ext3-properties/sll_of_sll_nondet_append-1.c"), and others. In these programs, the abstraction will never be able to create list predicates because the structures in the heap will be different from what is expected, and the analysis will not terminate. Some programs implement lists that are simply not covered by the supported list types, for example #svcomp_link("heap-manipulation/tree-1.c"), which implements a binary tree. A few programs that timeout do not have a fundamental reason why the analysis cannot succeed, such as #svcomp_link("memsafety-broom/sll-nested-sll-inline.c"). These will require further optimizations of the analyzer or the SL solver to pass.

The rest of the failed benchmarks are those in which a property violation is detected, but because a nondeterministic condition has been reached during the analysis, an unknown result is reported. This problem is described in detail in @const_prop. However, constant propagation and loop unrolling cannot solve all of these cases, especially not those where relevant non-pointer data is stored in the list nodes themselves. This includes most red-black list benchmarks (such as #svcomp_link("forester-heap/dll-rb-cnstr_1-1.c")) or benchmarks where integer indices are used to manipulate list nodes (#svcomp_link("list-ext3-properties/sll_nondet_insert-2.c")).

The analysis of most correct results is generally fast, 49 of the 80 correct results are analyzed under a second, and 67 under 10 seconds. Because the total analysis time is split into thirds, in which the tool runs with different settings, you can see a group of results appear after one minute.

Compared to PredatorHP, which reaches 119 of its 124 correct results in under a second, the analysis is overall slower. However, CPAchecker does not produce any results in less than 7 seconds, so compared to this tool, our analysis times are overall better. Note that the results of PredatorHP and CPAchecker are taken directly from the SV-COMP results, which was run on a more powerful hardware than ours, and therefore the relative analysis times might be slightly different.

#figure(
  caption: [The number of correct results reached by\
    the analyzers with increasing analysis time (logarithmic axis)],
  image("svcomp_tools.svg"),
)

In the next table, we can see how the optimized formula deduplication described in @join_operation improves the analysis times. With the optimization turned off, the analysis times are consistently higher, but more importantly, some tests do not finish at all. Without the optimization, only 77 benchmarks produce correct results.

#figure(
  caption: [Comparison of a normal benchmark run\
    with a run where the deduplication optimization is disabled],
  image("svcomp_optimization.svg"),
)

== Evaluation of SL Queries

Our analyzer is able to track how much time each run spends in the Astral solver. The table below shows the three fastest and slowest SV-COMP benchmarks. Because the analyzer is killed during a timeout, it does not have a chance to print out the Astral time. Therefore, this table only lists benchmarks that were completed during the first minute.

#figure(
  caption: [Examples of the fastest and slowest benchmarks\
    and the time spent in the solver versus in the analyzer itself],
  table(
    columns: 5,
    table.header("Benchmark", [Total\ time (s)], [Astral\ time (s)], [Analyzer\ time (s)], [Number\ of queries]),
    [`sll2c_append_unequal.c`], [0.50], [0.00], [0.50], [2],
    [`sll_shallow_copy-1.c`], [0.51], [0.00], [0.51], [2],
    [`dll2c_prepend_equal.c`], [0.51], [0.02], [0.49], [4],
    table.cell(colspan: 5, $ dots.c $),
    [`dll-rb-cnstr_1-2.c`], [16.29], [15.64], [0.65], [141],
    [`sll-sorted-2.c`], [31.79], [31.65], [0.14], [1472],
    [`dll-simple-white-blue-2.c`], [48.61], [48.13], [0.48], [816],
  ),
)

As you can see from the table, the time spent in the analyzer itself is marginal compared to the total run time. This includes the initialization of the framework, preprocessing, and all work done during the dataflow analysis. Both the average and the median time spent in the analyzer are approximately 0.53 seconds. This clearly shows that to achieve faster analysis, it is pointless to optimize the code of the analyzer itself. The only thing that can meaningfully improve the run times is a better usage of the solver through simplifications of formulae.

#figure(
  caption: [
    The times of evaluating queries when analyzing\
    `sll-shared-sll-after.c` (logarithmic time axis)
  ],
  image("queries.svg"),
)

There is usually a small number of queries that take up most of the time spent in the solver. For example, during the analysis of #svcomp_link("memsafety-broom/sll-shared-sll-after.c"), the top eight slowest queries take 18.2 seconds of the total 19.9 seconds spent in the solver.

These queries are usually entailments, especially those with logic variables on the right side, as these must be existentially quantified. The longest query in this example (3.60 seconds) is the following entailment:

$
  nil = a * b != nil * A_b != nil * freed(b) * freed(A_b) \
  entl\
  a != nil * A_a != nil * freed(a) * freed(A_a) * f0 -> nil * ls(bound: 0, b, f0)
$

The other slow queries are similar to this one in that they are entailments with list predicates and logic variables on the right side.

= Conclusion <conclusion>

This thesis introduced the KTSN static analyzer focused on verifying the memory safety of C programs. The tool uses dataflow analysis with separation logic formulae as the value domain. The analyzer implements abstraction using inductive predicates, which allows it to analyze list structures of unbounded size.

The analyzer was tested on a set of crafted programs which show it can prove the correctness of programs that create unbounded lists of all supported types. These benchmarks also verify that the analyzer is able to correctly detect use-after-free errors, null-pointer dereferences, double-free errors, and memory leaks.

Further benchmarking was done on the `LinkedLists` subset of the SV-COMP benchmarks. Of the 134 total benchmarks, KTSN can correctly analyze 80 programs. From the 18 analyzers that competed in SV-COMP 2025, only two others achieve better results than ours.

The use of a dedicated SL solver means that our analysis will benefit from future improvements to the Astral solver and the SMT backends it uses, unlike other analyzers that use custom formalisms for which there are no dedicated solvers. Compared to other tools, another advantage of our SL-based method is that it can be easily extended to other inductive predicates when implemented by the solver.

The main drawback of our method, preventing us from analyzing more programs, is the lack of support for pointer arithmetic. Another disadvantage compared to other tools is the lack of analysis of numeric values, which makes the analysis of integer conditions impossible. Because of this, most programs containing bugs are reported as unknown results instead of detected errors, because the analyzer cannot guarantee that the bug is real, and not just a false-positive caused by an imprecise analysis of a condition.

The analysis time differs based on the type of list involved and the complexity of the operations done on the list. For programs that work with singly-linked lists, the analysis time is usually around a second, often under a second. For doubly-linked lists and nested lists, the analysis can take seconds to tens of seconds. Most of the analysis time is spent in the Astral solver deciding formulae, whereas the time spent in the analyzer itself is roughly independent of the analyzed program, around half a second. Most queries to the solver are evaluated in less than 100 milliseconds, but each analysis usually contains just a small number of queries that take up most of the total analysis time. These queries are typically entailments with inductive predicates and existentially quantified logic variables.

== Future Work

The most promising direction to improve the precision of the analysis is to add a numeric value analysis to the tool. One approach might be to implement a simple numeric value analysis manually. Because Astral decides SL formulae by translating them into SMT, adding SMT terms with integer variables into SL formulae is possible. Another, more interesting way to get a numeric value analysis would be to integrate our analysis method into the EVA analyzer. EVA would provide the values of numeric variables to our tool to better analyze integer conditions, and our analyzer would provide more precise information about pointer variables to EVA. Moreover, the tools are based on similar analysis methods, and both use the same framework, which would simplify the integration.

Another possible improvement is switching from static list type detection based on C types to dynamic detection during the analysis based on pointer structures that form in the heap. This would eliminate timeouts caused by not applying the correct abstraction to the formulae.

Other possible improvements include extending the generalization and joining of similar formulae beyond comparing singular atoms, or using the #freed predicate to signify unallocated memory locations. These improvements could help reduce the amount of formulae in each state during analysis and decrease the time needed for deciding SL formulae.

#pagebreak()

#bibliography("references.bib")
