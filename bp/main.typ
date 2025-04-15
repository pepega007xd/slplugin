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

    This thesis descibes the design and implementation of a verification tool for C programs, focusing on programs using linked lists. The idea is to derive the shape of the data structures that exist in the program's heap during execution. This is then used to prove correctness of all memory accesses in the program, and to detect possible invalid memory accesses. The approach is based on representing abstract program states using formulae of separation logic. It is implemented in the Frama-C framework, utilizing its dataflow analysis template, and value analysis for semantic constant propagation. The tool was benchmarked on the linked lists subset of SV-COMP benchmarks and compared with similar verification tools. While not reaching the performance of top competitors in this category, it is on par with most verifiers.
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

test: @astral

// no subchapters

// - problem of memory safety bugs in software
// - C/C++
// - source of vulnerabilities
// - possible solutions (testing/dyn analysis -> not a proof of correctness, static an., formal verification)
// - Linked lists
// - SL

= Preliminaries

== Formal Verification

== Separation Logic

== Dataflow Analysis

=== Astral Solver

= Current Approaches

// TODO: find out what exists
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

= Implementation Details

== Framework

// Mention Frama-C/Ivette only here, or earlier?
// Describe Cil AST -> or not? maybe just stmt/exp representation

== Preprocessing

// Describe problem with type analysis -> static from C types / dynamic from actual heap structures created by the program -> possible future improvement
// Describe problem with complex stmts (*a->b = c->d->e), splitting into instructions
// maybe formalize Instr_type into something like in original paper?
// mention splitting condition exp, func param
// collecting stack allocated vars?

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

#pagebreak()

#bibliography("references.bib")
