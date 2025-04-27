#import "@preview/peace-of-posters:0.5.5" as pop
#import "@preview/diagraph:0.3.3" as graph

#set page("a0", margin: 1cm)
#set text(font: "New Computer Modern", lang: "en")
#pop.set-poster-layout(pop.layout-a0)
#pop.set-theme(pop.uni-fr)
#set text(size: pop.layout-a0.at("body-size"))
#let box-spacing = 1.2em
#set columns(gutter: box-spacing)
#set block(spacing: box-spacing)
#pop.update-poster-layout(spacing: box-spacing)
#set line(stroke: gray)
#set figure(gap: 1.2em)

#text(
  font: "Noto Sans",
  fill: white,
  grid(
    align: (horizon + center),
    columns: (1fr, 1.5fr, 1fr),
    inset: 1.2em,
    fill: rgb("#1d154d"),
    image("excel_logo.svg", width: 100%),
    align(
      center,
      stack(
        spacing: 1em,
        text(size: 50pt, "Static Analysis of Heap-Manipulating Programs using Separation Logic"),
        text(size: 30pt, [Tomáš Brablec #h(2em) Supervisor: Ing. Tomáš Dacík]),
        text(size: 30pt, "Advisor: prof. Ing. Tomáš Vojnar Ph.D."),
      ),
    ),
    image("fit_logo.svg", width: 100%),
  ),
)

#columns(
  2,
  [
    #pop.column-box(heading: "Introduction")[
      *Memory safety* is a property of programs that guarantees all pointer operations to be valid. Since memory safety bugs, such as _use-after-free_ or _null pointer dereferences_, often lead to exploitable vulnerabilities, there is a need to verify the memory safety of programs.

      This work implements a *static analyzer* that verifies the memory safety of C programs. The analyzer is focused on *linked lists* and works by doing a dataflow analysis over the program's CFG. Formulae of *separation logic* (SL) are used to represent the abstract memory states in the program.

      #figure(
        caption: [Supported lists types -- singly linked list,\
          doubly linked list, nested list],
        align(
          horizon,
          stack(
            dir: ltr,
            spacing: 2em,
            graph.raw-render(```
            digraph LinkedList {
                rankdir=LR
                node [shape=square style="filled,rounded" fillcolor=lightblue label=""]

                node1 -> node2 -> node3 -> end

                end [style="filled,rounded" fillcolor=white]
            }
            ```),

            graph.raw-render(```
            digraph LinkedList {
                node [shape=square style="filled,rounded" fillcolor=lightblue label=""]

                { rank=same; end_l; node1; node2; node3; end_r }
                end_l -> node1 [dir="back"]
                node1 -> node2 -> node3 -> end_r
                node3 -> node2 -> node1

                end_l [style="filled,rounded" fillcolor=white]
                end_r [style="filled,rounded" fillcolor=white]
            }
            ```),

            graph.raw-render(```
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
            ```),
          ),
        ),
      )

      #line(length: 100%)

      #figure(
        caption: [Example of an SL formula and its model, the formula represents\
          a disjoint partition of the heap into a list segment and a pointer.
        ],
        align(
          horizon,
          stack(
            dir: ltr,
            spacing: 6em,
            $
              text(#blue, "ls"_(1+)(x,y)) * text(#red, y |-> z)
            $,
            graph.raw-render(```
            digraph NestedLinkedList {
                rankdir=LR
                node [shape=square fixedsize=true width=0.6]
                x,x_2 [style="filled,rounded" fillcolor=lightblue]
                x_2 [label=""]
                y [style="filled,rounded" fillcolor=pink]
                z [style="rounded"]

                x -> x_2 -> y [style="dashed"]
                y -> z
            }
            ```),
          ),
        ),
      )
    ]

    #pop.column-box(heading: "Analysis", stretch-to-next: true)[
      #figure(
        caption: [Control flow graph of the program in @analysis_example],
        graph.raw-render(
          ```
          digraph NestedLinkedList {
              rankdir=LR
              node [shape=circle]

              start -> 1 -> 2 -> 3 -> 4 -> 5 -> 3
              3 -> 7

              start [shape=none label=""]
          }
          ```,
          width: 50%,
        ),
      )

      #let line_no = counter("line_no")
      #let c(code) = {
        line_no.step()
        context (
          text(fill: rgb("#808080"), raw(line_no.display())),
          h(0.8em),
          raw(lang: "c", code),
        ).join()
      }

      #let pass2(m) = text(red, m)
      #let pass3(m) = text(blue, m)

      #line(length: 100%)

      #figure(
        caption: [
          Simplified progress of an analysis run,\
          ($f'_(n)$ represents an existentially quantified variable)
          #grid(
            align: (horizon, left),
            row-gutter: 0.5em,
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
            "Code", "state formulae",
            line(length: 50%), line(length: 50%),
            c("Node *x = malloc(size);"), [],
            [], $x |-> f'_1$,
            c("Node *y = x;"), [],
            [], $x |-> f'_1 * y = x$,
            c("while (rand()) {"), [],
            [], $x |-> f'_1 * y = x$,
            [], pass2($x |-> y * y |-> f'_2$),
            [], pass3($"ls"_(2+)(x, y) * y |-> f'_3$),
            c("  y->next = malloc(size);"), [],
            [], $x |-> f'_1 * f'_1 |-> f'_2 * y = x$,
            [], pass2($x |-> y * y |-> f'_2 * f'_2 |-> f'_3$),
            [], pass3($"ls"_(2+)(x, y) * y |-> f'_3 * f'_3 |-> f'_4$),
            c("  y = y->next;"), [],
            [], $x |-> y * y |-> f'_2$,
            [], pass2($x |-> f'_4 * f'_4 |-> y * y |-> f'_3$),
            [], pass3($"ls"_(2+)(x, f'_5) * f'_5 |-> y * y |-> f'_4$),
            c("}"), [],
            c("y->next = NULL;"), [],
            [], pass3($"ls"_(0+)(x, y) * y |-> bot$),
          ),
        ),
      ) <analysis_example>

      #line(length: 100%)

      #figure(
        caption: [Entailments are verified by the Astral solver,\
          allowing the analysis to find fixpoints for loops],
        align(
          center,
          grid(
            columns: 3,
            align: horizon,
            gutter: 2em,
            $
              or.big
              vec(delim: "[",
                x |-> f'_1 * y = x,
                pass2(x |-> y * y |-> f'_2),
                pass3("ls"_(2+)(x, y) * y |-> f'_3),
              )
            $,
            $ tack.double $,
            pass3($ exists f'_3. thick "ls"_(0+)(x, y) * y |-> f'_3 $),
          ),
        ),
      )
    ]

    #colbreak()

    #pop.column-box(heading: "Technologies")[
      #align(
        center,
        grid(
          columns: 3,
          align: horizon + center,
          gutter: 2em,
          image("ocaml_logo.svg", width: 6em),
          image("astral_logo.svg", width: 5em),
          image("frama_c_logo.svg", width: 14em),
        ),
      )

      - OCaml -- the analyzer is implemented using functional programming
      - *Astral solver* [1] -- solver for separation logic, used for evaluating entailments of SL formulae in the join operation
      - *Frama-C framework* [2] -- provides a library for dataflow analysis, AST representation of the analyzed program, utilities for AST manipulation, and a GUI (Ivette) for interactive analysis
    ]

    #pop.column-box(heading: "Capabilities")[
      - Support for both linear and circular list variants
      - Analysis across function calls with *function summaries*
      - Pointers to variables on the stack are supported
      - Some integer bounds on loops are supported through loop unrolling
    ]

    #pop.column-box(heading: "Results")[
      The analyzer was tested on the SV-COMP benchmarks, specifically on the Linked Lists subset. From the total of *134 tests, 77 are passing*. Compared to the 2024 winner in this category, PredatorHP, this analyzer cannot analyze pointer arithmetic.

      #show table.cell.where(y: 0): strong
      #align(
        center,
        table(
          columns: 4,
          gutter: 0.5em,
          stroke: (x, y) => if y == 0 {
            (bottom: 0.7pt + black)
          },
          align: (x, y) => (
            if x > 0 { center } else { left }
          ),
          table.header(
            [Tests (134 total)],
            [This work],
            [PredatorHP],
            [EVA],
          ),

          [Correct], [77], [124], [80],
          [Correct (true)], [73], [96], [76],
          [Correct (false)], [4], [28], [4],
          [Incorrect], [0], [0], [40],
        ),
      )

      However, this tool correctly analyzes 15 tests, for which the EVA analyzer built into Frama-C produces wrong results, and unlike EVA, it can find memory leaks. It could, therefore, be worth it to *integrate our analysis method into EVA* itself, improving the precision of both tools by exchanging information during analysis.

      Manual testing shows that some simple *practical programs can be verified* with the current version of the tool, such as a postfix expression calculator that uses a linked list implementation of a stack.

      #figure(
        caption: [Ivette showing the analysis results],
        gap: 0.6em,
        image("ivette_screenshot.png"),
      )
    ]

    #pop.column-box(heading: "Future Work", stretch-to-next: true)[
      - Integrate with analyzers for other domains (e.g. EVA)
      - Detect types of lists dynamically from structures in the heap
      - Improve the analysis of non-pointer conditions to improve bug detection
      - Implement the remaining C language features (such as global variables)
    ]
  ],
)

#pop.bottom-box(
  text(
    size: 25pt,
    [
      [1] T. Dacík et al., “Deciding Boolean Separation Logic via Small Models,” in _TACAS_, 2024.
      #h(4em)
      [2] J. Signoles et al., “Frama-C: a Software Analysis Perspective,” in _Formal Aspects of Computing_, 2015.
    ],
  ),
)
