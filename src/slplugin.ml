open Config
open Dataflow2
open Astral
open Analysis
open Common
module ForwardsAnalysis = Forwards (Analysis)

let run () =
  Preprocessing.preprocess ();

  let dump_queries =
    if Dump_queries.get () then `Full "astral_queries" else `None
  in
  let backend = if Use_cvc5.get () then `CVC5 else `Z3 in
  Common.solver := Solver.init ~dump_queries ~backend ();

  let main, _ = Globals.entry_point () in
  let first_stmt = Kernel_function.find_first_stmt main in

  (* print control flow automaton to `cfa.dot` *)
  let automaton = Interpreted_automata.get_automaton main in
  let file = Out_channel.open_text "cfa.dot" in
  Interpreted_automata.output_to_dot ~labeling:`Stmt
    ~wto:(Interpreted_automata.get_wto main)
    file automaton;

  Hashtbl.add !results first_stmt [ SSL.mk_emp () ];

  ForwardsAnalysis.compute [ first_stmt ];
  Printing.print_result !results;
  Solver.dump_stats !Common.solver;
  Self.debug "Astral took %.2f seconds" !Common.solver_time

let () = Boot.Main.extend (fun () -> if Enable_analysis.get () then run ())
