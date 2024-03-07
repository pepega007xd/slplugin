open Dataflow2
open Astral
open Analysis
module ForwardsAnalysis = Forwards (Analysis)

let run () =
  Preprocessing.preprocess ();

  if Slplugin_options.Debug_output.get () then (
    Printing.print_warn "Analyzing this code:";
    Frama_c_kernel.Printer.pp_file Format.std_formatter (Ast.get ()));

  let dump_queries =
    if Slplugin_options.Dump_queries.get () then `Full "astral_queries"
    else `None
  in
  let astral_backend =
    if Slplugin_options.Use_cvc5.get () then `CVC5 else `Z3
  in
  Common.solver := Solver.init ~dump_queries ~backend:astral_backend ();

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
  if Slplugin_options.Debug_output.get () then
    Format.printf "Astral took %.2f seconds\n" !Common.solver_time

let () = Db.Main.extend run
