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
  solver := Solver.init ~dump_queries ();

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

  Printing.print_result !results

let () = Db.Main.extend run
