open Config
open Dataflow2
open Astral
module ForwardsAnalysis = Forwards (Analysis)

let run_analysis () =
  Transfer.compute_function := Some ForwardsAnalysis.compute;
  Preprocessing.preprocess ();

  Astral_query.init ();

  let main, _ = Globals.entry_point () in
  let first_stmt = Kernel_function.find_first_stmt main in

  Hashtbl.add !Transfer.results first_stmt [ [] ];

  ForwardsAnalysis.compute [ first_stmt ];

  Solver.dump_stats !Astral_query.solver;
  Self.debug "Astral took %.2f seconds" !Astral_query.solver_time

let run_with_stacktrace_printing () =
  Printexc.record_backtrace true;
  try run_analysis ()
  with e ->
    let backtrace = Printexc.get_backtrace () in
    Self.warning "EXCEPTION: %s" (Printexc.to_string e);
    Self.warning "BACKTRACE: \n%s" backtrace

let () =
  Boot.Main.extend (fun () ->
      if Enable_analysis.get () then run_with_stacktrace_printing ())
