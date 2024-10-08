open Config
open Dataflow2
open Astral
module ForwardsAnalysis = Forwards (Analysis)

let run_analysis () =
  Analysis.compute_function := Some ForwardsAnalysis.compute;
  Preprocessing.preprocess ();

  Astral_query.init ();

  let main, _ = Globals.entry_point () in
  let first_stmt = Kernel_function.find_first_stmt main in

  Hashtbl.add !Analysis.results first_stmt [ [] ];

  ForwardsAnalysis.compute [ first_stmt ];

  Solver.dump_stats !Astral_query.solver;
  Self.debug "Astral took %.2f seconds" !Astral_query.solver_time

let run_with_stacktrace_printing () =
  Printexc.record_backtrace true;
  try run_analysis ()
  with _ ->
    let stacktrace = Printexc.get_backtrace () in
    Self.warning "Stacktrace: \n%s" stacktrace

let () =
  Boot.Main.extend (fun () ->
      if Enable_analysis.get () then run_with_stacktrace_printing ())
