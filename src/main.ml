open Config
open Dataflow2
open Astral
open Common
module ForwardsAnalysis = Forwards (Analysis)

let run () =
  Preprocessing.preprocess ();

  Astral_query.init ();

  let main, _ = Globals.entry_point () in
  let first_stmt = Kernel_function.find_first_stmt main in

  Hashtbl.add !results first_stmt [ SSL.mk_emp () ];

  ForwardsAnalysis.compute [ first_stmt ];

  Solver.dump_stats !Astral_query.solver;
  Self.debug "Astral took %.2f seconds" !Astral_query.solver_time

let () = Boot.Main.extend (fun () -> if Enable_analysis.get () then run ())
