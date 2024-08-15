open Astral
open Common

(** Astral solver instance, created in `slplugin.ml` *)
let solver = ref (Solver.init ())

(** time spent in Astral *)
let solver_time = ref 0.0

let check_sat (formula : SSL.t) : bool =
  let start = Sys.time () in
  let result = Solver.check_sat !solver formula in
  solver_time := !solver_time +. Sys.time () -. start;
  result

let check_entailment (lhs : SSL.t) (rhs : SSL.t) : bool =
  let vars = SSL.get_vars rhs in
  let fresh_vars =
    List.filter is_fresh_var vars |> List.sort_uniq SSL.Variable.compare
  in
  let fresh_vars = List.map (fun var -> SSL.Var var) fresh_vars in
  let quantified_rhs = SSL.mk_exists fresh_vars rhs in
  let testsolver = Solver.init () in
  let start = Sys.time () in
  let result = Solver.check_entl testsolver lhs quantified_rhs in
  solver_time := !solver_time +. Sys.time () -. start;
  result

let check_inequality (lhs : SSL.Variable.t) (rhs : SSL.Variable.t)
    (formula : SSL.t) : bool =
  formula |> Formula.add_eq lhs rhs |> check_sat |> not
