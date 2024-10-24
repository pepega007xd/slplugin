open Astral
open Common

(** Astral solver instance, created in [slplugin.ml] *)
let solver = ref (Solver.init ())

(** time spent in Astral *)
let solver_time = ref 0.0

let init () =
  let dump_queries =
    if Config.Dump_queries.get () then `Full "astral_queries" else `None
  in
  let backend = if Config.Use_cvc5.get () then `CVC5 else `Z3 in
  let encoding = if Config.Use_Bitvectors.get () then `Bitvectors else `Sets in
  solver := Solver.init ~dump_queries ~backend ~encoding ()

let check_sat (formula : Formula.t) : bool =
  let start = Sys.time () in
  let result = Solver.check_sat !solver (Formula.to_astral formula) in
  solver_time := !solver_time +. Sys.time () -. start;
  result

let check_entailment (lhs : SSL.t) (rhs : SSL.t) : bool =
  let vars = SSL.get_vars rhs in
  let fresh_vars =
    List.filter is_fresh_var vars |> List.sort_uniq SSL.Variable.compare
  in
  let fresh_vars = List.map (fun var -> SSL.Var var) fresh_vars in
  let quantified_rhs = SSL.mk_exists fresh_vars rhs in

  let start = Sys.time () in
  let result = Solver.check_entl !solver lhs quantified_rhs in
  solver_time := !solver_time +. Sys.time () -. start;
  result

let check_inequality (lhs : SSL.Variable.t) (rhs : SSL.Variable.t)
    (formula : Formula.t) : bool =
  formula |> Formula.add_eq lhs rhs |> check_sat |> not
