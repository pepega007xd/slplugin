open Config
open Astral
open Cil_types
open Printing
open Common
open Dataflow2

let name = "slplugin"
let debug = true (* TODO: what does this do??? *)

type t = state

let copy state = state
let pretty fmt state = List.iter (SSL.pp fmt) state

let var (varinfo : Cil_types.varinfo) : SSL.Variable.t =
  SSL.Variable.mk varinfo.vname Sort.loc_ls

(* *this* is the transfer function for instructions, we take the instr and previous
   state, and create new state *)
let doInstr _ (instr : instr) (prev_state : state) : state =
  let new_state =
    match Preprocessing.get_instr_type instr with
    | Preprocessing.Assign_simple (lhs, rhs) ->
        prev_state |> List.map (Transfer.assign (var lhs) (var rhs))
    | Preprocessing.Assign_rhs_field (lhs, rhs, rhs_field) ->
        prev_state
        |> List.concat_map (Equiv_class.materialize (var rhs))
        |> List.map
             (Transfer.assign_rhs_field (var lhs) (var rhs)
                (Preprocessing.get_field_type rhs_field))
    | Preprocessing.Assign_lhs_field (lhs, lhs_field, rhs) ->
        prev_state
        |> List.concat_map (Equiv_class.materialize (var lhs))
        |> List.map
             (Transfer.assign_lhs_field (var lhs)
                (Preprocessing.get_field_type lhs_field)
                (var rhs))
    | Preprocessing.Call (lhs_opt, func, params) ->
        prev_state
        |> List.concat_map
             (Transfer.call (Option.map var lhs_opt) func (List.map var params))
    | Preprocessing.ComplexInstr -> fail "unreachable"
    | Preprocessing.Ignored -> prev_state
  in
  Self.debug ~current:true ~dkey:Printing.do_instr
    "previous state: %a\nnew state: %a" Printing.pp_state prev_state
    Printing.pp_state new_state;
  new_state

(* `state` comes from doInstr, so it is actually the new state *)
let computeFirstPredecessor _ state = state

let entailment (lhs : SSL.t) (rhs : SSL.t) : bool =
  let vars = extract_vars @@ get_atoms rhs in
  let fresh_vars = List.filter is_fresh_var vars |> list_deduplicate in
  let fresh_vars = List.map (fun var -> SSL.Var var) fresh_vars in
  let quantified_rhs = SSL.mk_exists fresh_vars rhs in
  let testsolver = Solver.init () in
  let start = Sys.time () in
  let result = Solver.check_entl testsolver lhs quantified_rhs in
  Common.solver_time := !Common.solver_time +. Sys.time () -. start;
  result

let deduplicate_states (state : state) : state =
  let rec select_to_keep (to_keep : state) (to_check : state) : state =
    match to_check with
    | [] -> to_keep
    | [ current ] when to_keep = [] -> [ current ]
    | current :: rest ->
        if entailment current (SSL.mk_or (to_keep @ rest)) then
          (* `current` is already contained in the other formulas *)
          select_to_keep to_keep rest
        else
          (* `current` has unique models, add it to `to_keep` *)
          select_to_keep (current :: to_keep) rest
  in
  select_to_keep [] state

(* iterate over all formulas of new_state `phi`, and each one that doesn't satisfy
   (phi => old) has to be added to `old`. If old is not changed, None is returned. *)
let combinePredecessors _ ~old:(old_state : state) (new_state : state) :
    state option =
  let joined_state = deduplicate_states @@ old_state @ new_state in

  if entailment (SSL.mk_or joined_state) (SSL.mk_or old_state) then (
    Self.debug ~current:true ~dkey:Printing.combine_predecessors
      "old state: %a\nnew state: %a\n<state did not change>" Printing.pp_state
      old_state Printing.pp_state new_state;
    None)
  else (
    Self.debug ~current:true ~dkey:Printing.combine_predecessors
      "old state: %a\nnew state: %a\ncombined state: %a" Printing.pp_state
      old_state Printing.pp_state new_state Printing.pp_state joined_state;
    Some joined_state)

(* we need to filter the formulas of `state` for each branch to only those,
   which are satisfiable in each of the branches *)
let doGuard _ (exp : exp) (state : state) :
    state guardaction * state guardaction =
  let add_eq lhs rhs state =
    List.map
      (fun formula -> SSL.mk_star [ formula; SSL.mk_eq (Var lhs) (Var rhs) ])
      state
  in
  let add_ne lhs rhs state =
    List.map
      (fun formula ->
        SSL.mk_star [ formula; SSL.mk_distinct (Var lhs) (Var rhs) ])
      state
  in

  let th, el =
    match exp.enode with
    | BinOp (Eq, lhs, rhs, _) -> (
        (* if (a == b) {...} *)
        match (lhs.enode, rhs.enode) with
        | Lval (Var lhs, NoOffset), Lval (Var rhs, NoOffset) ->
            let lhs = var lhs in
            let rhs = var rhs in
            (add_eq lhs rhs state, add_ne lhs rhs state)
        | _ -> (state, state))
    | BinOp (Ne, lhs, rhs, _) -> (
        (* if (a != b) {...} *)
        match (lhs.enode, rhs.enode) with
        | Lval (Var lhs, NoOffset), Lval (Var rhs, NoOffset) ->
            let lhs = var lhs in
            let rhs = var rhs in
            (add_ne lhs rhs state, add_eq lhs rhs state)
        | _ -> (state, state))
    (* all other conditions don't filter out any states *)
    | _ -> (state, state)
  in
  let th = List.filter check_sat th in
  let el = List.filter check_sat el in

  Self.debug ~current:true ~dkey:Printing.do_guard
    "state: %a\nthen branch: %a\nelse branch: %a" Printing.pp_state state
    Printing.pp_state th Printing.pp_state el;

  let th = if List.is_empty th then GUnreachable else GUse th in
  let el = if List.is_empty el then GUnreachable else GUse el in
  (th, el)

(* we always want to continue with analysis *)
let doStmt (_ : stmt) (_ : state) : state stmtaction = SDefault

(* simplify formulas and filter out unsatisfiable ones *)
let doEdge (prev_stmt : stmt) (next_stmt : stmt) (state : state) : state =
  let prev_locals = Hashtbl.find !local_vars_for_stmt prev_stmt in
  let new_locals = Hashtbl.find !local_vars_for_stmt next_stmt in
  let end_of_scope_locals =
    StringSet.diff prev_locals new_locals |> StringSet.to_list
  in

  let open Simplification in
  let modified =
    List.map Simplifier.simplify state
    |> List.filter check_sat
    |> List.map (convert_vars_to_fresh end_of_scope_locals)
    |> List.map remove_junk |> List.map remove_nil_vars
    |> List.map reduce_equiv_classes
    |> List.map convert_to_ls
    |> List.map remove_distinct_only
    |> deduplicate_states
  in

  Self.debug ~current:true ~dkey:Printing.do_edge
    "original state: %a\nnew state: %a" Printing.pp_state state
    Printing.pp_state modified;
  modified

module StmtStartData = struct
  type data = state

  let clear () = Hashtbl.clear !results
  let mem = Hashtbl.mem !results
  let find = Hashtbl.find !results
  let replace = Hashtbl.replace !results
  let add = Hashtbl.add !results
  let iter f = Hashtbl.iter f !results
  let length () = Hashtbl.length !results
end

module Tests = struct
  open Testing

  let changed joined_state old_state =
    not @@ entailment (SSL.mk_or joined_state) (SSL.mk_or old_state)

  let%test_unit "join" =
    let old_state = [ SSL.mk_star [ SSL.mk_pto x y' ] ] in
    let new_state = [ SSL.mk_star [ SSL.mk_pto x z' ] ] in
    let joined = deduplicate_states @@ old_state @ new_state in
    assert_eq_list old_state joined

  let%test_unit "join" =
    let old_state = [ SSL.mk_star [ SSL.mk_pto x y ] ] in
    let new_state = [ SSL.mk_star [ SSL.mk_pto x z ] ] in
    let joined = deduplicate_states @@ old_state @ new_state in
    assert_eq_list (old_state @ new_state) joined

  let%test_unit "join_ls" =
    let old_state = [ SSL.mk_star [ SSL.mk_pto x y ] ] in
    let new_state = [ SSL.mk_star [ SSL.mk_ls x y; SSL.mk_distinct x y ] ] in
    let joined = deduplicate_states @@ old_state @ new_state in
    assert_eq_list (old_state @ new_state) joined;
    assert (changed joined old_state)

  let%test_unit "entailment" =
    let old_state = SSL.mk_star [ SSL.mk_ls x y; SSL.mk_distinct x y ] in
    let new_state = SSL.mk_star [ SSL.mk_pto x' y'; SSL.mk_pto y' z' ] in
    assert (not @@ entailment old_state new_state)

  let%test_unit "entailment" =
    let start = mk_var "start" in
    let temp = mk_var "temp" in
    let alloc0 = mk_var "alloc!0" in
    let alloc1 = mk_var "alloc!1" in
    let alloc3 = mk_var "alloc!3" in
    let nullptr = mk_var "nullptr" in

    let old_state =
      SSL.mk_star
        [
          SSL.mk_eq nullptr nil;
          SSL.mk_pto temp alloc3;
          SSL.mk_eq x temp;
          SSL.mk_ls start temp;
          SSL.mk_distinct start temp;
        ]
    in
    let new_state =
      SSL.mk_or
        [
          SSL.mk_star
            [
              SSL.mk_eq start x;
              SSL.mk_pto x alloc0;
              SSL.mk_eq nullptr nil;
              SSL.mk_eq temp nil;
            ];
          SSL.mk_star
            [
              SSL.mk_pto start temp;
              SSL.mk_eq nullptr nil;
              SSL.mk_pto temp alloc1;
              SSL.mk_eq x temp;
            ];
        ]
    in
    assert (not @@ entailment old_state new_state)
end
