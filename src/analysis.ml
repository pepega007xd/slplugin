open Config
open Astral
open Cil_types
open Common
open Dataflow2

let name = "slplugin"
let debug = true (* TODO: what does this do??? *)

type t = Formula.state

let copy state = state
let pretty fmt state = Formula.pp_state fmt state
let var = Preprocessing.varinfo_to_var

(** this is the transfer function for instructions, we take the instr and previous state, and create new state *)
let doInstr _ (instr : instr) (prev_state : t) : t =
  (* this allows Ivette to load the current state of analysis (messages, AST properties, etc) *)
  Async.yield ();

  let new_state =
    match Preprocessing.get_instr_type instr with
    | Preprocessing.Assign_simple (lhs, rhs) ->
        prev_state |> List.map (Transfer.assign (var lhs) (var rhs))
    | Preprocessing.Assign_rhs_field (lhs, rhs, rhs_field) ->
        prev_state
        |> List.concat_map (Formula.materialize (var rhs))
        |> List.map
             (Transfer.assign_rhs_field (var lhs) (var rhs)
                (Preprocessing.get_field_type rhs_field))
    | Preprocessing.Assign_lhs_field (lhs, lhs_field, rhs) ->
        prev_state
        |> List.concat_map (Formula.materialize (var lhs))
        |> List.map
             (Transfer.assign_lhs_field (var lhs)
                (Preprocessing.get_field_type lhs_field)
                (var rhs))
    | Preprocessing.Call (lhs_opt, func, params) ->
        prev_state
        |> List.concat_map
             (Transfer.call (Option.map var lhs_opt) func (List.map var params))
    | Preprocessing.ComplexInstr -> fail "unreachable analysis.ml:46"
    | Preprocessing.Ignored -> prev_state
  in
  Self.debug ~current:true ~dkey:Printing.do_instr
    "previous state:\n%anew state:\n%a" Formula.pp_state prev_state
    Formula.pp_state new_state;
  new_state

(* [state] comes from doInstr, so it is actually the new state *)
let computeFirstPredecessor _ state = state

let deduplicate_states (state : t) : t =
  let rec select_to_keep (to_keep : t) (to_check : t) : t =
    match to_check with
    | [] -> to_keep
    | [ current ] when to_keep = [] -> [ current ]
    | current :: rest ->
        if
          Astral_query.check_entailment
            (Formula.to_astral current)
            (Formula.state_to_astral (to_keep @ rest))
        then
          (* [current] is already contained in the other formulas *)
          select_to_keep to_keep rest
        else
          (* [current] has unique models, add it to [to_keep] *)
          select_to_keep (current :: to_keep) rest
  in
  select_to_keep [] state

(* iterate over all formulas of new_state [phi], and each one that doesn't satisfy
   (phi => old) has to be added to [old]. If old is not changed, None is returned. *)
let combinePredecessors _ ~old:(old_state : t) (new_state : t) : t option =
  let joined_state = deduplicate_states @@ old_state @ new_state in

  if
    Astral_query.check_entailment
      (Formula.state_to_astral joined_state)
      (Formula.state_to_astral old_state)
  then (
    Self.debug ~current:true ~dkey:Printing.combine_predecessors
      "old state:\n%anew state:\n%a<state did not change>" Formula.pp_state
      old_state Formula.pp_state new_state;
    None)
  else (
    Self.debug ~current:true ~dkey:Printing.combine_predecessors
      "old state:\n%anew state:\n%acombined state:\n%a" Formula.pp_state
      old_state Formula.pp_state new_state Formula.pp_state joined_state;
    Some joined_state)

(* we need to filter the formulas of [state] for each branch to only those,
   which are satisfiable in each of the branches *)
let doGuard _ (condition : exp) (state : t) : t guardaction * t guardaction =
  let th, el =
    match condition.enode with
    | BinOp
        ( operator,
          { enode = Lval (Var lhs, NoOffset); _ },
          { enode = Lval (Var rhs, NoOffset); _ },
          _ ) -> (
        let lhs = var lhs in
        let rhs = var rhs in
        match operator with
        | Eq ->
            ( List.map (Formula.add_eq lhs rhs) state,
              List.map (Formula.add_distinct lhs rhs) state )
        | Ne ->
            ( List.map (Formula.add_distinct lhs rhs) state,
              List.map (Formula.add_eq lhs rhs) state )
        | _ -> (state, state))
    | _ -> (state, state)
  in
  let th = List.filter Astral_query.check_sat th in
  let el = List.filter Astral_query.check_sat el in

  Self.debug ~current:true ~dkey:Printing.do_guard
    "state:\n%athen branch:\n%aelse branch:\n%a" Formula.pp_state state
    Formula.pp_state th Formula.pp_state el;

  let th = if List.is_empty th then GUnreachable else GUse th in
  let el = if List.is_empty el then GUnreachable else GUse el in
  (th, el)

(* we always want to continue with the analysis *)
let doStmt (_ : stmt) (_ : t) : t stmtaction = SDefault

(* simplify formulas and filter out unsatisfiable ones *)
let doEdge (prev_stmt : stmt) (next_stmt : stmt) (state : t) : t =
  let end_of_scope_locals =
    Kernel_function.blocks_closed_by_edge prev_stmt next_stmt
    |> List.concat_map (fun block -> block.blocals)
    |> List.map Preprocessing.varinfo_to_var
  in

  let do_abstraction (state : t) : t =
    state
    |> List.map Abstraction.convert_to_ls
    |> List.map Abstraction.convert_to_dls
    |> List.map Abstraction.convert_to_nls
  in

  let do_abstraction : t -> t =
    match next_stmt.skind with
    | _ when Config.Edge_abstraction.get () -> do_abstraction
    | Loop _ -> do_abstraction
    | _ -> Fun.id
  in

  let deduplicate_states : t -> t =
    if Config.Edge_deduplication.get () then deduplicate_states else Fun.id
  in

  let open Simplification in
  let modified =
    state
    |> List.filter Astral_query.check_sat
    |> List.map (convert_vars_to_fresh end_of_scope_locals)
    |> List.map remove_leaks
    |> List.map reduce_equiv_classes
    |> do_abstraction
    |> List.map remove_distinct_only
    |> List.map remove_single_eq
    |> List.map (Formula.remove_spatial_from Formula.nil)
    (* deduplicate atoms syntactically *)
    |> List.map (List.sort_uniq compare)
    (* deduplicate formulas syntactically *)
    |> List.sort_uniq compare
    (* deduplicate formulas semantically *)
    |> deduplicate_states
  in

  Self.debug ~current:true ~dkey:Printing.do_edge
    "original state:\n%anew state:\n%a" Formula.pp_state state Formula.pp_state
    modified;
  modified

module StmtStartData = struct
  type data = t

  let results = Func_call.results
  let clear () = Hashtbl.clear !results

  (* we cannot just assign `let mem = Hashtbl.mem !results`,
     that would evaluate `!results` immediately, but we need it to be evaluated each time
     (inside function calls, `results` refers to a different hashtable *)
  let mem stmt = Hashtbl.mem !results stmt
  let find stmt = Hashtbl.find !results stmt
  let replace stmt = Hashtbl.replace !results stmt
  let add stmt = Hashtbl.add !results stmt
  let iter f = Hashtbl.iter f !results
  let length () = Hashtbl.length !results
end

module Tests = struct
  open Testing

  let%test "atom_deduplication" =
    let input = [ Distinct (x, y); Distinct (x, y); Distinct (x, y) ] in
    let expected = [ Distinct (x, y) ] in
    assert_eq (List.sort_uniq compare input) expected

  let changed joined_state old_state =
    not
    @@ Astral_query.check_entailment (SSL.mk_or joined_state)
         (SSL.mk_or old_state)

  (* let%test_unit "join" = *)
  (*   let old_state = [ SSL.mk_star [ SSL.mk_pto x y' ] ] in *)
  (*   let new_state = [ SSL.mk_star [ SSL.mk_pto x z' ] ] in *)
  (*   let joined = deduplicate_states @@ old_state @ new_state in *)
  (*   assert_eq_list old_state joined *)
  (**)
  (* let%test_unit "join" = *)
  (*   let old_state = [ SSL.mk_star [ SSL.mk_pto x y ] ] in *)
  (*   let new_state = [ SSL.mk_star [ SSL.mk_pto x z ] ] in *)
  (*   let joined = deduplicate_states @@ old_state @ new_state in *)
  (*   assert_eq_list (old_state @ new_state) joined *)
  (**)
  (* let%test_unit "join_ls" = *)
  (*   let old_state = [ SSL.mk_star [ SSL.mk_pto x y ] ] in *)
  (*   let new_state = [ SSL.mk_star [ SSL.mk_ls x y; SSL.mk_distinct x y ] ] in *)
  (*   let joined = deduplicate_states @@ old_state @ new_state in *)
  (*   assert_eq_list (old_state @ new_state) joined; *)
  (*   assert (changed joined old_state) *)
  (**)
  (* let%test_unit "entailment" = *)
  (*   let old_state = SSL.mk_star [ SSL.mk_ls x y; SSL.mk_distinct x y ] in *)
  (*   let new_state = SSL.mk_star [ SSL.mk_pto x' y'; SSL.mk_pto y' z' ] in *)
  (*   assert (not @@ Astral_query.check_entailment old_state new_state) *)
  (**)
  (* let%test_unit "entailment" = *)
  (*   let start = mk_var "start" in *)
  (*   let temp = mk_var "temp" in *)
  (*   let alloc0 = mk_var "alloc!0" in *)
  (*   let alloc1 = mk_var "alloc!1" in *)
  (*   let alloc3 = mk_var "alloc!3" in *)
  (*   let nullptr = mk_var "nullptr" in *)
  (**)
  (*   let old_state = *)
  (*     SSL.mk_star *)
  (*       [ *)
  (*         SSL.mk_eq nullptr nil; *)
  (*         SSL.mk_pto temp alloc3; *)
  (*         SSL.mk_eq x temp; *)
  (*         SSL.mk_ls start temp; *)
  (*         SSL.mk_distinct start temp; *)
  (*       ] *)
  (*   in *)
  (*   let new_state = *)
  (*     SSL.mk_or *)
  (*       [ *)
  (*         SSL.mk_star *)
  (*           [ *)
  (*             SSL.mk_eq start x; *)
  (*             SSL.mk_pto x alloc0; *)
  (*             SSL.mk_eq nullptr nil; *)
  (*             SSL.mk_eq temp nil; *)
  (*           ]; *)
  (*         SSL.mk_star *)
  (*           [ *)
  (*             SSL.mk_pto start temp; *)
  (*             SSL.mk_eq nullptr nil; *)
  (*             SSL.mk_pto temp alloc1; *)
  (*             SSL.mk_eq x temp; *)
  (*           ]; *)
  (*       ] *)
  (*   in *)
  (*   assert (not @@ Astral_query.check_entailment old_state new_state) *)
end
