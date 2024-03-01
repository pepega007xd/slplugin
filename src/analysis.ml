open Astral
open Cil_types
open Slplugin_options
open Printing
open Common
open Dataflow2

let results : (stmt, SSL.t list) Hashtbl.t ref = ref (Hashtbl.create 1024)
let name = "slplugin"
let debug = false

type t = SSL.t list

let copy state = state
let pretty fmt state = List.iter (SSL.pp fmt) state

let var_of_varinfo (varinfo : varinfo) : SSL.Variable.t =
  SSL.Variable.mk varinfo.vname Sort.loc_ls

let var_of_exp (exp : exp) : SSL.Variable.t =
  match exp.enode with
  | Lval (Var varinfo, NoOffset) -> var_of_varinfo varinfo
  | Const _ -> SSL.Variable.nil
  | _ -> fail "expression is not Lval or Const"

(* *this* is the transfer function for instructions, we take the instr and previous
   state, and create new state *)
let doInstr (stmt : stmt) (instr : instr) (prev_state : SSL.t list) : SSL.t list
    =
  if Debug_output.get () then (
    print_warn "doInstr";
    print_stmt stmt;
    print_endline "previous state:";
    print_state prev_state);

  let new_state =
    match instr with
    | Local_init (lhs, rhs, _) -> (
        (* int *a = malloc(...) *)
        let lhs = var_of_varinfo lhs in
        match rhs with
        | AssignInit (SingleInit rhs) ->
            List.map (Transfer.assign lhs (var_of_exp rhs)) prev_state
        | ConsInit (fn, _, _) ->
            List.map (Transfer.call lhs fn.vname) prev_state |> List.flatten
        | _ -> fail "unimplemented Local_init")
    | Set (lhs, rhs, _) -> (
        match (lhs, rhs.enode) with
        (* *a = b; *)
        | (Mem { enode = Lval (Var lhs, NoOffset); _ }, NoOffset), Lval _ ->
            List.map
              (Transfer.assign_lhs_deref (var_of_varinfo lhs) (var_of_exp rhs))
              prev_state
            |> List.flatten
        (* a = *b; *)
        | ( (Var lhs, NoOffset),
            Lval (Mem { enode = Lval (Var rhs, NoOffset); _ }, NoOffset) ) ->
            List.map
              (Transfer.assign_rhs_deref (var_of_varinfo lhs)
                 (var_of_varinfo rhs))
              prev_state
            |> List.flatten
        (* a = b; *)
        (* a = NULL; *)
        | (Var lhs, NoOffset), (Lval (Var _, NoOffset) | Const _) ->
            List.map
              (Transfer.assign (var_of_varinfo lhs) (var_of_exp rhs))
              prev_state
        | _ -> fail "unimplemented Set")
    | Call (lhs_opt, func, _, _) -> (
        match (lhs_opt, func.enode) with
        (* a = func() *)
        | Some (Var lhs, NoOffset), Lval (Var func, NoOffset) ->
            List.map (Transfer.call (var_of_varinfo lhs) func.vname) prev_state
            |> List.flatten
        | _ -> fail "unimplemented Call")
    | _ -> fail "other unimplemented Instr"
  in
  if Debug_output.get () then (
    print_endline "new state:";
    print_state new_state;
    print_newline ());
  new_state

(* `state` comes from doInstr, so it is actually the new state *)
let computeFirstPredecessor _ state = state

let entailment (lhs : SSL.t) (rhs : SSL.t) : bool =
  let vars = extract_vars @@ get_atoms rhs in
  let fresh_vars = List.filter is_fresh_var vars |> list_deduplicate in
  let fresh_vars = List.map (fun var -> SSL.Var var) fresh_vars in
  let quantified_rhs = SSL.mk_exists fresh_vars rhs in
  Solver.check_entl !solver lhs quantified_rhs

let join_states (old_state : SSL.t list) (new_state : SSL.t list) : SSL.t list =
  let concat_state = new_state @ old_state in
  let rec select_to_keep (to_keep : SSL.t list) (to_check : SSL.t list) :
      SSL.t list =
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
  select_to_keep [] concat_state

(* iterate over all formulas of new_state `phi`, and each one that doesn't satisfy
   (phi => old) has to be added to `old`. If old is not changed, None is returned. *)
let combinePredecessors (stmt : stmt) ~old:(old_state : t) (new_state : t) :
    t option =
  if Debug_output.get () then (
    print_warn "combinePredecessors";
    print_stmt stmt;
    print_endline "old state:";
    print_state old_state;
    print_endline "new state:";
    print_state new_state);

  let joined_state = join_states old_state new_state in
  if entailment (SSL.mk_or joined_state) (SSL.mk_or old_state) then (
    if Debug_output.get () then (
      print_endline "old state did not change";
      print_newline ());
    None)
  else (
    if Debug_output.get () then (
      print_endline "joined state:";
      print_state joined_state;
      print_newline ());
    Some joined_state)

(* we need to filter the formulas of `state` for each branch to only those,
   which are satisfiable in each of the branches *)
let doGuard (stmt : stmt) (exp : exp) (state : t) :
    t guardaction * t guardaction =
  if Debug_output.get () then (
    print_warn "doGuard";
    print_stmt stmt);

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
            let lhs = var_of_varinfo lhs in
            let rhs = var_of_varinfo rhs in
            (add_eq lhs rhs state, add_ne lhs rhs state)
        | _ -> (state, state))
    | BinOp (Ne, lhs, rhs, _) -> (
        (* if (a != b) {...} *)
        match (lhs.enode, rhs.enode) with
        | Lval (Var lhs, NoOffset), Lval (Var rhs, NoOffset) ->
            let lhs = var_of_varinfo lhs in
            let rhs = var_of_varinfo rhs in
            (add_ne lhs rhs state, add_eq lhs rhs state)
        | _ -> (state, state))
    (* all other conditions don't filter out any states *)
    | _ -> (state, state)
  in
  let th = List.filter check_sat th in
  let el = List.filter check_sat el in
  print_endline "then:";
  print_state th;
  print_endline "else:";
  print_state el;
  print_newline ();
  let th = if List.is_empty th then GUnreachable else GUse th in
  let el = if List.is_empty el then GUnreachable else GUse el in
  (th, el)

(* we always want to continue with analysis *)
let doStmt (_ : stmt) (_ : SSL.t list) : SSL.t list stmtaction = SDefault

(* simplify formulas and filter out unsatisfiable ones *)
let doEdge (prev_stmt : stmt) (next_stmt : stmt) (state : SSL.t list) :
    SSL.t list =
  if Debug_output.get () then (
    print_warn "doEdge";
    print_endline "previous stmt:";
    print_stmt prev_stmt;
    print_endline "next stmt:";
    print_stmt next_stmt;
    print_endline "original state:";
    print_state state);

  let open Simplification in
  let modified =
    List.map Simplifier.simplify state
    |> List.filter check_sat |> List.map remove_junk |> List.map remove_nil_vars
    |> List.map convert_to_ls
  in

  if Debug_output.get () then (
    print_endline "simplified state:";
    print_state modified;
    print_newline ());
  modified

module StmtStartData = struct
  type data = t

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

  let%test_unit "join" =
    let old = SSL.mk_star [ SSL.mk_pto x y' ] in
    let old_state = [ old ] in
    let new_state = [ SSL.mk_star [ SSL.mk_pto x z' ] ] in
    let joined = List.nth (join_states old_state new_state) 0 in
    assert_eq old joined

  let%test_unit "join2" =
    let old = SSL.mk_star [ SSL.mk_pto x y ] in
    let old_state = [ old ] in
    let new_state = [ SSL.mk_star [ SSL.mk_pto x z ] ] in
    let joined = List.nth (join_states old_state new_state) 0 in
    assert_eq old joined
end
