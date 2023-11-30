(* astral definuje a=b jako s(a) = s(b) && emp *)
open Dataflow2
open Astral
open Cil_types

let results = ref (Hashtbl.create 1024)
let solver = Solver.init ()

module Self = Plugin.Register (struct
  let name = "Shape analysis based using separation logic"
  let shortname = "SLplugin"
  let help = ""
end)

(* checks whether formula has any model *)
let check_sat (formula : SSL.t) : bool = Solver.check_sat solver formula
let mk_var (var : varinfo) : SSL.t = SSL.mk_var var.vname Sort.loc_ls

let mk_var_plain (var : varinfo) : SSL.Variable.t =
  match mk_var var with Var var -> var | _ -> failwith ""

let mk_fresh_var () : SSL.t =
  let fresh_var = SSL.mk_fresh_var "fresh" Sort.loc_ls in
  fresh_var

let mk_fresh_var_plain () : SSL.Variable.t =
  match mk_fresh_var () with Var var -> var | _ -> failwith ""

(* if <var> is ptr type, creates formula (<var> -> x') where x' is fresh primed variable *)
(* otherwise, returns prev_state *)
let mk_init (lhs : varinfo) (rhs : local_init) (prev_state : SSL.t) : SSL.t list
    =
  let is_alloc fn_name =
    (* frama-c has support for allocation functions *)
    fn_name = "malloc" || fn_name = "calloc" || fn_name = "realloc"
  in
  let var = mk_var lhs in
  let atom_list =
    match rhs with
    (* assuming all locations named by value are NULL *)
    | AssignInit (SingleInit rhs) ->
        [ SSL.mk_eq var @@ SSL.mk_nil () ] (* TODO handle all possible expr *)
    (* initialization by function call *)
    | ConsInit (fn, _, _) ->
        if is_alloc fn.vname then
          (* malloc can return valid pointer or nil *)
          [ SSL.mk_pto var @@ mk_fresh_var (); SSL.mk_eq var @@ SSL.mk_nil () ]
        else
          failwith
            "variable initialization with arbitrary function is not implemented"
    | _ -> failwith "target_loc: Compound initializers are not supported"
  in
  match lhs.vtype with
  (* if lhs is a pointer, add appropriate atom to init *)
  | TPtr (_, _) ->
      List.map (fun pto -> SSL.mk_star @@ [ prev_state; pto ]) atom_list
  | _ -> [ prev_state ]

let mk_assign (lhs : varinfo) (rhs : exp) (prev_state : SSL.t) : SSL.t =
  match (lhs.vtype, rhs.enode) with
  (* a = b *)
  | TPtr (_, _), Lval (Var rhs, NoOffset) ->
      SSL.mk_star
        [
          SSL.mk_eq (mk_var lhs) (mk_var rhs);
          SSL.substitute prev_state ~var:(mk_var_plain lhs)
            ~by:(mk_fresh_var_plain ());
        ]
  (* a = *b *)
  | TPtr (_, _), AddrOf (Var rhs, NoOffset) -> failwith "unimplemented"
  | _ -> prev_state

let mk_ptr_write (lhs : varinfo) (rhs : exp) (prev_state : SSL.t) : SSL.t =
  match (lhs.vtype, rhs.enode) with
  (* *a = b *)
  | TPtr (_, _), Lval (Var rhs, NoOffset) -> failwith "unimplemented"
  | _ -> prev_state

(* leaves in only those formulas, which satisfy `sat(phi * (lhs == rhs))` *)
let filter_eq (lhs : varinfo) (rhs : varinfo) (states : SSL.t list) : SSL.t list
    =
  List.filter
    (fun state_piece ->
      check_sat
      @@ SSL.mk_star [ state_piece; SSL.mk_eq (mk_var lhs) (mk_var rhs) ])
    states

(* filters out only those formulas, which satisfy `sat(phi * (lhs != rhs))` *)
let filter_ne (lhs : varinfo) (rhs : varinfo) (states : SSL.t list) : SSL.t list
    =
  List.filter
    (fun state_piece ->
      check_sat
      @@ SSL.mk_star [ state_piece; SSL.mk_distinct (mk_var lhs) (mk_var rhs) ])
    states

module Transfer = struct
  let name = "test"
  let debug = true

  type t = SSL.t list

  let copy state = state
  let pretty fmt state = List.iter (SSL.pp fmt) state

  let computeFirstPredecessor stmt prev_state =
    match stmt.skind with
    | Instr instr -> (
        match instr with
        | Set (lval, rhs, _) -> (
            match lval with
            (* a = b; *)
            (* a = *b; *)
            | Var lhs, NoOffset -> List.map (mk_assign lhs rhs) prev_state
            (* *a = b; *)
            | Mem exp, NoOffset -> (
                match exp.enode with
                (* *a = b; *)
                | AddrOf (Var lhs, NoOffset) ->
                    List.map (mk_ptr_write lhs rhs) prev_state
                | _ -> failwith "unimplemented")
            | _ -> failwith "unimplemented")
        (* int *a = malloc(...) *)
        | Local_init (lhs, rhs, _) ->
            List.flatten @@ List.map (mk_init lhs rhs) prev_state
        | _ -> prev_state)
    | _ -> prev_state

  (* Stmt is reached multiple times ~> join operator *)
  (* iterate over all formulas of new_state `phi`, and each one that doesn't satisfy
     (phi -> old) has to be added to `old`. If old is not changed, None is returned. *)
  let combinePredecessors _ ~old:old_state new_state =
    let new_components =
      List.filter
        (fun new_piece ->
          not @@ Solver.check_entl solver new_piece @@ SSL.mk_or old_state)
        new_state
    in
    if List.length new_components == 0 then None
    else Some (new_components @ old_state)

  (* ??? *)
  let doInstr _ _ s = s

  (* we need to filter the formulas of `state` for each branch to only those, which  *)
  let doGuard _ exp state =
    let th, el =
      match exp.enode with
      | BinOp (Eq, lhs, rhs, _) -> (
          (* if (a == b) {...} *)
          match (lhs.enode, rhs.enode) with
          | Lval (Var lhs, NoOffset), Lval (Var rhs, NoOffset) ->
              (filter_eq lhs rhs state, filter_ne lhs rhs state)
          | _ -> (state, state))
      | BinOp (Ne, lhs, rhs, _) -> (
          (* if (a != b) {...} *)
          match (lhs.enode, rhs.enode) with
          | Lval (Var lhs, NoOffset), Lval (Var rhs, NoOffset) ->
              (filter_ne lhs rhs state, filter_eq lhs rhs state)
          | _ -> (state, state))
      (* all other conditions don't filter out any states *)
      | _ -> (state, state)
    in
    (GUse th, GUse el)

  (* generate new state that will be used in combinePredecessors *)
  let doStmt stmt prev_state = SUse (computeFirstPredecessor stmt prev_state)

  (* probably not useful *)
  let doEdge _ _ s = s

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
end

module Analysis = Forwards (Transfer)

let print_stmt (result : stmt * SSL.t list) =
  let stmt, formulas = result in
  print_newline ();
  (* statements are printed yellow *)
  Self.printf "\x1b[33m%a\x1b[0m" Cil_datatype.Stmt.pretty stmt;
  let space = "    " in
  print_string space;
  List.map (fun f -> SSL.show @@ Simplifier.simplify f) formulas
  |> String.concat ("\n" ^ space)
  |> print_string

let run () =
  let main, _ = Globals.entry_point () in
  let first_stmt = Kernel_function.find_first_stmt main in
  Hashtbl.add !results first_stmt [ SSL.mk_emp () ];
  Analysis.compute [ first_stmt ];
  Hashtbl.to_seq !results |> List.of_seq
  |> List.sort (fun a b ->
         let a, _ = a in
         let b, _ = b in
         a.sid - b.sid)
  |> List.iter print_stmt |> ignore

let () = Db.Main.extend run
