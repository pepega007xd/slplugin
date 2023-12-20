(* astral definuje a=b jako s(a) = s(b) && emp *)
open Dataflow2
open Astral
open Cil_types

type t2 = SSL.t list

let results = ref (Hashtbl.create 1024)
let solver = Solver.init ()

module Self = Plugin.Register (struct
  let name = "Shape analysis"
  let shortname = "SLplugin"
  let help = ""
end)

let print_stmt (stmt : Cil_types.stmt) =
  (* print in yellow color *)
  print_string "\x1b[33m";

  let stmt = Format.asprintf "%a" Cil_datatype.Stmt.pretty stmt in
  (String.split_on_char '\n' stmt |> function
   | [] -> print_endline "<empty stmt>"
   | a :: [] -> print_endline a
   | [ a; b ] -> print_endline (a ^ "\n" ^ b)
   | a :: b :: _ -> print_endline (a ^ "\n" ^ b ^ "\n" ^ "..."));

  print_string "\x1b[0m"

let print_state (state : t2) =
  let space = "    " in
  print_string space;
  List.map (fun f -> SSL.show @@ Simplifier.simplify f) state
  |> String.concat ("\n" ^ space)
  |> print_endline

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

let is_alloc fn_name =
  (* frama-c has support for allocation functions *)
  fn_name = "malloc" || fn_name = "calloc" || fn_name = "realloc"

(* if <var> is ptr type, creates formula (<var> -> x') where x' is fresh primed variable *)
(* otherwise, returns prev_state *)
let mk_init (lhs : varinfo) (rhs : local_init) (prev_state : SSL.t) : t2 =
  let lhs_var = mk_var lhs in
  let new_atoms =
    match rhs with
    (* assuming all locations named by value are NULL *)
    (* TODO handle all possible expr *)
    | AssignInit (SingleInit exp) -> [ SSL.mk_eq lhs_var @@ SSL.mk_nil () ]
    (* initialization by function call *)
    | ConsInit (fn, _, _) ->
        if is_alloc fn.vname then
          [
            (* malloc can return valid pointer or nil *)
            SSL.mk_pto lhs_var @@ mk_fresh_var ();
            SSL.mk_eq lhs_var @@ SSL.mk_nil ();
          ]
        else
          failwith
            "variable initialization with arbitrary function is not implemented"
    | _ -> failwith "target_loc: Compound initializers are not supported"
  in
  match lhs.vtype with
  (* if lhs is a pointer, add appropriate atom to init *)
  | TPtr (_, _) ->
      List.map (fun pto -> SSL.mk_star @@ [ prev_state; pto ]) new_atoms
  | _ -> [ prev_state ]

let list_contains (list : 'a List.t) (elem : 'a) : bool =
  match List.find_opt (fun x -> x = elem) list with
  | Some _ -> true
  | None -> false

let find_new_equiv_vars (atoms : t2) (found_vars : SSL.Variable.t list) :
    SSL.Variable.t list =
  List.filter_map
    (fun atom ->
      match atom with
      | SSL.Eq [ SSL.Var lhs; SSL.Var rhs ] ->
          if
            (* lhs is new to equivalence class *)
            list_contains found_vars rhs && (not @@ list_contains found_vars lhs)
          then Some lhs
          else if
            (* rhs is new to equivalence class *)
            list_contains found_vars lhs && (not @@ list_contains found_vars rhs)
          then Some rhs
          else None
      | SSL.Eq _ -> failwith "equality with less/more than two operators"
      | _ -> None)
    atoms

(* finds equivalence class of variable given as only member of
   list `found_vars` on list on atoms `atoms` *)
let rec mk_equiv_class_rec (atoms : t2) (found_vars : SSL.Variable.t list) :
    SSL.Variable.t list =
  match find_new_equiv_vars atoms found_vars with
  | [] -> found_vars
  | new_vars -> mk_equiv_class_rec atoms (found_vars @ new_vars)

(* accepts variable and formula, finds pto from equivalence class of variable,
   returns both sides of pto atom *)
let find_pto (formula : SSL.t) (var : SSL.Variable.t) :
    (SSL.Variable.t * SSL.Variable.t) option =
  (* flattens formula to single sep: (star a b c d ...) *)
  let formula = Simplifier.simplify formula in
  match formula with
  | SSL.Star atoms -> (
      let equiv_class = mk_equiv_class_rec atoms [ var ] in
      let target_var_struct =
        List.find_map
          (fun atom ->
            match atom with
            | SSL.PointsTo (src, dst) ->
                if list_contains equiv_class var then Some (src, dst) else None
            | _ -> None)
          atoms
      in
      match target_var_struct with
      (* TODO differentiate between list segment and simple pto, or is this simple pto? *)
      | Some (SSL.Var src, LS_t dst) -> Some (src, dst)
      | Some _ -> failwith "DLS/NLS found in get_pto"
      | None ->
          failwith
            "did not find any points-to target in `get_pto` (maybe it's ls?)")
  | _ -> failwith "unreachable"

let is_allocated (formula : SSL.t) (var : SSL.Variable.t) : bool =
  match find_pto formula var with Some _ -> true | None -> false

let substitute_var (var : varinfo) (formula : SSL.t) : SSL.t =
  SSL.substitute formula ~var:(mk_var_plain var) ~by:(mk_fresh_var_plain ())

let mk_assign (lhs : varinfo) (rhs : exp) (prev_state : SSL.t) : SSL.t =
  match (lhs.vtype, rhs.enode) with
  (* a = b *)
  | TPtr (_, _), Lval (Var rhs, NoOffset) ->
      SSL.mk_star
        [ SSL.mk_eq (mk_var lhs) (mk_var rhs); substitute_var lhs prev_state ]
  (* a = *b *)
  | TPtr (_, _), AddrOf (Var rhs, NoOffset) ->
      let src, dst = Option.get @@ find_pto prev_state (mk_var_plain rhs) in
      SSL.mk_star
        [ SSL.mk_eq (mk_var lhs) (SSL.Var dst); substitute_var lhs prev_state ]
  | _ -> prev_state

let mk_ptr_write (lhs : varinfo) (rhs : exp) (prev_state : SSL.t) : SSL.t =
  match (lhs.vtype, rhs.enode) with
  (* *a = b *)
  | TPtr (_, _), Lval (Var rhs, NoOffset) -> (
      let src, dst = Option.get @@ find_pto prev_state (mk_var_plain lhs) in
      match prev_state with
      | SSL.Star atoms ->
          List.map
            (fun atom ->
              if atom = SSL.mk_pto (SSL.Var src) (SSL.Var dst) then
                SSL.mk_pto (SSL.Var src) (mk_var rhs)
              else atom)
            atoms
          |> SSL.mk_star
      | _ -> failwith "unreachable")
  | _ -> prev_state

let mk_call (lhs : varinfo) (func : varinfo) (formula : SSL.t) : t2 =
  let formula = substitute_var lhs formula in
  let lhs = mk_var lhs in
  if is_alloc func.vname then
    [
      SSL.mk_star [ SSL.mk_pto lhs @@ mk_fresh_var (); formula ];
      SSL.mk_star [ SSL.mk_eq lhs @@ SSL.mk_nil (); formula ];
    ]
  else failwith "mk_call: function calls are not implemented"

(* leaves in only those formulas, which satisfy `sat(phi * (lhs == rhs))` *)
let filter_eq (lhs : varinfo) (rhs : varinfo) (states : t2) : t2 =
  List.filter
    (fun state_piece ->
      check_sat
      @@ SSL.mk_star [ state_piece; SSL.mk_eq (mk_var lhs) (mk_var rhs) ])
    states

(* filters out only those formulas, which satisfy `sat(phi * (lhs != rhs))` *)
let filter_ne (lhs : varinfo) (rhs : varinfo) (states : t2) : t2 =
  List.filter
    (fun state_piece ->
      check_sat
      @@ SSL.mk_star [ state_piece; SSL.mk_distinct (mk_var lhs) (mk_var rhs) ])
    states

let is_prime_var (var : SSL.Variable.t) : bool =
  let var_name, _ = var in
  if String.contains var_name '!' then true else false

(* removes all atoms of the form (x' -> y), where x' doesn't appear anywhere else *)
let remove_junk (formula : SSL.t) : SSL.t =
  let atoms =
    match formula with
    | Star atoms -> atoms
    | _ -> failwith "remove_junk: formula is not Star of atoms"
  in
  failwith "todo"

module Transfer = struct
  let name = "test"
  let debug = true

  type t = SSL.t list

  let copy state = state
  let pretty fmt state = List.iter (SSL.pp fmt) state

  (* *this* is the transfer function for instructions, we take the instr and previous
     state, and create new state *)
  let doInstr (stmt : stmt) (instr : instr) prev_state =
    match instr with
    | Local_init (lhs, rhs, _) ->
        (* int *a = malloc(...) *)
        Printf.printf "local_init lhs: %s\n" lhs.vname;
        List.flatten @@ List.map (mk_init lhs rhs) prev_state
    | Set (lhs, rhs, _) -> (
        match lhs with
        (* a = b; *)
        (* a = *b; *)
        | Var lhs, NoOffset -> List.map (mk_assign lhs rhs) prev_state
        (* *a = b; *)
        | Mem { eid; enode = Lval (Var lhs, NoOffset); eloc }, NoOffset ->
            List.map (mk_ptr_write lhs rhs) prev_state
        | _ -> failwith "computeFirstPredecessor: unimplemented Set")
    | Call (lhs_opt, func, _, _) -> (
        match (lhs_opt, func.enode) with
        (* a = func() *)
        | Some (Var lhs, NoOffset), Lval (Var func, NoOffset) ->
            List.flatten @@ List.map (mk_call lhs func) prev_state
        | _ -> prev_state)
    | _ -> failwith "unimplemented"

  (* `state` comes from doInstr, so it is actually the new state *)
  let computeFirstPredecessor _ state = state

  (* Stmt is reached multiple times ~> join operator *)
  (* iterate over all formulas of new_state `phi`, and each one that doesn't satisfy
     (phi -> old) has to be added to `old`. If old is not changed, None is returned. *)
  let combinePredecessors (stmt : stmt) ~old:(old_state : t) (new_state : t) :
      t option =
    print_endline "\ncombinePredecessors\nold_state:";
    print_state old_state;
    print_endline "new_state:";
    print_state new_state;
    print_stmt stmt;
    let new_components =
      List.filter
        (fun new_piece ->
          not @@ Solver.check_entl solver new_piece @@ SSL.mk_or old_state)
        new_state
    in
    if List.length new_components == 0 then (
      Printf.printf "Result: state did not change\n";
      None)
    else
      let joined_state = new_components @ old_state in
      Printf.printf "Result: state changed\njoined_state:\n";
      print_state joined_state;
      Some joined_state

  (* we need to filter the formulas of `state` for each branch to only those, which  *)
  let doGuard _ (exp : exp) (state : t) : t guardaction * t guardaction =
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

  (* probably not useful *)
  let doStmt _ _ = SDefault

  (* simplify formulas and filter out unsatisfiable ones *)
  let doEdge _ _ state =
    let simplified = List.map Simplifier.simplify state in
    List.filter check_sat simplified

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

let print_result (result : stmt * t2) =
  let stmt, state = result in
  print_newline ();
  print_stmt stmt;
  print_state state

let run () =
  let main, _ = Globals.entry_point () in
  let first_stmt = Kernel_function.find_first_stmt main in

  (* print control from automaton to `cfa.dot` *)
  let automaton = Interpreted_automata.get_automaton main in
  let file = Out_channel.open_text "cfa.dot" in
  Interpreted_automata.output_to_dot ~labeling:`Stmt
    ~wto:(Interpreted_automata.get_wto main)
    file automaton;

  Hashtbl.add !results first_stmt [ SSL.mk_emp () ];
  Analysis.compute [ first_stmt ];

  Hashtbl.to_seq !results |> List.of_seq
  |> List.sort (fun a b ->
         let a, _ = a in
         let b, _ = b in
         a.sid - b.sid)
  |> List.iter print_result |> ignore

let () = Db.Main.extend run
