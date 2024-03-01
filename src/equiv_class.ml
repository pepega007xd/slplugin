open Astral
open Common

let find_new_equiv_vars (atoms : SSL.t list) (found_vars : SSL.Variable.t list)
    : SSL.Variable.t list =
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
      | SSL.Eq _ -> fail "equality with less/more than two operators"
      | _ -> None)
    atoms

(* finds equivalence class of variable given as only member of
   list `found_vars` on list on atoms `atoms` *)
let rec mk_equiv_class_rec (atoms : SSL.t list)
    (found_vars : SSL.Variable.t list) : SSL.Variable.t list =
  match find_new_equiv_vars atoms found_vars with
  | [] -> found_vars
  | new_vars -> mk_equiv_class_rec atoms (found_vars @ new_vars)

let expose_pto (var : SSL.Variable.t) (formula : SSL.t) : SSL.t list =
  let atoms = get_atoms formula in
  let equiv_class = mk_equiv_class_rec atoms [ var ] in
  let new_atoms, rest =
    List.partition_map
      (fun atom ->
        match atom with
        | SSL.LS (Var src, Var dst) when list_contains equiv_class src ->
            let new_fresh_var = mk_fresh_var "ls" in
            Left
              [
                (* ls was a simple pto *)
                SSL.mk_pto (Var src) (Var dst);
                (* ls was at least length 2 *)
                SSL.mk_star
                  [
                    SSL.mk_pto (Var src) (Var new_fresh_var);
                    SSL.mk_ls (Var new_fresh_var) (Var dst);
                    SSL.mk_distinct (Var new_fresh_var) (Var dst);
                  ];
              ]
        | SSL.PointsTo (Var src, _) when list_contains equiv_class src ->
            Left [ atom ]
        | other -> Right other)
      atoms
  in
  let new_atoms = List.flatten new_atoms in
  List.map
    (fun atom -> SSL.mk_star (atom :: rest) |> Simplifier.simplify)
    new_atoms

(* accepts variable and formula, finds pto from equivalence class of variable,
   returns both sides of pto atom *)
let extract_target_single (formula : SSL.t) (var : SSL.Variable.t) :
    SSL.Variable.t * SSL.Variable.t =
  let atoms = get_atoms formula in
  let equiv_class = mk_equiv_class_rec atoms [ var ] in
  let src, dst =
    List.find_map
      (fun atom ->
        match atom with
        | SSL.PointsTo (Var src, LS_t dst) when list_contains equiv_class src ->
            Some (src, dst)
        | _ -> None)
      atoms
    |> Option.get
  in
  (src, dst)

let extract_target (formula : SSL.t) (var : SSL.Variable.t) :
    (SSL.Variable.t * SSL.Variable.t * SSL.t) list =
  let formulas = expose_pto var formula in
  List.map
    (fun formula ->
      let src, dst = extract_target_single formula var in
      (src, dst, formula))
    formulas
