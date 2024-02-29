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

(* accepts variable and formula, finds pto from equivalence class of variable,
   returns both sides of pto atom *)
let find_pto (formula : SSL.t) (var : SSL.Variable.t) :
    (SSL.Variable.t * SSL.Variable.t) option =
  (* flattens formula to single sep: (star a b c d ...) *)
  let formula = Simplifier.simplify formula in
  let atoms = get_atoms formula in
  let equiv_class = mk_equiv_class_rec atoms [ var ] in
  let target_var_struct =
    List.find_map
      (fun atom ->
        match atom with
        | SSL.PointsTo (Var src, LS_t dst) when list_contains equiv_class src ->
            Some (src, dst)
        | _ -> None)
      atoms
  in
  match target_var_struct with
  | Some (src, dst) -> Some (src, dst)
  | None ->
      fail "did not find any points-to target in `get_pto` (maybe it's ls?)"
