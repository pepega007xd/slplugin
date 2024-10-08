open Config
open Astral
open Common

(** removes fresh variables by substituting them with other variables in their equivalence class *)
let reduce_equiv_classes (formula : Formula.t) : Formula.t =
  let remove_fresh_vars (formula : Formula.t)
      (equiv_class : SSL.Variable.t list) : Formula.t =
    let fresh_vars, program_vars = List.partition is_fresh_var equiv_class in

    (* variable, to which all fresh vars will be changed *)
    let substitution_var =
      match (List.mem Formula.nil equiv_class, program_vars, fresh_vars) with
      (* preferrably, pick nil *)
      | true, _, _ -> Formula.nil
      (* if not possible, pick a program variable *)
      | _, program_var :: _, _ -> program_var
      (* if not possible, pick any fresh variable *)
      | _, _, fresh_var :: _ -> fresh_var
      | _ -> fail "unreachable (equivalence class cannot be empty)"
    in

    List.fold_left
      (fun formula to_remove ->
        Formula.substitute formula ~var:to_remove ~by:substitution_var)
      formula fresh_vars
  in
  formula |> Formula.get_equiv_classes
  |> List.fold_left remove_fresh_vars formula
  (* remove multiple occurences of a single variable in an equiv class created by a substitution *)
  |> Formula.map_equiv_classes @@ List.sort_uniq SSL.Variable.compare

let remove_distinct_only (formula : Formula.t) : Formula.t =
  let fresh_distinct_only (var : SSL.Variable.t) : bool =
    is_fresh_var var && Formula.count_occurences_excl_distinct var formula = 0
  in
  List.filter
    (function
      | Formula.Distinct (lhs, rhs) ->
          (not @@ fresh_distinct_only lhs) || (not @@ fresh_distinct_only rhs)
      | _ -> true)
    formula

(* removes all spatial atoms where the source variable doesn't appear anywhere else in the formula *)
let remove_leaks (formula : Formula.t) : Formula.t =
  let is_fresh_unique (var : SSL.Variable.t) : bool =
    is_fresh_var var && Formula.count_occurences_excl_distinct var formula = 1
  in
  let junk_atoms, valid_atoms =
    List.partition
      (function
        | Formula.PointsTo (src, _) -> is_fresh_unique src
        (* TODO: remove cyclic lists *)
        | Formula.LS ls -> is_fresh_unique ls.first
        | Formula.DLS dls ->
            is_fresh_unique dls.first && is_fresh_unique dls.last
        | Formula.NLS nls -> is_fresh_unique nls.first
        | _ -> false)
      formula
  in

  if List.is_empty junk_atoms then formula
  else (
    List.iter
      (Self.warning ~current:true "leak of atom %a" Formula.pp_atom)
      junk_atoms;
    valid_atoms)

module Tests = struct
  open Testing

  (* let%test_unit "reduce_equiv_classes" = *)
  (*   let input = SSL.mk_star [ SSL.mk_eq x y'; SSL.mk_eq y' nil ] in *)
  (*   let result = reduce_equiv_classes input in *)
  (*   let expected = SSL.mk_star [ SSL.mk_eq x nil ] in *)
  (*   assert_eq result expected *)
  (**)
  (* let%test_unit "reduce_equiv_classes" = *)
  (*   let input = *)
  (*     SSL.mk_star [ SSL.mk_eq x y'; SSL.mk_eq y' z'; SSL.mk_eq z' z ] *)
  (*   in *)
  (*   let result = reduce_equiv_classes input in *)
  (*   let expected = SSL.mk_star [ SSL.mk_eq x z ] in *)
  (*   assert_eq result expected *)
  (**)
  (* let%test_unit "reduce_equiv_classes" = *)
  (*   let input = *)
  (*     SSL.mk_star [ SSL.mk_eq x' y'; SSL.mk_eq x x'; SSL.mk_eq y y' ] *)
  (*   in *)
  (*   let result = reduce_equiv_classes input in *)
  (*   let expected = SSL.mk_star [ SSL.mk_eq x y ] in *)
  (*   assert_eq result expected *)
  (**)
  (* let%test_unit "reduce_equiv_classes" = *)
  (*   let input = SSL.mk_star [ SSL.mk_eq x' y'; SSL.mk_eq y' z' ] in *)
  (*   let result = reduce_equiv_classes input in *)
  (*   let expected = SSL.mk_star [] in *)
  (*   assert_eq result expected *)
  (**)
  (* let%test_unit "reduce_equiv_classes" = *)
  (*   let input = *)
  (*     SSL.mk_star [ SSL.mk_pto x x'; SSL.mk_eq x' y'; SSL.mk_ls y' z ] *)
  (*   in *)
  (*   let result = reduce_equiv_classes input in *)
  (*   let expected = SSL.mk_star [ SSL.mk_pto x x'; SSL.mk_ls x' z ] in *)
  (*   assert_eq result expected *)
end
