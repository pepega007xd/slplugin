open Config
open Astral
open Common
open Printing

(** removes fresh variables by substituting them with other variables in their equivalence class *)
let reduce_equiv_classes (formula : SSL.t) : SSL.t =
  let remove_fresh_vars (formula : SSL.t) (equiv_class : SSL.Variable.t list) :
      SSL.t =
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
        SSL.substitute formula ~var:to_remove ~by:substitution_var)
      formula fresh_vars
  in
  formula |> Formula.get_equiv_classes
  |> List.fold_left remove_fresh_vars formula
  (* remove multiple occurences of a single variable in an equiv class created by a substitution *)
  |> Formula.map_equiv_classes @@ List.sort_uniq SSL.Variable.compare

let remove_distinct_only (formula : SSL.t) : SSL.t =
  let atoms = Formula.get_atoms formula in
  let fresh_distinct_only (var : SSL.Variable.t) : bool =
    is_fresh_var var && Formula.count_occurences_excl_distinct var formula = 0
  in
  List.filter
    (function
      | SSL.Distinct [ Var lhs; Var rhs ] ->
          (not @@ fresh_distinct_only lhs) || (not @@ fresh_distinct_only rhs)
      | _ -> true)
    atoms
  |> SSL.mk_star

(* removes all spatial atoms where the source variable doesn't appear anywhere else in the formula *)
let remove_leaks (formula : SSL.t) : SSL.t =
  let is_fresh_unique (var : SSL.Variable.t) : bool =
    is_fresh_var var && Formula.count_occurences_excl_distinct var formula = 1
  in
  let atoms = Formula.get_atoms formula in
  let junk_atoms, valid_atoms =
    List.partition
      (function
        | SSL.PointsTo (Var src, _)
        (* TODO: remove cyclic lists *)
        | SSL.LS (Var src, _)
        | SSL.NLS (Var src, _, _) ->
            is_fresh_unique src
        | SSL.DLS (Var src, Var dst, _, _) ->
            is_fresh_unique src && is_fresh_unique dst
        | _ -> false)
      atoms
  in

  if List.is_empty junk_atoms then formula
  else (
    Self.warning "removing junk: %a" SSL.pp (SSL.mk_star junk_atoms);
    SSL.mk_star valid_atoms)

module Tests = struct
  open Testing

  let%test_unit "reduce_equiv_classes" =
    let input = SSL.mk_star [ SSL.mk_eq x y'; SSL.mk_eq y' nil ] in
    let result = reduce_equiv_classes input in
    let expected = SSL.mk_star [ SSL.mk_eq x nil ] in
    assert_eq result expected

  let%test_unit "reduce_equiv_classes" =
    let input =
      SSL.mk_star [ SSL.mk_eq x y'; SSL.mk_eq y' z'; SSL.mk_eq z' z ]
    in
    let result = reduce_equiv_classes input in
    let expected = SSL.mk_star [ SSL.mk_eq x z ] in
    assert_eq result expected

  let%test_unit "reduce_equiv_classes" =
    let input =
      SSL.mk_star [ SSL.mk_eq x' y'; SSL.mk_eq x x'; SSL.mk_eq y y' ]
    in
    let result = reduce_equiv_classes input in
    let expected = SSL.mk_star [ SSL.mk_eq x y ] in
    assert_eq result expected

  let%test_unit "reduce_equiv_classes" =
    let input = SSL.mk_star [ SSL.mk_eq x' y'; SSL.mk_eq y' z' ] in
    let result = reduce_equiv_classes input in
    let expected = SSL.mk_star [] in
    assert_eq result expected

  let%test_unit "reduce_equiv_classes" =
    let input =
      SSL.mk_star [ SSL.mk_pto x x'; SSL.mk_eq x' y'; SSL.mk_ls y' z ]
    in
    let result = reduce_equiv_classes input in
    let expected = SSL.mk_star [ SSL.mk_pto x x'; SSL.mk_ls x' z ] in
    assert_eq result expected
end
