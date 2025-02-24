open Config
open Astral
open Common

(** removes fresh variables by substituting them with other variables in their
    equivalence class *)
let reduce_equiv_classes (formula : Formula.t) : Formula.t =
  let remove_fresh_vars (formula : Formula.t) (equiv_class : Formula.var list) :
      Formula.t =
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
      | _ -> assert false
    in

    List.fold_left
      (fun formula to_remove ->
        Formula.substitute formula ~var:to_remove ~by:substitution_var)
      formula fresh_vars
  in
  formula |> Formula.get_equiv_classes
  (* filter out equivalence classes with less than two members *)
  |> List.filter (function [] | [ _ ] -> false | _ -> true)
  |> List.fold_left remove_fresh_vars formula
  (* remove multiple occurences of a single variable 
     in an equiv class created by a substitution *)
  |> Formula.map_equiv_classes @@ List.sort_uniq SL.Variable.compare

let remove_distinct_only (formula : Formula.t) : Formula.t =
  let fresh_distinct_only (var : Formula.var) : bool =
    is_fresh_var var && Formula.count_occurences_excl_distinct var formula = 0
  in
  List.filter
    (function
      | Formula.Distinct (lhs, rhs) ->
          not @@ (fresh_distinct_only lhs || fresh_distinct_only rhs)
      | _ -> true)
    formula

(* removes all spatial atoms where the source variable doesn't appear anywhere else in the formula *)
let remove_leaks (formula : Formula.t) : Formula.t =
  let is_fresh_unique (var : Formula.var) : bool =
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

let remove_single_eq (formula : Formula.t) : Formula.t =
  let to_remove =
    formula |> Formula.get_equiv_classes
    |> List.filter (fun eq_class -> List.length eq_class < 2)
  in
  List.fold_left
    (fun formula eq_class -> Formula.remove_equiv_class eq_class formula)
    formula to_remove

let convert_vars_to_fresh (vars : Formula.var list) (formula : Formula.t) :
    Formula.t =
  List.fold_left
    (fun formula var -> Formula.substitute_by_fresh var formula)
    formula vars

let join_abstraction_lengths (lhs : Formula.t) (rhs : Formula.t) :
    (Formula.t list * Formula.t) option =
  let rest, first = List.partition (fun atom -> List.mem atom rhs) lhs in
  let second = List.filter (fun atom -> not @@ List.mem atom lhs) rhs in
  match (first, second) with
  | [ first ], [ second ] ->
      let new_atom =
        match (Formula.pto_to_list first, Formula.pto_to_list second) with
        | LS lhs, LS rhs
          when { lhs with min_len = 0 } = { rhs with min_len = 0 } ->
            Some (Formula.LS { lhs with min_len = min lhs.min_len rhs.min_len })
        | DLS lhs, DLS rhs
          when { lhs with min_len = 0 } = { rhs with min_len = 0 } ->
            Some
              (Formula.DLS { lhs with min_len = min lhs.min_len rhs.min_len })
        | NLS lhs, NLS rhs
          when { lhs with min_len = 0 } = { rhs with min_len = 0 } ->
            Some
              (Formula.NLS { lhs with min_len = min lhs.min_len rhs.min_len })
        | _ -> None
      in
      Option.map (fun atom -> ([ lhs; rhs ], atom :: rest)) new_atom
  | _ -> None

let join_similar_formulas (state : Formula.state) : Formula.state =
  if List.length state >= 2 then
    let original, joined =
      state
      |> Common.list_map_pairs join_abstraction_lengths
      |> List.filter_map Fun.id |> List.split
    in
    let original = List.flatten original in
    state
    |> List.filter (fun formula -> not @@ List.mem formula original)
    |> ( @ ) joined
  else state

module Tests = struct
  open Testing
  open Formula

  let%test "remove_distinct_only" =
    let f = [ Distinct (nil, x') ] |> remove_distinct_only in
    assert_eq f []

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
