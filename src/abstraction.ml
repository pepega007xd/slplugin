open Astral
open Common

let convert_to_ls (formula : SSL.t) : SSL.t =
  let is_unique_fresh var =
    is_fresh_var var && Formula.count_occurences_excl_distinct var formula = 2
  in

  let extract_ptr (ptr : SSL.t) : SSL.Variable.t * SSL.Variable.t =
    match ptr with
    | PointsTo (Var src, LS_t dst) -> (src, dst)
    | LS (Var src, Var dst) -> (src, dst)
    | _ -> fail "unreachable"
  in

  let do_abstraction (formula : SSL.t) (atom : SSL.t) : SSL.t =
    let src_var, middle_var = extract_ptr atom in
    let dst_var =
      Formula.get_spatial_target middle_var Preprocessing.Next formula
    in
    (* conditions for abstraction *)
    if
      (* middle must be fresh variable, and occur only in these two predicates *)
      is_unique_fresh middle_var
      (* src must be different from dst (checked using solver) *)
      && Astral_query.check_inequality src_var dst_var formula
    then
      formula
      |> Formula.remove_spatial_from src_var
      |> Formula.remove_spatial_from middle_var
      |> Formula.add_atom (SSL.mk_ls (Var src_var) (Var dst_var))
      |> Formula.add_atom (SSL.mk_distinct (Var src_var) (Var dst_var))
    else formula
  in

  formula |> Formula.get_atoms
  |> List.filter (function
       | SSL.LS _ | SSL.PointsTo (_, LS_t _) -> true
       | _ -> false)
  |> List.fold_left do_abstraction formula

module Tests = struct
  open Testing

  let%test_unit "abstraction_ls_nothing" =
    let input = SSL.mk_star [ SSL.mk_pto x y'; SSL.mk_pto y' z ] in
    let result = convert_to_ls input in
    assert_eq input result

  let%test_unit "abstraction_ls_allocated_end" =
    let input =
      SSL.mk_star [ SSL.mk_pto x y'; SSL.mk_pto y' z; SSL.mk_distinct x z ]
    in
    let result = convert_to_ls input in
    let expected = SSL.mk_star [ SSL.mk_ls x z; SSL.mk_distinct x z ] in
    assert_eq result expected

  let%test_unit "abstraction_ls_nil_end" =
    let input = SSL.mk_star [ SSL.mk_pto x y'; SSL.mk_pto y' nil ] in
    let result = convert_to_ls input in
    let expected = SSL.mk_star [ SSL.mk_ls x nil; SSL.mk_distinct x nil ] in
    assert_eq result expected
end
