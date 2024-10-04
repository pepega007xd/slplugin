open Astral
open Common

let convert_to_ls (formula : Formula.t) : Formula.t =
  let is_unique_fresh var =
    is_fresh_var var && Formula.count_occurences_excl_distinct var formula = 2
  in

  let add_min_lengths (lhs : Formula.atom) (rhs : Formula.atom) : int =
    max
      (Formula.get_spatial_atom_min_length lhs
      + Formula.get_spatial_atom_min_length rhs)
      2
  in

  let extract_ptr (ptr : Formula.atom) : SSL.Variable.t * SSL.Variable.t =
    match ptr with
    | PointsTo (src, LS_t dst) -> (src, dst)
    | LS ls -> (ls.first, ls.next)
    | _ -> fail "unreachable"
  in

  let do_abstraction (formula : Formula.t) (first_atom : Formula.atom) :
      Formula.t =
    let src_var, middle_var = extract_ptr first_atom in
    Formula.get_spatial_atom_from middle_var formula |> function
    | Some second_atom ->
        let dst_var =
          Formula.get_spatial_target middle_var Preprocessing.Next formula
          |> Option.get
        in
        (* conditions for abstraction *)
        if
          (* both spatial predicates (src|->middle and middle|->dst) must still be in formula *)
          List.mem first_atom formula
          (* middle must be fresh variable, and occur only in these two predicates *)
          && is_unique_fresh middle_var
          (* src must be different from dst (checked using solver) *)
          && Astral_query.check_inequality src_var dst_var formula
        then
          let min_length = add_min_lengths first_atom second_atom in
          formula
          |> Formula.remove_atom first_atom
          |> Formula.remove_atom second_atom
          |> Formula.add_atom @@ Formula.mk_ls src_var dst_var min_length
        else formula
    | None -> formula
  in

  formula
  |> List.filter (function
       | Formula.LS _ | Formula.PointsTo (_, LS_t _) -> true
       | _ -> false)
  |> List.fold_left do_abstraction formula

module Tests = struct
  open Testing

  (* let%test_unit "abstraction_ls_nothing" = *)
  (*   let input = SSL.mk_star [ SSL.mk_pto x y'; SSL.mk_pto y' z ] in *)
  (*   let result = convert_to_ls input in *)
  (*   assert_eq input result *)
  (**)
  (* let%test_unit "abstraction_ls_allocated_end" = *)
  (*   let input = *)
  (*     SSL.mk_star [ SSL.mk_pto x y'; SSL.mk_pto y' z; SSL.mk_distinct x z ] *)
  (*   in *)
  (*   let result = convert_to_ls input in *)
  (*   let expected = SSL.mk_star [ SSL.mk_ls x z; SSL.mk_distinct x z ] in *)
  (*   assert_eq result expected *)
  (**)
  (* let%test_unit "abstraction_ls_nil_end" = *)
  (*   let input = SSL.mk_star [ SSL.mk_pto x y'; SSL.mk_pto y' nil ] in *)
  (*   let result = convert_to_ls input in *)
  (*   let expected = SSL.mk_star [ SSL.mk_ls x nil; SSL.mk_distinct x nil ] in *)
  (*   assert_eq result expected *)
end
