open Astral
open Common

let is_unique_fresh (var : Formula.var) (formula : Formula.t) : bool =
  is_fresh_var var && Formula.count_occurences_excl_distinct var formula = 2

let add_min_lengths (lhs : int) (rhs : int) : int = min 2 (lhs + rhs)

let convert_to_ls (formula : Formula.t) : Formula.t =
  let atom_to_ls : Formula.atom -> Formula.ls option = function
    | Formula.LS ls -> Some ls
    | Formula.PointsTo (first, LS_t next) -> Some { first; next; min_len = 1 }
    | _ -> None
  in

  let do_abstraction (formula : Formula.t) (first_ls : Formula.ls) : Formula.t =
    match
      formula
      |> Formula.get_spatial_atom_from first_ls.next
      |> Option.map atom_to_ls |> Option.join
    with
    | Some second_ls
    (* conditions for abstraction *)
      when (* both spatial predicates (src|->middle and middle|->dst) must still be in formula *)
           List.mem (Formula.LS first_ls) formula
           (* middle must be fresh variable, and occur only in these two predicates *)
           && is_unique_fresh first_ls.next formula
           (* src must be different from dst (checked using solver) *)
           && Astral_query.check_inequality first_ls.first second_ls.next
                formula ->
        let min_length = add_min_lengths first_ls.min_len second_ls.min_len in
        formula
        |> Formula.remove_spatial_from first_ls.first
        |> Formula.remove_spatial_from second_ls.first
        |> Formula.add_atom
           @@ Formula.mk_ls first_ls.first second_ls.next min_length
    | _ -> formula
  in

  formula |> List.filter_map atom_to_ls |> List.fold_left do_abstraction formula

let convert_to_dls (formula : Formula.t) : Formula.t =
  let atom_to_dls : Formula.atom -> Formula.dls option = function
    | Formula.DLS dls -> Some dls
    | Formula.PointsTo (src, DLS_t (next, prev)) ->
        Some { first = src; last = src; next; prev; min_len = 1 }
    | _ -> None
  in

  let do_abstraction (formula : Formula.t) (first_dls : Formula.dls) : Formula.t
      =
    match
      formula
      |> Formula.get_spatial_atom_from first_dls.next
      |> Option.map atom_to_dls |> Option.join
    with
    | Some second_dls
    (* conditions for abstraction *)
      when (* both spatial predicates must still be in formula *)
           List.mem (Formula.DLS second_dls) formula
           (* middle vars must be fresh, and occur only in these two predicates *)
           && is_unique_fresh first_dls.last formula
           && is_unique_fresh second_dls.first formula
           (* `prev` pointer from second DLS must lead to end of first DLS *)
           && Formula.is_eq first_dls.last second_dls.prev formula
           (* DLS must not be cyclic (checked both forward and backward) *)
           && Astral_query.check_inequality first_dls.first second_dls.next
                formula
           && Astral_query.check_inequality second_dls.last first_dls.prev
                formula ->
        let min_length = add_min_lengths first_dls.min_len second_dls.min_len in
        formula
        |> Formula.remove_spatial_from first_dls.first
        |> Formula.remove_spatial_from second_dls.first
        |> Formula.add_atom
           @@ Formula.mk_dls first_dls.first second_dls.last first_dls.prev
                second_dls.next min_length
    | _ -> formula
  in

  formula
  |> List.filter_map atom_to_dls
  |> List.fold_left do_abstraction formula

let convert_to_nls (formula : Formula.t) : Formula.t =
  let atom_to_nls : Formula.atom -> Formula.nls option = function
    | Formula.NLS nls -> Some nls
    | Formula.PointsTo (first, NLS_t (top, next)) ->
        Some { first; top; next; min_len = 1 }
    | _ -> None
  in

  let do_abstraction (formula : Formula.t) (first_nls : Formula.nls) : Formula.t
      =
    match
      formula
      |> Formula.get_spatial_atom_from first_nls.top
      |> Option.map atom_to_nls |> Option.join
    with
    | Some second_nls
    (* conditions for abstraction *)
      when (* both spatial predicates (src|->middle and middle|->dst) must still be in formula *)
           List.mem (Formula.NLS first_nls) formula
           (* middle must be fresh variable, and occur only in these two predicates *)
           && is_unique_fresh first_nls.top formula
           (* src must be different from dst (checked using solver) *)
           && Astral_query.check_inequality first_nls.first second_nls.top
                (* common variable `next` must lead to the same target *)
                formula
           && Formula.is_eq first_nls.next second_nls.next formula ->
        let min_length = add_min_lengths first_nls.min_len second_nls.min_len in
        formula
        |> Formula.remove_spatial_from first_nls.first
        |> Formula.remove_spatial_from second_nls.first
        |> Formula.add_atom
           @@ Formula.mk_nls first_nls.first second_nls.top first_nls.next
                min_length
    | _ -> formula
  in

  formula
  |> List.filter_map atom_to_nls
  |> List.fold_left do_abstraction formula

module Tests = struct
  open Testing

  let%test "abstraction_ls_nothing" =
    let input = [ PointsTo (x, LS_t y'); PointsTo (y', LS_t z) ] in
    assert_eq (convert_to_ls input) input

  let%test "abstraction_ls_allocated_end" =
    let input =
      [ PointsTo (x, LS_t y'); PointsTo (y', LS_t z); Distinct (x, z) ]
    in
    let result = convert_to_ls input in
    let expected = [ mk_ls x z 2; Distinct (x, z) ] in
    assert_eq result expected

  (* let%test_unit "abstraction_ls_nil_end" = *)
  (*   let input = SSL.mk_star [ SSL.mk_pto x y'; SSL.mk_pto y' nil ] in *)
  (*   let result = convert_to_ls input in *)
  (*   let expected = SSL.mk_star [ SSL.mk_ls x nil; SSL.mk_distinct x nil ] in *)
  (*   assert_eq result expected *)
end
