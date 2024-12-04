open Common

let is_unique_fresh (var : Formula.var) (formula : Formula.t) : bool =
  is_fresh_var var && Formula.count_occurences_excl_distinct var formula = 2

let is_in_formula (src : Formula.var) (dst : Formula.var)
    (field : Preprocessing.field_type) (formula : Formula.t) : bool =
  Formula.get_spatial_atom_from_opt src formula
  |> Option.map (fun atom ->
         Formula.get_target_of_atom field atom |> fun atom_dst ->
         Formula.is_eq atom_dst dst formula)
  |> Option.value ~default:false

let convert_to_ls (formula : Formula.t) : Formula.t =
  let atom_to_ls (atom : Formula.atom) : Formula.ls option =
    atom |> Formula.pto_to_list |> function
    | Formula.LS ls -> Some ls
    | _ -> None
  in

  let do_abstraction (formula : Formula.t) (first_ls : Formula.ls) : Formula.t =
    match
      formula
      |> Formula.get_spatial_atom_from_opt first_ls.next
      |> Option.map atom_to_ls |> Option.join
    with
    | Some second_ls
    (* conditions for abstraction *)
      when (* first_ls must still be in formula *)
           is_in_formula first_ls.first first_ls.next Preprocessing.Next formula
           (* middle must be fresh variable, and occur only in these two predicates *)
           && is_unique_fresh first_ls.next formula
           (* src must be different from dst (checked using solver) *)
           && Astral_query.check_inequality first_ls.first second_ls.next
                formula ->
        let min_length = min 2 (first_ls.min_len + second_ls.min_len) in
        formula
        |> Formula.remove_spatial_from first_ls.first
        |> Formula.remove_spatial_from second_ls.first
        |> Formula.add_atom
           @@ Formula.mk_ls first_ls.first second_ls.next min_length
    | _ -> formula
  in

  formula |> List.filter_map atom_to_ls |> List.fold_left do_abstraction formula

let convert_to_dls (formula : Formula.t) : Formula.t =
  (* Formula.show_formula formula; *)
  let atom_to_dls (atom : Formula.atom) : Formula.dls option =
    atom |> Formula.pto_to_list |> function
    | Formula.DLS dls -> Some dls
    | _ -> None
  in

  (* unlike ls and nls, dls contains each variable 3 times *)
  let is_unique_fresh (var : Formula.var) (formula : Formula.t) : bool =
    is_fresh_var var && Formula.count_occurences_excl_distinct var formula = 3
  in

  let do_abstraction (formula : Formula.t) (first_dls : Formula.dls) : Formula.t
      =
    let second_dls =
      formula
      |> Formula.get_spatial_atom_from_opt first_dls.next
      |> Option.map atom_to_dls |> Option.join
    in

    let third_dls =
      Option.map
        (fun (second_dls : Formula.dls) ->
          formula
          |> Formula.get_spatial_atom_from_opt second_dls.next
          |> Option.map atom_to_dls)
        second_dls
      |> Option.join |> Option.join
    in

    match (second_dls, third_dls) with
    | Some second_dls, Some third_dls
    (* conditions for abstraction *)
      when (* first_dls must still be in formula *)
           is_in_formula first_dls.first first_dls.next Preprocessing.Next
             formula
           (* middle vars must be fresh, and occur only in these two predicates *)
           && is_unique_fresh second_dls.first formula
           && is_unique_fresh second_dls.last formula
           (* `prev` pointers from second and third DLS must lead to end of the previous DLS *)
           && Formula.is_eq first_dls.last second_dls.prev formula
           && Formula.is_eq second_dls.last third_dls.prev formula
           (* DLS must not be cyclic (checked both forward and backward) *)
           && Astral_query.check_inequality first_dls.first third_dls.next
                formula
           && Astral_query.check_inequality third_dls.last first_dls.prev
                formula ->
        let min_length =
          min 3 (first_dls.min_len + second_dls.min_len + third_dls.min_len)
        in
        formula
        |> Formula.remove_spatial_from first_dls.first
        |> Formula.remove_spatial_from second_dls.first
        |> Formula.remove_spatial_from third_dls.first
        |> Formula.add_atom
           @@ Formula.mk_dls first_dls.first third_dls.last first_dls.prev
                third_dls.next min_length
    | _ -> formula
  in

  formula
  |> List.filter_map atom_to_dls
  |> List.fold_left do_abstraction formula

let convert_to_nls (formula : Formula.t) : Formula.t =
  let atom_to_nls (atom : Formula.atom) : Formula.nls option =
    atom |> Formula.pto_to_list |> function
    | Formula.NLS nls -> Some nls
    | _ -> None
  in

  (* returns modified formula and new [next] var if join succeeded *)
  let join_sublists (lhs : Formula.var) (rhs : Formula.var)
      (formula : Formula.t) : (Formula.t * Formula.var) option =
    let lhs_target =
      Formula.get_spatial_target lhs Preprocessing.Next formula
    in
    let rhs_target =
      Formula.get_spatial_target rhs Preprocessing.Next formula
    in
    match (lhs, rhs, lhs_target, rhs_target) with
    | lhs, rhs, _, _ when Formula.is_eq lhs rhs formula -> Some (formula, lhs)
    | lhs, rhs, _, Some rhs_t
      when Formula.is_eq lhs rhs_t formula && is_unique_fresh rhs formula ->
        Some (formula |> Formula.remove_spatial_from rhs, lhs)
    | lhs, rhs, Some lhs_t, _
      when Formula.is_eq lhs_t rhs formula && is_unique_fresh lhs formula ->
        Some (formula |> Formula.remove_spatial_from lhs, lhs_t)
    | lhs, rhs, Some lhs_t, Some rhs_t
      when Formula.is_eq lhs_t rhs_t formula
           && is_unique_fresh lhs formula
           && is_unique_fresh rhs formula ->
        Some
          ( formula
            |> Formula.remove_spatial_from lhs
            |> Formula.remove_spatial_from rhs,
            lhs_t )
    | _ -> None
  in

  let do_abstraction (formula : Formula.t) (first_nls : Formula.nls) : Formula.t
      =
    match
      formula
      |> Formula.get_spatial_atom_from_opt first_nls.top
      |> Option.map atom_to_nls |> Option.join
    with
    | Some second_nls
    (* conditions for abstraction *)
      when (* first_nls must still be in formula *)
           is_in_formula first_nls.first first_nls.top Preprocessing.Top formula
           (* middle must be fresh variable, and occur only in these two predicates *)
           && is_unique_fresh first_nls.top formula
           (* src must be different from dst (checked using solver) *)
           && Astral_query.check_inequality first_nls.first second_nls.top
                formula
           (* common variable `next` must lead to the same target *)
           (*TODO: *) -> (
        match join_sublists first_nls.next second_nls.next formula with
        | Some (formula, next) ->
            let min_length = min 2 (first_nls.min_len + second_nls.min_len) in
            formula
            |> Formula.remove_spatial_from first_nls.first
            |> Formula.remove_spatial_from second_nls.first
            |> Formula.add_atom
               @@ Formula.mk_nls first_nls.first second_nls.top next min_length
        | None -> formula)
    | _ -> formula
  in

  formula
  |> List.filter_map atom_to_nls
  |> List.fold_left do_abstraction formula

module Tests = struct
  open Testing

  (* DLS abstraction *)

  let%test "abstraction_ls_nothing" =
    let input = [ PointsTo (x, LS_t y'); PointsTo (y', LS_t z) ] in
    assert_eq (convert_to_ls input) input

  let%test "abstraction_ls_1" =
    let input =
      [ PointsTo (x, LS_t y'); PointsTo (y', LS_t z); Distinct (x, z) ]
    in
    let result = convert_to_ls input in
    let expected = [ mk_ls x z 2; Distinct (x, z) ] in
    assert_eq result expected

  let%test "abstraction_ls_1_nil" =
    let input = [ PointsTo (x, LS_t y'); PointsTo (y', LS_t nil) ] in
    let result = convert_to_ls input in
    let expected = [ mk_ls x nil 2 ] in
    assert_eq result expected

  let%test "abstraction_ls_2" =
    let input =
      [
        PointsTo (x, LS_t y');
        PointsTo (y', LS_t z);
        PointsTo (u, LS_t v');
        PointsTo (v', LS_t w);
        Distinct (u, w);
      ]
    in
    let result = convert_to_ls input in
    let expected =
      [
        PointsTo (x, LS_t y');
        PointsTo (y', LS_t z);
        mk_ls u w 2;
        Distinct (u, w);
      ]
    in
    assert_eq result expected

  let%test "abstraction_ls_3" =
    let input =
      [
        PointsTo (x, LS_t y');
        PointsTo (y', LS_t z');
        PointsTo (z', LS_t w);
        Distinct (x, w);
      ]
    in
    let result = convert_to_ls input in
    let expected = [ mk_ls x z' 2; PointsTo (z', LS_t w); Distinct (x, w) ] in
    assert_eq result expected

  let%test "abstraction_ls_double" =
    let input =
      [
        PointsTo (x, LS_t y');
        PointsTo (y', LS_t z');
        PointsTo (z', LS_t w);
        Distinct (x, w);
      ]
    in
    let result = convert_to_ls @@ convert_to_ls input in
    let expected = [ mk_ls x w 2; Distinct (x, w) ] in
    assert_eq result expected

  (* DLS abstraction *)

  let%test "abstraction_dls_nothing" =
    let input =
      [
        PointsTo (u, DLS_t (v', z));
        PointsTo (v', DLS_t (w, u));
        PointsTo (w, DLS_t (x, v'));
        Distinct (u, x);
      ]
    in
    assert_eq (convert_to_dls input) input

  let%test "abstraction_dls_1" =
    let input =
      [
        PointsTo (u, DLS_t (v', z));
        PointsTo (v', DLS_t (w, u));
        PointsTo (w, DLS_t (x, v'));
        Distinct (u, x);
        Distinct (w, z);
      ]
    in
    let expected = [ mk_dls u w z x 3; Distinct (u, x); Distinct (w, z) ] in
    assert_eq (convert_to_dls input) expected

  let%test "abstraction_dls_2" =
    let input =
      [
        PointsTo (u, DLS_t (v', z));
        PointsTo (v', DLS_t (w, u));
        PointsTo (w, DLS_t (x, v'));
        PointsTo (x, DLS_t (y, w'));
        Distinct (w, z);
      ]
    in
    let expected =
      [ mk_dls u w z x 3; PointsTo (x, DLS_t (y, w')); Distinct (w, z) ]
    in
    assert_eq (convert_to_dls input) expected

  let%test "abstraction_dls_long_nothing" =
    let input =
      [
        PointsTo (u, DLS_t (v, z));
        PointsTo (v, DLS_t (w, u));
        PointsTo (w, DLS_t (x, v));
        PointsTo (x, DLS_t (y, w));
        PointsTo (y, DLS_t (z, x));
      ]
    in
    assert_eq (convert_to_dls input) input

  let%test "abstraction_dls_long_1" =
    let input =
      [
        PointsTo (u, DLS_t (v, z));
        PointsTo (v, DLS_t (w', u));
        PointsTo (w', DLS_t (x, v));
        PointsTo (x, DLS_t (y, w'));
        PointsTo (y, DLS_t (z, x));
      ]
    in
    let expected =
      [
        PointsTo (u, DLS_t (v, z)); mk_dls v x u y 3; PointsTo (y, DLS_t (z, x));
      ]
    in
    assert_eq (convert_to_dls input) expected

  (* NLS abstraction *)

  let%test "abstraction_nls_nothing" =
    let input =
      [ PointsTo (x, NLS_t (y', nil)); PointsTo (y', NLS_t (z, nil)) ]
    in
    assert_eq (convert_to_nls input) input

  let%test "abstraction_nls_nothing_2" =
    let input =
      [
        PointsTo (x, NLS_t (y', nil));
        PointsTo (y', NLS_t (z, w));
        Distinct (x, z);
      ]
    in
    let result = convert_to_nls input in
    assert_eq result input

  let%test "abstraction_nls_1" =
    let input =
      [
        PointsTo (x, NLS_t (y', nil));
        PointsTo (y', NLS_t (z, nil));
        Distinct (x, z);
      ]
    in
    let result = convert_to_nls input in
    let expected = [ mk_nls x z nil 2; Distinct (x, z) ] in
    assert_eq result expected

  let%test "abstraction_nls_1_nil" =
    let input =
      [ PointsTo (x, NLS_t (y', nil)); PointsTo (y', NLS_t (nil, nil)) ]
    in
    let result = convert_to_nls input in
    let expected = [ mk_nls x nil nil 2 ] in
    assert_eq result expected

  let%test "abstraction_nls_2" =
    let input =
      [
        PointsTo (x, NLS_t (y', nil));
        PointsTo (y', NLS_t (z, nil));
        PointsTo (u, NLS_t (v', nil));
        PointsTo (v', NLS_t (w, nil));
        Distinct (u, w);
      ]
    in
    let result = convert_to_nls input in
    let expected =
      [
        PointsTo (x, NLS_t (y', nil));
        PointsTo (y', NLS_t (z, nil));
        mk_nls u w nil 2;
        Distinct (u, w);
      ]
    in
    assert_eq result expected

  let%test "abstraction_nls_3" =
    let input =
      [
        PointsTo (x, NLS_t (y', nil));
        PointsTo (y', NLS_t (z', nil));
        PointsTo (z', NLS_t (w, nil));
        Distinct (x, w);
      ]
    in
    let result = convert_to_nls input in
    let expected =
      [ mk_nls x z' nil 2; PointsTo (z', NLS_t (w, nil)); Distinct (x, w) ]
    in
    assert_eq result expected

  let%test "abstraction_nls_double" =
    let input =
      [
        PointsTo (x, NLS_t (y', nil));
        PointsTo (y', NLS_t (z', nil));
        PointsTo (z', NLS_t (w, nil));
        Distinct (x, w);
      ]
    in
    let result = convert_to_nls @@ convert_to_nls input in
    let expected = [ mk_nls x w nil 2; Distinct (x, w) ] in
    assert_eq result expected

  let%test "abstraction_nls_with_ls_0" =
    let input =
      [
        PointsTo (x, NLS_t (y', z'));
        PointsTo (y', NLS_t (u, v'));
        Distinct (x, u);
        LS { first = z'; next = nil; min_len = 0 };
        LS { first = v'; next = nil; min_len = 0 };
      ]
    in
    let result = convert_to_nls input in
    let expected = [ mk_nls x u nil 2; Distinct (x, u) ] in
    assert_eq result expected

  let%test "abstraction_nls_with_ls_different_lengths" =
    let input =
      [
        PointsTo (x, NLS_t (y', z'));
        PointsTo (y', NLS_t (u, v'));
        Distinct (x, u);
        LS { first = z'; next = nil; min_len = 1 };
        LS { first = v'; next = nil; min_len = 0 };
      ]
    in
    let result = convert_to_nls input in
    let expected = [ mk_nls x u nil 2; Distinct (x, u) ] in
    assert_eq result expected

  let%test "abstraction_nls_with_ls_different_lengths_2" =
    let input =
      [
        PointsTo (x, NLS_t (y', z'));
        PointsTo (y', NLS_t (nil, v'));
        LS { first = z'; next = nil; min_len = 1 };
        LS { first = v'; next = nil; min_len = 2 };
      ]
    in
    let result = convert_to_nls input in
    let expected = [ mk_nls x nil nil 2 ] in
    assert_eq result expected
end
