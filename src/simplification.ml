open Astral
open Common
open Printing
open Equiv_class
open Slplugin_options

let extract_ptr (ptr : SSL.t) : SSL.Variable.t * SSL.Variable.t =
  match ptr with
  | PointsTo (Var src, LS_t dst) -> (src, dst)
  | LS (Var src, Var dst) -> (src, dst)
  | _ -> fail "unreachable"

let extract_vars_nondistinct (atoms : SSL.t list) : SSL.Variable.t list =
  let atoms_nondistinct =
    List.filter
      (fun atom -> match atom with SSL.Distinct _ -> false | _ -> true)
      atoms
  in
  extract_vars atoms_nondistinct

let convert_to_ls (formula : SSL.t) : SSL.t =
  let atoms = get_atoms formula in
  (* LS always has a (start != end) atom with it, we
     don't want to count this second occurence the variables *)
  let vars_nodistinct = extract_vars_nondistinct atoms in
  let is_unique_fresh var =
    is_fresh_var var && list_count vars_nodistinct var = 2
  in
  let ptrs_to_fresh, rest =
    List.partition
      (fun (atom : SSL.t) ->
        match atom with
        | PointsTo (_, LS_t dst) when is_unique_fresh dst -> true
        | LS (_, Var dst) when is_unique_fresh dst -> true
        | _ -> false)
      atoms
  in
  let ptrs_from_fresh, rest =
    List.partition
      (fun (atom : SSL.t) ->
        match atom with
        | PointsTo (Var src, _) when is_unique_fresh src -> true
        | LS (Var src, _) when is_unique_fresh src -> true
        | _ -> false)
      rest
  in
  let ptrs_from_fresh = Array.of_list ptrs_from_fresh in
  let ptrs_with_ls =
    List.map
      (fun ptr_to_fresh ->
        let ls_src, dst = extract_ptr ptr_to_fresh in
        let index_opt =
          Array.find_index
            (fun ptr_from_fresh ->
              let src, _ = extract_ptr ptr_from_fresh in
              dst = src)
            ptrs_from_fresh
        in
        match index_opt with
        | Some index ->
            let _, ls_dst = extract_ptr @@ ptrs_from_fresh.(index) in
            if
              check_sat
              @@ SSL.mk_star [ formula; SSL.mk_eq (Var ls_src) (Var ls_dst) ]
            then (* cannot make ls *) ptr_to_fresh
            else (
              ptrs_from_fresh.(index) <- SSL.mk_emp ();
              SSL.mk_star
                [
                  SSL.mk_ls (Var ls_src) (Var ls_dst);
                  SSL.mk_distinct (Var ls_src) (Var ls_dst);
                ])
        | None -> ptr_to_fresh)
      ptrs_to_fresh
  in
  let remaining_ptrs =
    Array.to_list ptrs_from_fresh
    |> List.filter (fun atom -> match atom with SSL.Emp -> false | _ -> true)
  in
  (* flatten SSL.Star *)
  SSL.mk_star (ptrs_with_ls @ remaining_ptrs @ rest) |> Simplifier.simplify

let substitute_all_fresh (formula : SSL.t) (to_remove : SSL.Variable.t list)
    (replacement : SSL.Variable.t) : SSL.t =
  List.fold_left
    (fun formula to_remove ->
      if is_fresh_var to_remove then
        SSL.substitute formula ~var:to_remove ~by:replacement
      else formula)
    formula to_remove

let remove_identities (atoms : SSL.t list) : SSL.t list =
  List.filter
    (fun atom ->
      match atom with
      | SSL.Eq [ Var lhs; Var rhs ] -> not (lhs = rhs)
      | _ -> true)
    atoms

let reduce_equiv_classes (formula : SSL.t) : SSL.t =
  let atoms = get_atoms formula in
  let vars = extract_vars atoms in
  let fresh_vars = List.filter is_fresh_var vars in

  let reduced =
    List.fold_left
      (fun formula fresh_var ->
        let atoms = get_atoms formula in
        let equiv_class = Equiv_class.mk_equiv_class_rec atoms [ fresh_var ] in
        match
          List.find_opt (fun var -> not @@ is_fresh_var var) equiv_class
        with
        | Some program_var ->
            substitute_all_fresh formula equiv_class program_var
        | None -> substitute_all_fresh formula equiv_class fresh_var)
      formula fresh_vars
  in
  let reduced_atoms = get_atoms reduced in
  SSL.mk_star @@ remove_identities reduced_atoms

let remove_distinct_only (formula : SSL.t) : SSL.t =
  let atoms = get_atoms formula in
  let distinct_only (var : SSL.Variable.t) : bool =
    List.for_all
      (fun atom ->
        match atom with
        | SSL.Distinct _ -> true
        | _ when list_contains (extract_vars [ atom ]) var -> false
        | _ -> true)
      atoms
  in
  List.filter
    (fun atom ->
      match atom with
      | SSL.Distinct [ Var lhs; _ ] when is_fresh_var lhs && distinct_only lhs
        ->
          false
      | SSL.Distinct [ _; Var rhs ] when is_fresh_var rhs && distinct_only rhs
        ->
          false
      | _ -> true)
    atoms
  |> SSL.mk_star

(* removes all atoms of the form (x' -> y), where x' doesn't appear anywhere else *)
let remove_junk (formula : SSL.t) : SSL.t =
  let atoms = get_atoms formula in
  let vars = extract_vars atoms in
  let vars_nodistinct = extract_vars_nondistinct atoms in
  let valid_atoms, junk_atoms =
    List.partition
      (fun atom ->
        match atom with
        | SSL.Eq [ Var lhs; Var rhs ] | SSL.Distinct [ Var lhs; Var rhs ] ->
            (* filters out atoms (fresh = x) and (fresh != x), where fresh occurs only once in the formula *)
            not
              ((list_count vars lhs = 1 && is_fresh_var lhs)
              || (list_count vars rhs = 1 && is_fresh_var rhs))
        | SSL.PointsTo (Var src, LS_t dst) ->
            (* filters out atoms (fresh -> x), where fresh occurs only once in formula *)
            let is_alone =
              is_fresh_var src && list_count vars_nodistinct src = 1
            in

            (* filters out atoms (fresh1 -> fresh2) and (fresh2 -> fresh1), if this is their only
               occurence in the formula *)
            let is_cycle =
              is_fresh_var src && is_fresh_var dst
              && list_contains atoms (SSL.mk_pto (Var dst) (Var src))
            in
            (not @@ is_alone) || is_cycle
        | SSL.LS (Var src, Var _) ->
            (* filters out atoms ls(fresh, x), where fresh occurs only once in formula *)
            let is_alone =
              is_fresh_var src && list_count vars_nodistinct src = 1
            in

            not @@ is_alone
        | _ -> true)
      atoms
  in
  if List.length junk_atoms = 0 then formula
  else (
    if Debug_output.get () then (
      print_warn "removing junk:";
      print_state junk_atoms);

    SSL.Star valid_atoms)

let remove_nil_vars (formula : SSL.t) : SSL.t =
  let atoms = get_atoms formula in
  let nil = SSL.Variable.nil in
  let nil_fresh_vars =
    List.filter_map
      (fun atom ->
        match atom with
        | SSL.Eq [ Var a; Var b ] when is_fresh_var a && b = nil -> Some a
        | SSL.Eq [ Var a; Var b ] when is_fresh_var b && a = nil -> Some b
        | _ -> None)
      atoms
    |> List.map (fun var -> mk_equiv_class_rec atoms [ var ])
    |> List.flatten
  in

  List.fold_left
    (fun formula var -> SSL.substitute ~var ~by:SSL.Variable.nil formula)
    formula nil_fresh_vars

let convert_vars_to_fresh (var_names : string list) (formula : SSL.t) : SSL.t =
  List.fold_left
    (fun formula var_name ->
      SSL.substitute formula
        ~var:(SSL.Variable.mk var_name Sort.loc_ls)
        ~by:(mk_fresh_var var_name))
    formula var_names

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
