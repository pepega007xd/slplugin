open Astral
open Common
open Equiv_class

let substitute_by_fresh (var : SSL.Variable.t) (formula : SSL.t) : SSL.t =
  let name, _ = var in
  SSL.substitute formula ~var ~by:(mk_fresh_var name)

(* transfer function for `a = b;` *)
let assign (lhs : SSL.Variable.t) (rhs : SSL.Variable.t) (prev_state : SSL.t) :
    SSL.t =
  SSL.mk_star
    [ SSL.mk_eq (Var lhs) (Var rhs); substitute_by_fresh lhs prev_state ]

(* transfer function for `a = *b;` *)
let assign_rhs_deref (lhs : SSL.Variable.t) (rhs : SSL.Variable.t)
    (prev_state : SSL.t) : SSL.t list =
  let formulas = extract_target prev_state rhs in
  List.map
    (fun state ->
      let _, dst, formula = state in
      SSL.mk_star
        [ SSL.mk_eq (Var lhs) (Var dst); substitute_by_fresh lhs formula ])
    formulas

(* transfer function for `*a = b;` *)
let assign_lhs_deref (lhs : SSL.Variable.t) (rhs : SSL.Variable.t)
    (prev_state : SSL.t) : SSL.t list =
  let formulas = extract_target prev_state lhs in
  List.map
    (fun state ->
      let src, dst, formula = state in
      let atoms = get_atoms formula in
      List.map
        (fun atom ->
          if atom = SSL.mk_pto (SSL.Var src) (SSL.Var dst) then
            SSL.mk_pto (Var src) (Var rhs)
          else atom)
        atoms
      |> SSL.mk_star)
    formulas

let transfer_function_free (formula : SSL.t) (var : SSL.Variable.t) : SSL.t list
    =
  let targets = extract_target formula var in
  List.map
    (fun target ->
      let lhs, rhs, formula = target in
      let atoms = get_atoms formula in
      let pto_to_remove = SSL.mk_pto (SSL.Var lhs) (SSL.Var rhs) in
      let output = List.filter (( <> ) pto_to_remove) atoms |> SSL.mk_star in
      output)
    targets

(* transfer function for function calls *)
let call (lhs : SSL.Variable.t option) (func : Cil_types.varinfo)
    (params : Cil_types.exp list) (formula : SSL.t) : SSL.t list =
  let open Cil_types in
  let formula =
    match lhs with
    | Some lhs -> substitute_by_fresh lhs formula
    | None -> formula
  in

  match (func.vname, params) with
  | "malloc", _ ->
      let lhs = Option.value lhs ~default:(mk_fresh_var "leak") in
      [
        SSL.mk_star
          [ SSL.mk_pto (Var lhs) @@ Var (mk_fresh_var "alloc"); formula ];
        SSL.mk_star [ SSL.mk_eq (Var lhs) @@ SSL.mk_nil (); formula ];
      ]
  | "__safe_malloc", _ ->
      (* malloc that always succeeds - for simpler experimenting *)
      let lhs = Option.value lhs ~default:(mk_fresh_var "leak") in
      [
        SSL.mk_star
          [ SSL.mk_pto (Var lhs) @@ Var (mk_fresh_var "alloc"); formula ];
      ]
  | "free", [ first ] -> (
      match first.enode with
      | Lval (Var ptr, NoOffset) ->
          transfer_function_free formula (Common.var_of_varinfo ptr)
      | _ -> fail "invalid argument of free")
  | _ -> fail "call: function calls are not implemented"

module Tests = struct
  open Testing

  let%test_unit "assign" =
    let input = SSL.mk_star [ SSL.mk_pto x z; SSL.mk_pto y y' ] in
    (* x = y; *)
    let result = Simplifier.simplify @@ assign x_var y_var input in
    let expected =
      SSL.mk_star
        [ SSL.mk_pto (mk_var "x!0") z; SSL.mk_pto y y'; SSL.mk_eq x y ]
    in
    assert_eq result expected

  let%test_unit "assign_lhs_deref" =
    let input = SSL.mk_star [ SSL.mk_pto x z ] in
    (* *x = y; *)
    let result = assign_lhs_deref x_var y_var input in
    let expected = SSL.mk_star [ SSL.mk_pto x y ] in
    assert_eq_list result [ expected ]

  let%test_unit "assign_lhs_deref2" =
    let input = SSL.mk_star [ SSL.mk_pto x z; SSL.mk_pto z z' ] in
    (* *x = y; *)
    let result = assign_lhs_deref x_var y_var input in
    let expected = SSL.mk_star [ SSL.mk_pto x y; SSL.mk_pto z z' ] in
    assert_eq_list result [ expected ]

  let%test_unit "assign_rhs_deref" =
    let input = SSL.mk_star [ SSL.mk_pto x z; SSL.mk_pto y y' ] in
    (* x = *y; *)
    let result =
      List.map Simplifier.simplify (assign_rhs_deref x_var y_var input)
    in
    let expected =
      SSL.mk_star
        [ SSL.mk_pto (mk_var "x!1") z; SSL.mk_pto y y'; SSL.mk_eq x y' ]
    in
    assert_eq_list result [ expected ]
end
