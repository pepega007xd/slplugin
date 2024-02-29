open Astral
open Common
open Equiv_class

let mk_fresh_var (basename : string) : SSL.Variable.t =
  SSL.Variable.mk_fresh basename Sort.loc_ls

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
    (prev_state : SSL.t) : SSL.t =
  let _, dst = Option.get @@ find_pto prev_state rhs in
  SSL.mk_star
    [ SSL.mk_eq (Var lhs) (Var dst); substitute_by_fresh lhs prev_state ]

(* transfer function for `*a = b;` *)
let assign_lhs_deref (lhs : SSL.Variable.t) (rhs : SSL.Variable.t)
    (prev_state : SSL.t) : SSL.t =
  let src, dst = Option.get @@ find_pto prev_state lhs in
  let atoms = get_atoms prev_state in
  List.map
    (fun atom ->
      if atom = SSL.mk_pto (SSL.Var src) (SSL.Var dst) then
        SSL.mk_pto (Var src) (Var rhs)
      else atom)
    atoms
  |> SSL.mk_star

(* transfer function for `a = malloc(...)` *)
let call (lhs : SSL.Variable.t) (func_name : string) (formula : SSL.t) :
    SSL.t list =
  let formula = substitute_by_fresh lhs formula in
  (*TODO implement free *)
  match func_name with
  | "malloc" ->
      [
        SSL.mk_star
          [ SSL.mk_pto (Var lhs) @@ Var (mk_fresh_var "alloc"); formula ];
        SSL.mk_star [ SSL.mk_eq (Var lhs) @@ SSL.mk_nil (); formula ];
      ]
  | "__safe_malloc" ->
      (* malloc that always succeeds - for simpler experimenting *)
      [
        SSL.mk_star
          [ SSL.mk_pto (Var lhs) @@ Var (mk_fresh_var "alloc"); formula ];
      ]
  | _ -> fail "mk_call: function calls are not implemented"

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
    assert_eq result expected

  let%test_unit "assign_lhs_deref2" =
    let input = SSL.mk_star [ SSL.mk_pto x z; SSL.mk_pto z z' ] in
    (* *x = y; *)
    let result = assign_lhs_deref x_var y_var input in
    let expected = SSL.mk_star [ SSL.mk_pto x y; SSL.mk_pto z z' ] in
    assert_eq result expected

  let%test_unit "assign_rhs_deref" =
    let input = SSL.mk_star [ SSL.mk_pto x z; SSL.mk_pto y y' ] in
    (* x = *y; *)
    let result = Simplifier.simplify @@ assign_rhs_deref x_var y_var input in
    let expected =
      SSL.mk_star
        [ SSL.mk_pto (mk_var "x!1") z; SSL.mk_pto y y'; SSL.mk_eq x y' ]
    in
    assert_eq result expected
end
