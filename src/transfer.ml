open Astral
open Common
open Equiv_class

(** transfer function for `var = var;` *)
let assign (lhs : SSL.Variable.t) (rhs : SSL.Variable.t) (formula : SSL.t) :
    SSL.t =
  formula |> Formula.substitute_by_fresh lhs |> Formula.add_eq lhs rhs

(** transfer function for `var = var->field;` *)
let assign_rhs_field (lhs : SSL.Variable.t) (rhs : SSL.Variable.t)
    (rhs_field : Preprocessing.field_type) (formula : SSL.t) : SSL.t =
  let rhs_var = Formula.get_pto_target rhs rhs_field formula |> Option.get in
  formula |> Formula.substitute_by_fresh lhs |> Formula.add_eq lhs rhs_var

(** transfer function for `var->field = var;` *)
let assign_lhs_field (lhs : SSL.Variable.t)
    (lhs_field : Preprocessing.field_type) (rhs : SSL.Variable.t)
    (formula : SSL.t) : SSL.t =
  Formula.change_pto_target lhs lhs_field rhs formula

(** transfer function for function calls *)
let call (lhs_opt : SSL.Variable.t option) (func : Cil_types.varinfo)
    (params : SSL.Variable.t list) (formula : SSL.t) : state =
  let formula =
    match lhs_opt with
    | Some lhs -> Formula.substitute_by_fresh lhs formula
    | None -> formula
  in

  let lhs = Option.value lhs_opt ~default:(mk_fresh_var "leak") in
  let rhs = mk_fresh_var "alloc" in

  match (func.vname, params) with
  | "malloc", _ ->
      [
        Formula.add_atom (SSL.mk_pto (Var lhs) (Var rhs)) formula;
        Formula.add_eq lhs Formula.nil formula;
      ]
  | "__safe_malloc", _ ->
      (* malloc that always succeeds - for simpler experimenting *)
      [ Formula.add_atom (SSL.mk_pto (Var lhs) (Var rhs)) formula ]
  | "calloc", _ ->
      [
        Formula.add_atom (SSL.mk_pto (Var lhs) (SSL.mk_nil ())) formula;
        Formula.add_eq lhs Formula.nil formula;
      ]
  | "realloc", _ ->
      (* realloc changes only the size of allocation - no change in pointer structure *)
      [ formula ]
  | "free", [ var ] -> [ Formula.remove_pto_from var formula ]
  | _ -> fail "function calls are not implemented"

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

  (* let%test_unit "assign_lhs_deref" = *)
  (*   let input = SSL.mk_star [ SSL.mk_pto x z ] in *)
  (*   (* *x = y; *) *)
  (*   let result = assign_lhs_field x_var y_var input in *)
  (*   let expected = SSL.mk_star [ SSL.mk_pto x y ] in *)
  (*   assert_eq_list result [ expected ] *)
  (**)
  (* let%test_unit "assign_lhs_deref2" = *)
  (*   let input = SSL.mk_star [ SSL.mk_pto x z; SSL.mk_pto z z' ] in *)
  (*   (* *x = y; *) *)
  (*   let result = assign_lhs_field x_var y_var input in *)
  (*   let expected = SSL.mk_star [ SSL.mk_pto x y; SSL.mk_pto z z' ] in *)
  (*   assert_eq_list result [ expected ] *)

  (* let%test_unit "assign_rhs_deref" = *)
  (*   let input = SSL.mk_star [ SSL.mk_pto x z; SSL.mk_pto y y' ] in *)
  (*   (* x = *y; *) *)
  (*   let result = *)
  (*     List.map Simplifier.simplify (assign_rhs_field x_var y_var input) *)
  (*   in *)
  (*   let expected = *)
  (*     SSL.mk_star *)
  (*       [ SSL.mk_pto (mk_var "x!1") z; SSL.mk_pto y y'; SSL.mk_eq x y' ] *)
  (*   in *)
  (*   assert_eq_list result [ expected ] *)
end
