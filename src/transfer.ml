open Astral
open Common

type summary_input = Formula.var list * Cil_types.stmt

let function_summaries : (summary_input, Formula.state) Hashtbl.t ref =
  ref @@ Hashtbl.create 113

(** for running dataflow analysis recursively on other functions *)
let compute_function : (Cil_types.stmt list -> unit) option ref = ref None

(* state of dataflow analysis is stored here *)
let results : (Cil_types.stmt, Formula.state) Hashtbl.t ref =
  ref (Hashtbl.create 113)

(** transfer function for [var = var;] *)
let assign (lhs : SSL.Variable.t) (rhs : SSL.Variable.t) (formula : Formula.t) :
    Formula.t =
  (* assignment into "_const" is used to check if rhs is allocated *)
  if SSL.Variable.get_name lhs = Preprocessing.const_var_name then (
    Formula.assert_allocated rhs formula;
    formula)
  else
    let rhs =
      if SSL.Variable.get_name rhs = Preprocessing.null_var_name then
        Formula.nil
      else rhs
    in
    formula |> Formula.substitute_by_fresh lhs |> Formula.add_eq lhs rhs

(** transfer function for [var = var->field;] *)
let assign_rhs_field (lhs : SSL.Variable.t) (rhs : SSL.Variable.t)
    (rhs_field : Preprocessing.field_type) (formula : Formula.t) : Formula.t =
  Formula.assert_allocated rhs formula;
  let rhs_var =
    Formula.get_spatial_target rhs rhs_field formula |> Option.get
  in
  formula |> Formula.substitute_by_fresh lhs |> Formula.add_eq lhs rhs_var

(** transfer function for [var->field = var;] *)
let assign_lhs_field (lhs : SSL.Variable.t)
    (lhs_field : Preprocessing.field_type) (rhs : SSL.Variable.t)
    (formula : Formula.t) : Formula.t =
  Formula.change_pto_target lhs lhs_field rhs formula

(** transfer function for function calls *)
let call (lhs_opt : Formula.var option) (func : Cil_types.varinfo)
    (args : Formula.var list) (formula : Formula.t) : Formula.state =
  let get_allocation (init_vars_to_null : bool) : Formula.state =
    let lhs, pto =
      match lhs_opt with
      | Some lhs -> (
          let sort = SSL.Variable.get_sort lhs in
          let fresh () =
            if init_vars_to_null then Formula.nil
            else Common.mk_fresh_var_from lhs
          in
          ( lhs,
            match () with
            | _ when sort = Sort.loc_ls ->
                Formula.PointsTo (lhs, LS_t (fresh ()))
            | _ when sort = Sort.loc_dls ->
                Formula.PointsTo (lhs, DLS_t (fresh (), fresh ()))
            | _ when sort = Sort.loc_nls ->
                Formula.PointsTo (lhs, NLS_t (fresh (), fresh ()))
            | _ -> fail "unreachable transfer.ml:52" ))
      | None ->
          let lhs = SSL.Variable.mk_fresh "leak" Sort.loc_ls in
          (lhs, Formula.PointsTo (lhs, LS_t (Common.mk_fresh_var_from lhs)))
    in
    [
      formula |> Formula.substitute_by_fresh lhs |> Formula.add_atom pto;
      formula
      |> Formula.substitute_by_fresh lhs
      |> Formula.add_eq lhs Formula.nil;
    ]
  in

  match (func.vname, args) with
  | "malloc", _ -> get_allocation false
  | "calloc", _ -> get_allocation true
  | "realloc", var :: _ ->
      (* realloc changes the pointer value => all references to `var` are now dangling *)
      Formula.materialize var formula
      |> List.map (fun formula ->
             let spatial_atom = Formula.get_spatial_atom_from var formula in
             formula
             |> Formula.remove_spatial_from var
             |> Formula.substitute_by_fresh var
             |> Formula.add_atom spatial_atom)
  | "free", [ var ] ->
      formula |> Formula.materialize var
      |> List.map (Formula.remove_spatial_from var)
  | _, args ->
      let func = Globals.Functions.get func in
      let fundec = Kernel_function.get_definition func in
      let first_stmt = Kernel_function.find_first_stmt func in
      let return_stmt = Kernel_function.find_return func in

      let params = fundec.sformals |> List.map Preprocessing.varinfo_to_var in
      let locals =
        params @ (fundec.slocals |> List.map Preprocessing.varinfo_to_var)
      in

      (* add function's parameter names to formula *)
      let formula =
        List.fold_left2
          (fun formula var param -> Formula.add_eq var param formula)
          formula params args
      in

      Hashtbl.add !results first_stmt [ formula ];

      let compute_function = !compute_function |> Option.get in
      compute_function [ first_stmt ];

      let result_state = Hashtbl.find !results return_stmt in

      let result_state =
        match lhs_opt with
        | Some lhs ->
            let retres =
              SSL.Variable.mk "__retres" (SSL.Variable.get_sort lhs)
            in
            result_state
            |> List.map (Formula.substitute_by_fresh lhs)
            |> List.map (Formula.substitute ~var:retres ~by:lhs)
        | None -> result_state
      in

      List.map (Simplification.convert_vars_to_fresh locals) result_state

module Tests = struct
  open Testing

  (* let%test_unit "assign" = *)
  (*   let input = SSL.mk_star [ SSL.mk_pto x z; SSL.mk_pto y y' ] in *)
  (*   (* x = y; *) *)
  (*   let result = Simplifier.simplify @@ assign x_var y_var input in *)
  (*   let expected = *)
  (*     SSL.mk_star *)
  (*       [ SSL.mk_pto (mk_var "x!0") z; SSL.mk_pto y y'; SSL.mk_eq x y ] *)
  (*   in *)
  (*   assert_eq result expected *)

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
