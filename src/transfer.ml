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
  let is_alloc fn_name =
    fn_name = "malloc" || fn_name = "calloc" || fn_name = "realloc"
  in
  if is_alloc func_name then
    [
      SSL.mk_star
        [ SSL.mk_pto (Var lhs) @@ Var (mk_fresh_var "alloc"); formula ];
      SSL.mk_star [ SSL.mk_eq (Var lhs) @@ SSL.mk_nil (); formula ];
    ]
  else fail "mk_call: function calls are not implemented"
