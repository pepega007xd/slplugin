open Astral
open Common

type var = SSL.Variable.t

let substitute_by_fresh (var : var) (f : SSL.t) : SSL.t =
  let (_, name), _ = var in
  SSL.substitute f ~var ~by:(mk_fresh_var name)

let add_eq (lhs : var) (rhs : var) (f : SSL.t) : SSL.t = fail "TODO"
let add_distinct (lhs : var) (rhs : var) (f : SSL.t) : SSL.t = fail "TODO"

let get_pto_target (var : var) (field : Preprocessing.field_type) (f : SSL.t) :
    var option =
  fail "TODO"

let change_pto_target (var : var) (field : Preprocessing.field_type)
    (new_target : var) (f : SSL.t) : SSL.t =
  fail "TODO"

let remove_pto_from (var : var) (f : SSL.t) : SSL.t = fail "TODO"
let add_atom (atom : SSL.t) (f : SSL.t) : SSL.t = fail "TODO"
let nil = SSL.Variable.nil

let convert_vars_to_fresh (var_names : string list) (formula : SSL.t) : SSL.t =
  List.fold_left
    (fun formula var_name ->
      SSL.substitute formula
        ~var:(SSL.Variable.mk var_name Sort.loc_ls)
        ~by:(mk_fresh_var var_name))
    formula var_names

let rec get_atoms (f : SSL.t) : SSL.t list =
  match f with
  | SSL.Emp -> []
  | SSL.Eq _ | SSL.Distinct _ | SSL.PointsTo _ | SSL.LS _ -> [ f ]
  | SSL.Star atoms -> atoms
  | SSL.Or (lhs, rhs) -> get_atoms lhs @ get_atoms rhs
  | _ -> fail "invalid shape of formula"

(** returns true when variable appears only once in the formula, `Distinct` atoms are ignored *)
let count_occurences_excl_distinct (var : var) (f : SSL.t) : int =
  f |> get_atoms
  |> List.filter (function SSL.Distinct _ -> false | _ -> true)
  |> List.concat_map SSL.get_vars
  |> Common.list_count var

let get_equiv_classes (f : SSL.t) : var list list = fail "TODO"

let map_equiv_classes (fn : var list -> var list) (f : SSL.t) : SSL.t =
  (* Formula.get_atoms *)
  (*   |> List.map (function *)
  (*        | SSL.Eq equiv_class -> SSL.Eq (reduce_class equiv_class) *)
  (*        | other -> other) *)
  (*   |> SSL.mk_star *)
  fail "TODO"
