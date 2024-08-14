open Astral
open Common

type var = SSL.Variable.t

let substitute_by_fresh (var : var) (f : SSL.t) : SSL.t =
  let (_, name), _ = var in
  SSL.substitute f ~var ~by:(mk_fresh_var name)

let add_eq (lhs : var) (rhs : var) (f : SSL.t) : SSL.t = fail "TODO"

let get_pto_target (var : var) (field : Preprocessing.field_type) (f : SSL.t) :
    var =
  fail "TODO"

let change_pto_target (var : var) (field : Preprocessing.field_type)
    (new_target : var) (f : SSL.t) : SSL.t =
  fail "TODO"

let remove_pto_from (var : var) (f : SSL.t) : SSL.t = fail "TODO"
let add_atom (atom : SSL.t) (f : SSL.t) : SSL.t = fail "TODO"
let nil = SSL.Variable.nil
