open Astral

type sort = Ls | Dls | Nls | Nil
type var = { name : string; logic_var : bool; sort : sort }

(* fields called `var`, `first`, `last` are allocated (inside the list),
   fields `prev`, `next`, `top`, `top_next` are outside of the lists *)

(* singly linked list *)
type ls_pto = { var : var; next : var }
type ls = { first : var; next : var }

(* doubly linked list *)
type dls_pto = { var : var; prev : var; next : var }
type dls = { first : var; last : var; prev : var; next : var }

let a (b : 'a) = b.next

(* nested list *)
type nls_pto = { var : var; top : var; next : var }
type nls = { first : var; top_next : var; common : var }

type formula = {
  equivalence_classes : var list list;
  inequalities : (var * var) list;
  ls_ptos : ls_pto list;
  ls_segments : ls list;
  dls_ptos : dls_pto list;
  dls_segments : dls list;
  nls_ptos : nls_pto list;
  nls_segments : nls list;
}

type state = formula list

let nil : var = { name = "nil"; logic_var = false; sort = Nil }

let to_astral (formula : formula) : SSL.t =
  let astral_sort (sort : sort) : Sort.t =
    match sort with
    | Ls -> Sort.loc_ls
    | Dls -> Sort.loc_dls
    | Nls -> Sort.loc_nls
    | Nil -> Sort.loc_nil
  in

  let convert_var (var : var) : SSL.t =
    SSL.mk_var var.name (astral_sort var.sort)
  in

  SSL.mk_star
    (* equivalence classes *)
    (List.map
       (fun vars -> SSL.mk_eq_list @@ List.map convert_var vars)
       formula.equivalence_classes
    (* inequalities *)
    @ List.map
        (fun (lhs, rhs) -> SSL.mk_distinct (convert_var lhs) (convert_var rhs))
        formula.inequalities
    (* ls pto *)
    @ List.map
        (fun (ls_pto : ls_pto) ->
          SSL.mk_pto (convert_var ls_pto.var) (convert_var ls_pto.next))
        formula.ls_ptos
    (* ls *)
    @ List.map
        (fun (ls : ls) ->
          SSL.mk_ls (convert_var ls.first) (convert_var ls.next))
        formula.ls_segments
    (* dls pto *)
    @ List.map
        (fun (dls_pto : dls_pto) ->
          SSL.mk_pto_dls (convert_var dls_pto.var) (convert_var dls_pto.next)
            (convert_var dls_pto.prev))
        formula.dls_ptos
    (* dls *)
    @ List.map
        (fun (dls : dls) ->
          SSL.mk_dls
            (* TODO is this the correct order? *)
            (convert_var dls.first)
            (convert_var dls.last) (convert_var dls.prev) (convert_var dls.next))
        formula.dls_segments
    (* nls pto *)
    @ List.map
        (fun (nls_pto : nls_pto) ->
          SSL.mk_pto_nls (convert_var nls_pto.var) (convert_var nls_pto.next)
            (convert_var nls_pto.next))
        formula.nls_ptos
    (* nls *)
    @ List.map
        (fun (nls : nls) ->
          SSL.mk_nls (convert_var nls.first) (convert_var nls.top_next)
            (convert_var nls.common))
        formula.nls_segments)

(* transfer function for `a = b->next;` *)
(* let assign_rhs_deref_new (lhs : var) (rhs : var) (prev_state : state) : state = *)
(*   let state = *)
(*     state |> flat_map materialize rhs |> map substitute_by_fresh lhs *)
(*   in *)
(*   { state with ls_ptos = sth :: state.ls_ptos } *)
