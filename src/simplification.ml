open Astral
open Common
open Printing
open Equiv_class
open Slplugin_options

(* removes all atoms of the form (x' -> y), where x' doesn't appear anywhere else *)
let remove_junk (formula : SSL.t) : SSL.t =
  let atoms = get_atoms formula in
  let vars = extract_vars atoms in
  let valid_atoms, junk_atoms =
    List.partition
      (fun atom ->
        match atom with
        | SSL.Eq [ Var src; Var dst ] ->
            (* filters out atoms (fresh = x), where fresh occurs only once in the formula *)
            not
              ((list_count vars src = 1 && is_fresh_var src)
              || (list_count vars dst = 1 && is_fresh_var dst))
        | SSL.PointsTo (Var src, LS_t dst) ->
            (* filters out atoms (fresh -> x), where fresh occurs only once in formula *)
            let is_alone = is_fresh_var src && list_count vars src = 1 in

            (* filters out atoms (fresh1 -> fresh2) and (fresh2 -> fresh1), if this is their only
               occurence in the formula *)
            let is_cycle =
              is_fresh_var src && is_fresh_var dst
              && list_contains atoms (SSL.mk_pto (Var dst) (Var src))
            in
            (not @@ is_alone) || is_cycle
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
