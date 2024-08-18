open Astral
open Common

type var = SSL.Variable.t

let nil = SSL.Variable.nil

(** Variables *)

let substitute_by_fresh (var : var) (f : SSL.t) : SSL.t =
  SSL.substitute f ~var ~by:(mk_fresh_var_from var)

let convert_vars_to_fresh (var_names : string list) (formula : SSL.t) : SSL.t =
  List.fold_left
    (fun formula var_name ->
      (* TODO: get proper variable sorts (or just subtitute all of them?) *)
      SSL.substitute formula
        ~var:(SSL.Variable.mk var_name Sort.loc_ls)
        ~by:(SSL.Variable.mk_fresh var_name Sort.loc_ls))
    formula var_names

let swap_vars (var1 : var) (var2 : var) (f : SSL.t) =
  let tmp_name = SSL.Variable.mk "__tmp_var" Sort.loc_nil in
  f
  |> SSL.substitute ~var:var1 ~by:tmp_name
  |> SSL.substitute ~var:var2 ~by:var1
  |> SSL.substitute ~var:tmp_name ~by:var2

(** Atoms *)

let get_atoms : SSL.t -> SSL.t list =
  (* TODO: do we need the other cases? *)
  function
  (* | SSL.Emp -> [] *)
  (* | SSL.Eq _ | SSL.Distinct _ | SSL.PointsTo _ | SSL.LS _ -> [ f ] *)
  | SSL.Star atoms -> atoms
  (* | SSL.Or (lhs, rhs) -> get_atoms lhs @ get_atoms rhs *)
  | _ -> fail "invalid shape of formula"

let add_atom (atom : SSL.t) (f : SSL.t) : SSL.t =
  SSL.mk_star (atom :: get_atoms f)

let remove_atom (atom : SSL.t) (f : SSL.t) : SSL.t =
  f |> get_atoms |> List.filter (( <> ) atom) |> SSL.mk_star

(** Equivalence classes *)

let make_equiv_class (vars : var list) : SSL.t =
  vars |> List.map (fun x -> SSL.Var x) |> SSL.mk_eq_list

let get_equiv_classes (f : SSL.t) : var list list =
  let get_vars vars =
    List.map (function SSL.Var var -> var | _ -> fail "unreachable") vars
  in

  f |> get_atoms
  |> List.filter_map (function
       | SSL.Eq list -> Some (get_vars list)
       | _ -> None)

let map_equiv_classes (fn : var list -> var list) (f : SSL.t) : SSL.t =
  let new_equiv_classes =
    get_equiv_classes f |> List.map fn |> List.map make_equiv_class
  in
  f |> get_atoms
  |> List.filter (function SSL.Eq _ -> false | _ -> true)
  |> List.append new_equiv_classes
  |> SSL.mk_star

let add_equiv_class (equiv_class : var list) (f : SSL.t) =
  add_atom (make_equiv_class equiv_class) f

let remove_equiv_class (equiv_class : var list) (f : SSL.t) =
  remove_atom (make_equiv_class equiv_class) f

(** Pure atoms *)

let add_eq (lhs : var) (rhs : var) (f : SSL.t) : SSL.t =
  let equiv_classes = get_equiv_classes f in
  let lhs_class = List.find_opt (List.mem lhs) equiv_classes in
  let rhs_class = List.find_opt (List.mem rhs) equiv_classes in

  match (lhs_class, rhs_class) with
  (* both variables are already in the same equiv class - do nothing *)
  | Some lhs_class, Some rhs_class when lhs_class = rhs_class -> f
  (* each variable is already in a different equiv class - merge classes *)
  | Some lhs_class, Some rhs_class ->
      f
      |> remove_equiv_class lhs_class
      |> remove_equiv_class rhs_class
      |> add_equiv_class (lhs_class @ rhs_class)
      (* one of the variables is in no existing class - add it to the existing one *)
  | Some lhs_class, None ->
      f |> remove_equiv_class lhs_class |> add_equiv_class (rhs :: lhs_class)
  | None, Some rhs_class ->
      f |> remove_equiv_class rhs_class |> add_equiv_class (lhs :: rhs_class)
  (* no variable is in an existing class - create a new class *)
  | _ -> f |> add_equiv_class [ lhs; rhs ]

let add_distinct (lhs : var) (rhs : var) : SSL.t -> SSL.t =
  add_atom (SSL.mk_distinct (Var lhs) (Var rhs))

(** Spatial atoms *)

let is_spatial_source (src : var) (f : SSL.t) : bool =
  f |> get_atoms
  (* TODO: use better exception than Not_found *)
  |> List.exists (function
       | SSL.PointsTo (Var var, _)
       | SSL.LS (Var var, _)
       | SSL.DLS (Var var, _, _, _)
       | SSL.NLS (Var var, _, _) ->
           var = src
       | _ -> false)

(** returns the spatial atom starting at [var] or at a variable equal to it *)
let make_var_explicit_src (var : var) (f : SSL.t) : SSL.t =
  get_equiv_classes f |> List.find_opt (List.mem var) |> function
  | Some equiv_class ->
      let current_src =
        List.find (fun src -> is_spatial_source src f) equiv_class
      in
      swap_vars current_src var f
  | None -> f

let get_spatial_atom_from (src : var) (f : SSL.t) : SSL.t =
  f |> make_var_explicit_src src |> get_atoms
  (* TODO: use better exception than Not_found *)
  |> List.find (function
       | SSL.PointsTo (Var var, _)
       | SSL.LS (Var var, _)
       | SSL.DLS (Var var, _, _, _)
       | SSL.DLS (_, Var var, _, _)
       | SSL.NLS (Var var, _, _) ->
           var = src
       | _ -> false)

let get_spatial_target (var : var) (field : Preprocessing.field_type)
    (f : SSL.t) : var =
  let spatial_atom = get_spatial_atom_from var f in
  match (field, spatial_atom) with
  | ( Next,
      ( PointsTo (_, (LS_t next | DLS_t (next, _) | NLS_t (_, next)))
      | LS (_, Var next)
      | DLS (_, _, _, Var next)
      | NLS (_, _, Var next) ) ) ->
      next
  | Prev, (PointsTo (_, DLS_t (_, prev)) | DLS (_, _, Var prev, _)) -> prev
  | Top, (PointsTo (_, NLS_t (top, _)) | NLS (_, Var top, _)) -> top
  | _ -> fail "unreachable"

let change_pto_target (src : var) (field : Preprocessing.field_type)
    (new_target : var) (f : SSL.t) : SSL.t =
  (* TODO: make this general for all spatial atoms? *)
  let old_atom = get_spatial_atom_from src f in
  let old_struct =
    match old_atom with
    | SSL.PointsTo (_, target) -> target
    | _ -> fail "unreachable"
  in

  let open SSL.Struct in
  let new_struct =
    match (field, old_struct) with
    | Next, LS_t _ -> LS_t new_target
    | Next, DLS_t (_, prev) -> DLS_t (new_target, prev)
    | Next, NLS_t (top, _) -> NLS_t (top, new_target)
    | Prev, DLS_t (next, _) -> DLS_t (next, new_target)
    | Top, NLS_t (_, next) -> NLS_t (new_target, next)
    | _ -> fail "unreachable"
  in
  f |> remove_atom old_atom |> add_atom (SSL.PointsTo (Var src, new_struct))

let remove_spatial_from (var : var) (f : SSL.t) : SSL.t =
  let original_atom = get_spatial_atom_from var f in
  remove_atom original_atom f

let assert_allocated (var : var) (f : SSL.t) : unit =
  ignore @@ get_spatial_atom_from var f

(** transforms [formula] so that [var] is a part of a points-to atom, not a list segment,
   multiple formulas can be produced, representing different lengths of [ls] (1, 2+) *)
let materialize (var : var) (f : SSL.t) : SSL.t list =
  let get_fresh_var = function
    | SSL.Var var -> SSL.Var (Common.mk_fresh_var_from var)
    | _ -> fail "unreachable"
  in
  let old_atom = get_spatial_atom_from var f in
  let f = remove_atom old_atom f in
  let var = SSL.Var var in
  match old_atom with
  | SSL.LS (src, next) ->
      let new_var = get_fresh_var src in
      [
        add_atom (SSL.mk_pto src next) f;
        f
        |> add_atom (SSL.mk_pto src new_var)
        |> add_atom (SSL.mk_ls new_var next)
        |> add_atom (SSL.mk_distinct new_var next);
      ]
  | SSL.DLS (src, dst, prev, next) when var = src ->
      let new_var = get_fresh_var src in
      [ (* TODO: *) ]
  | SSL.DLS (src, dst, prev, next) when var = dst ->
      let new_var = get_fresh_var dst in
      [ (* TODO: *) ]
  | SSL.NLS (src, top, next) ->
      let new_var = get_fresh_var src in
      [ (* TODO: *) ]
  | _ -> [ f ]

(** Miscellaneous *)

(** returns true when variable appears only once in the formula, [Distinct] atoms are ignored *)
let count_occurences_excl_distinct (var : var) (f : SSL.t) : int =
  f |> get_atoms
  |> List.filter (function SSL.Distinct _ -> false | _ -> true)
  |> List.concat_map SSL.get_vars
  |> Common.list_count var
