open Astral
open Common

type var = SSL.Variable.t

type atom =
  | Eq of var list
  | Distinct of var * var
  | PointsTo of var * SSL.Struct.t
  | LS of { first : var; next : var; min_len : int }
  | DLS of { first : var; last : var; prev : var; next : var; min_len : int }
  | NLS of { first : var; top : var; next : var; min_len : int }

type t = atom list

(* state stored by each CFG node in dataflow analysis *)
type state = t list

let nil = SSL.Variable.nil

(** Constructors *)

let mk_ls (first : var) (next : var) (min_len : int) =
  LS { first; next; min_len }

let mk_dls (first : var) (last : var) (prev : var) (next : var) (min_len : int)
    =
  DLS { first; last; prev; next; min_len }

let mk_nls (first : var) (top : var) (next : var) (min_len : int) =
  NLS { first; top; next; min_len }

(** Formatters *)

let atom_to_string : atom -> 'a =
  let var var =
    SSL.Variable.show var
    (* let sort = SSL.Variable.get_sort var |> Sort.show in *)
    (* SSL.Variable.show var ^ ":" ^ sort *)
  in
  function
  | Eq vars -> vars |> List.map var |> String.concat " = "
  | Distinct (lhs, rhs) -> var lhs ^ " != " ^ var rhs
  | PointsTo (src, LS_t next) -> var src ^ " -> " ^ var next
  | PointsTo (src, DLS_t (next, prev)) ->
      Format.sprintf "%s -> n:%s,p:%s" (var src) (var next) (var prev)
  | PointsTo (src, NLS_t (top, next)) ->
      Format.sprintf "%s -> t:%s,n:%s" (var src) (var top) (var next)
  | LS ls ->
      Format.sprintf "ls_%d+(%s,%s)" ls.min_len (var ls.first) (var ls.next)
  | DLS dls ->
      Format.sprintf "dls_%d+(%s,%s,%s,%s)" dls.min_len (var dls.first)
        (var dls.last) (var dls.prev) (var dls.next)
  | NLS nls ->
      Format.sprintf "nls_%d+(%s,%s,%s)" nls.min_len (var nls.first)
        (var nls.top) (var nls.next)

let pp_atom (fmt : Format.formatter) (atom : atom) =
  Format.fprintf fmt "%s" (atom_to_string atom)

let pp_formula (fmt : Format.formatter) (formula : t) =
  let formula =
    formula |> List.map atom_to_string
    |> List.map (fun s -> "(" ^ s ^ ")")
    |> String.concat " * "
  in
  if formula = "" then Format.fprintf fmt "emp"
  else Format.fprintf fmt "%s" formula

let show_formula (f : t) : unit = Config.Self.warning "FORMULA: %a" pp_formula f

let pp_state (fmt : Format.formatter) (state : state) =
  List.iter
    (fun formula ->
      pp_formula fmt formula;
      Format.fprintf fmt "\n")
    state

(** Conversion to and from Astral type *)

let to_astral (formula : t) : SSL.t =
  let map_vars = List.map (fun var -> SSL.Var var) in
  let atoms =
    List.map
      (function
        | Eq vars -> SSL.Eq (map_vars vars)
        | Distinct (lhs, rhs) -> SSL.mk_distinct (Var lhs) (Var rhs)
        | PointsTo (src, dst) -> SSL.PointsTo (Var src, dst)
        | LS ls -> (
            let first = SSL.Var ls.first in
            let next = SSL.Var ls.next in

            let ls_0 = SSL.mk_ls first next in
            let ls_1 = SSL.mk_star [ ls_0; SSL.mk_distinct first next ] in

            match ls.min_len with
            | 0 -> ls_0
            | 1 -> ls_1
            | _ -> SSL.mk_gneg ls_1 (SSL.mk_pto first next))
        | DLS dls -> (
            let first = SSL.Var dls.first in
            let last = SSL.Var dls.last in
            let prev = SSL.Var dls.prev in
            let next = SSL.Var dls.next in

            let dls_0 = SSL.mk_dls first last prev next in
            let dls_1 = SSL.mk_star [ dls_0; SSL.mk_distinct first next ] in
            let dls_2 =
              SSL.mk_star
                [
                  dls_0; SSL.mk_distinct first next; SSL.mk_distinct first last;
                ]
            in

            match dls.min_len with
            | 0 -> dls_0
            | 1 -> dls_1
            | 2 -> dls_2
            | _ ->
                SSL.mk_gneg dls_2
                  (SSL.mk_star
                     [
                       SSL.mk_pto_dls first last prev;
                       SSL.mk_pto_dls last next first;
                     ]))
        | NLS nls -> (
            let first = SSL.Var nls.first in
            let top = SSL.Var nls.top in
            let next = SSL.Var nls.next in
            let nls_0 = SSL.mk_nls first top next in
            let nls_1 = SSL.mk_star [ nls_0; SSL.mk_distinct first top ] in
            let fresh = SSL.Var (Common.mk_fresh_var_from nls.next) in
            match nls.min_len with
            | 0 -> nls_0
            | 1 -> nls_1
            | _ ->
                SSL.mk_gneg nls_1
                  (SSL.mk_star
                     [ SSL.mk_pto_nls first top fresh; SSL.mk_ls fresh next ])))
      formula
  in
  SSL.Star atoms

let state_to_astral (state : state) : SSL.t =
  state |> List.map to_astral |> SSL.mk_or

let from_astral_atom : SSL.t -> atom =
  let get_var = function
    | SSL.Var var -> var
    | _ -> fail "unreachable formula.ml:100"
  in
  let map_vars = List.map get_var in

  function
  | SSL.Eq vars -> Eq (map_vars vars)
  | SSL.Distinct [ Var lhs; Var rhs ] -> Distinct (lhs, rhs)
  | SSL.PointsTo (Var src, dst) -> PointsTo (src, dst)
  | SSL.LS (Var first, Var next) -> LS { first; next; min_len = 0 }
  | SSL.DLS (Var first, Var last, Var prev, Var next) ->
      DLS { first; last; prev; next; min_len = 0 }
  | SSL.NLS (Var first, Var top, Var next) ->
      NLS { first; top; next; min_len = 0 }
  | SSL.Star (inner :: rest) -> (
      match inner with
      | SSL.LS (Var first, Var next) -> LS { first; next; min_len = 1 }
      | SSL.DLS (Var first, Var last, Var prev, Var next) ->
          DLS { first; last; prev; next; min_len = List.length rest }
      | SSL.NLS (Var first, Var top, Var next) ->
          NLS { first; top; next; min_len = 1 }
      | _ -> fail "invalid formula shape")
  | SSL.GuardedNeg (SSL.Star (inner :: _), _) -> (
      match inner with
      | SSL.LS (Var first, Var next) -> LS { first; next; min_len = 2 }
      | SSL.DLS (Var first, Var last, Var prev, Var next) ->
          DLS { first; last; prev; next; min_len = 3 }
      | SSL.NLS (Var first, Var top, Var next) ->
          NLS { first; top; next; min_len = 2 }
      | _ -> fail "invalid formula shape")
  | _ -> fail "invalid formula shape"

let from_astral : SSL.t -> t = function
  | SSL.Star atoms -> List.map from_astral_atom atoms
  | SSL.Emp -> []
  | other -> fail "unreachable formula.ml:136: %a" SSL.pp other

(** Variables *)

let substitute (f : t) ~(var : var) ~(by : var) : t =
  f |> to_astral |> SSL.substitute ~var ~by |> from_astral

let substitute_by_fresh (var : var) : t -> t =
  substitute ~var ~by:(mk_fresh_var_from var)

let convert_vars_to_fresh (var_names : string list) (formula : t) : t =
  List.fold_left
    (fun formula var_name ->
      (* TODO: get proper variable sorts (or just subtitute all of them?) *)
      substitute formula
        ~var:(SSL.Variable.mk var_name Sort.loc_ls)
        ~by:(SSL.Variable.mk_fresh var_name Sort.loc_ls))
    formula var_names

let swap_vars (var1 : var) (var2 : var) (f : t) =
  let tmp_name = SSL.Variable.mk "__tmp_var" Sort.loc_nil in
  f
  |> substitute ~var:var1 ~by:tmp_name
  |> substitute ~var:var2 ~by:var1
  |> substitute ~var:tmp_name ~by:var2

(** Atoms *)

let add_atom (atom : atom) (f : t) : t = atom :: f
let remove_atom (atom : atom) (f : t) : t = f |> List.filter (( <> ) atom)

(** Equivalence classes *)

let get_equiv_classes : t -> var list list =
  List.filter_map (function Eq list -> Some list | _ -> None)

let map_equiv_classes (fn : var list -> var list) : t -> t =
  List.map (function Eq vars -> Eq (fn vars) | other -> other)

let add_equiv_class (equiv_class : var list) (f : t) =
  add_atom (Eq equiv_class) f

let remove_equiv_class (equiv_class : var list) (f : t) =
  remove_atom (Eq equiv_class) f

(** Pure atoms *)

let add_eq (lhs : var) (rhs : var) (f : t) : t =
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

let add_distinct (lhs : var) (rhs : var) : t -> t =
  add_atom (Distinct (lhs, rhs))

(** Spatial atoms *)

let is_spatial_source (src : var) : atom -> bool = function
  | PointsTo (var, _) -> src = var
  | LS ls -> ls.first = src
  | DLS dls -> dls.first = src || dls.last = src
  | NLS nls -> nls.first = src
  | _ -> false

let make_var_explicit_src (var : var) (f : t) : t =
  get_equiv_classes f |> List.find_opt (List.mem var) |> function
  | Some equiv_class ->
      let current_src =
        List.find_opt
          (fun src -> List.exists (is_spatial_source src) f)
          equiv_class
      in
      Option.map (fun current_src -> swap_vars current_src var f) current_src
      |> Option.value ~default:f
  | None -> f

let get_spatial_atom_from (src : var) (f : t) : atom option =
  f |> make_var_explicit_src src |> List.find_opt (is_spatial_source src)

let get_spatial_target (var : var) (field : Preprocessing.field_type) (f : t) :
    var option =
  get_spatial_atom_from var f |> function
  | Some spatial_atom ->
      Some
        (match (spatial_atom, field) with
        | PointsTo (_, LS_t next), Next -> next
        | PointsTo (_, DLS_t (next, _)), Next -> next
        | PointsTo (_, DLS_t (_, prev)), Prev -> prev
        | PointsTo (_, NLS_t (top, _)), Top -> top
        | PointsTo (_, NLS_t (_, next)), Next -> next
        | LS ls, Next -> ls.next
        | DLS dls, Next -> dls.next
        | DLS dls, Prev -> dls.prev
        | NLS nls, Top -> nls.top
        | NLS nls, Next -> nls.next
        | _ -> fail "TODO: what to do with Data field?")
  | None -> None

let remove_spatial_from (var : var) (f : t) : t =
  get_spatial_atom_from var f |> function
  | Some original_atom -> remove_atom original_atom f
  | None -> f

let change_pto_target (src : var) (field : Preprocessing.field_type)
    (new_target : var) (f : t) : t =
  (* TODO: make this general for all spatial atoms? *)
  let old_struct =
    get_spatial_atom_from src f |> Option.get |> function
    | PointsTo (_, old_struct) -> old_struct
    | _ -> fail "unreachable formula.ml:259"
  in
  let open SSL.Struct in
  let new_struct =
    match (field, old_struct) with
    | Next, LS_t _ -> LS_t new_target
    | Next, DLS_t (_, prev) -> DLS_t (new_target, prev)
    | Next, NLS_t (top, _) -> NLS_t (top, new_target)
    | Prev, DLS_t (next, _) -> DLS_t (next, new_target)
    | Top, NLS_t (_, next) -> NLS_t (new_target, next)
    | _ -> fail "unreachable formula.ml:270"
  in
  f |> remove_spatial_from src |> add_atom (PointsTo (src, new_struct))

let get_spatial_atom_min_length : atom -> int = function
  | LS ls -> ls.min_len
  | DLS dls -> dls.min_len
  | NLS nls -> nls.min_len
  | PointsTo _ -> 1
  | _ -> fail "unreachable formula.ml:279"

let assert_allocated (var : var) (f : t) : unit =
  if get_spatial_atom_from var f |> Option.is_none then
    fail "Variable %a is not allocated in %a" SSL.Variable.pp var pp_formula f

(** transforms [formula] so that [var] is a part of a points-to atom, not a list segment,
   multiple formulas can be produced, representing different lengths of [ls] (1, 2+) *)
let rec materialize (var : var) (f : t) : t list =
  let fresh_var = mk_fresh_var_from var in
  let old_atom = get_spatial_atom_from var f |> Option.get in
  let f = remove_atom old_atom f in

  (* let open SSL.Struct in *)
  match old_atom with
  | PointsTo _ -> [ add_atom old_atom f ]
  (* ls has minimum length greater than zero -> just decrement and split off PointsTo *)
  | LS ls when ls.min_len > 0 ->
      [
        f
        |> add_atom (PointsTo (var, LS_t fresh_var))
        |> add_atom @@ mk_ls fresh_var ls.next (ls.min_len - 1);
      ]
  (* ls has minimum length equal to zero -> case split to 0 and 1+ *)
  | LS ls ->
      (* case where ls has length 1+ *)
      (f
      |> add_atom (PointsTo (var, LS_t fresh_var))
      |> add_atom @@ mk_ls fresh_var ls.next 0)
      (* cases where ls has length 0 *)
      :: (f |> add_eq ls.first ls.next |> materialize var)
  (* cases where DLS has minimum length of at least one *)
  | DLS dls when dls.min_len > 0 && var = dls.first ->
      [
        f
        |> add_atom (PointsTo (var, DLS_t (fresh_var, dls.prev)))
        |> add_atom @@ mk_dls fresh_var dls.last var dls.next (dls.min_len - 1);
      ]
  | DLS dls when dls.min_len > 0 && var = dls.last ->
      [
        f
        |> add_atom (PointsTo (var, DLS_t (dls.next, fresh_var)))
        |> add_atom @@ mk_dls dls.first fresh_var dls.prev var (dls.min_len - 1);
      ]
  (* cases where DLS has minimum length of zero -> case split *)
  | DLS dls when var = dls.first ->
      (* length 1+ case *)
      (f
      |> add_atom (PointsTo (var, DLS_t (fresh_var, dls.prev)))
      |> add_atom @@ mk_dls fresh_var dls.last var dls.next 0)
      (* length 0 cases *)
      :: (f |> add_eq dls.first dls.last |> add_eq dls.first dls.next
        |> add_eq dls.last dls.prev |> materialize var)
  | DLS dls when var = dls.last ->
      (f
      |> add_atom (PointsTo (var, DLS_t (dls.next, fresh_var)))
      |> add_atom @@ mk_dls dls.first fresh_var dls.prev var (dls.min_len - 1))
      :: (f |> add_eq dls.first dls.last |> add_eq dls.first dls.next
        |> add_eq dls.last dls.prev |> materialize var)
  (* case where NLS has minimum length of at least one *)
  | NLS nls when nls.min_len > 0 ->
      (* materalization of NLS produces a LS_0+ from fresh_var to `nls.next` *)
      let fresh_ls = mk_fresh_var_from nls.next in
      [
        f
        |> add_atom (PointsTo (var, NLS_t (fresh_var, fresh_ls)))
        |> add_atom @@ mk_ls fresh_ls nls.next 0
        |> add_atom @@ mk_nls fresh_var nls.top nls.next (nls.min_len - 1);
      ]
  | NLS nls ->
      let fresh_ls = mk_fresh_var_from nls.next in
      (* length 1+ case *)
      (f
      |> add_atom (PointsTo (var, NLS_t (fresh_var, fresh_ls)))
      |> add_atom @@ mk_ls fresh_ls nls.next 0
      |> add_atom @@ mk_nls fresh_var nls.top nls.next (nls.min_len - 1))
      (* length 0 cases *)
      :: (f |> add_eq nls.first nls.top
         |> add_atom @@ mk_ls nls.first nls.next 0
         |> materialize var)
  | _ -> fail "unreachable formula.ml:357"

(** Miscellaneous *)

(** returns true when variable appears only once in the formula, [Distinct] atoms are ignored *)
let count_occurences_excl_distinct (var : var) (f : t) : int =
  f
  |> List.filter (function Distinct _ -> false | _ -> true)
  |> to_astral |> SSL.get_vars |> Common.list_count var

module Tests = struct
  open Testing
end
