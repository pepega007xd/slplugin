open Astral
open Slplugin_options

let results : (Cil_types.stmt, SSL.t list) Hashtbl.t ref =
  ref (Hashtbl.create 113)

let solver = ref (Solver.init ())
let solver_time = ref 0.0

module StringSet = Set.Make (String)

let var_of_varinfo (varinfo : Cil_types.varinfo) : SSL.Variable.t =
  SSL.Variable.mk varinfo.vname Sort.loc_ls

let local_vars_for_stmt : (Cil_types.stmt, StringSet.t) Hashtbl.t ref =
  ref @@ Hashtbl.create 113

let check_sat (formula : SSL.t) : bool =
  let start = Sys.time () in
  let result = Solver.check_sat !solver formula in
  solver_time := !solver_time +. Sys.time () -. start;
  result

let fail message = Self.fatal ~current:true message

let mk_fresh_var (basename : string) : SSL.Variable.t =
  SSL.Variable.mk_fresh basename Sort.loc_ls

let is_fresh_var (var : SSL.Variable.t) : bool =
  let name, _ = var in
  String.contains name '!'

let rec get_atoms (formula : SSL.t) : SSL.t list =
  match formula with
  | SSL.Emp -> []
  | SSL.Eq list -> [ SSL.Eq list ]
  | SSL.Distinct list -> [ SSL.Distinct list ]
  | SSL.PointsTo (src, dst) -> [ SSL.PointsTo (src, dst) ]
  | SSL.LS (src, dst) -> [ SSL.LS (src, dst) ]
  | SSL.Star atoms -> atoms
  | SSL.Or (lhs, rhs) -> get_atoms lhs @ get_atoms rhs
  | _ -> fail "get_atoms: invalid shape of formula"

let list_contains (list : 'a List.t) (elem : 'a) : bool =
  match List.find_opt (fun x -> x = elem) list with
  | Some _ -> true
  | None -> false

let list_count (list : 'a List.t) (elem : 'a) : int =
  List.length @@ List.find_all (( = ) elem) list

let rec list_deduplicate (lst : 'a list) : 'a list =
  match lst with
  | [] -> []
  | first :: rest when list_contains rest first -> list_deduplicate rest
  | first :: rest -> first :: list_deduplicate rest

let extract_vars (atoms : SSL.t list) : SSL.Variable.t list =
  List.map
    (fun atom ->
      match atom with
      | SSL.PointsTo (Var src, LS_t dst) -> [ src; dst ]
      | SSL.Eq [ Var lhs; Var rhs ] -> [ lhs; rhs ]
      | SSL.Distinct [ Var lhs; Var rhs ] -> [ lhs; rhs ]
      | SSL.LS (Var lhs, Var rhs) -> [ lhs; rhs ]
      | _ -> fail "extract_vars: formula contains atoms other than pto or eq")
    atoms
  |> List.flatten
