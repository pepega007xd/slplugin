open Astral
open Slplugin_options

let solver = ref (Solver.init ())
let check_sat (formula : SSL.t) : bool = Solver.check_sat !solver formula
let fail message = Self.fatal ~current:true message

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
  | first :: rest ->
      if list_contains rest first then list_deduplicate rest
      else first :: list_deduplicate rest

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
