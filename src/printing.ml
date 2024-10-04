open Astral

(* categories of debug output, enable with [-sl-msg-key <category1,category2,...>] *)
let do_instr = Config.Self.register_category "do_instr"
let combine_predecessors = Config.Self.register_category "combine_predecessors"
let do_guard = Config.Self.register_category "do_guard"
let do_edge = Config.Self.register_category "do_edge"

let atom_to_string : Formula.atom -> 'a =
  let var = SSL.Variable.show in
  function
  | Emp -> "Emp"
  | Eq vars -> vars |> List.map var |> String.concat " = "
  | Distinct (lhs, rhs) -> var lhs ^ " != " ^ var rhs
  | PointsTo (src, LS_t next) -> var src ^ " |-> " ^ var next
  | PointsTo (src, DLS_t (next, prev)) ->
      Format.sprintf "%s |-> n:%s,p:%s" (var src) (var next) (var prev)
  | PointsTo (src, NLS_t (top, next)) ->
      Format.sprintf "%s |-> t:%s,n:%s" (var src) (var top) (var next)
  | LS ls ->
      Format.sprintf "ls_%d+(%s,%s)" ls.min_len (var ls.first) (var ls.next)
  | DLS dls ->
      Format.sprintf "dls_%d+(%s,%s,%s,%s)" dls.min_len (var dls.first)
        (var dls.last) (var dls.prev) (var dls.next)
  | NLS nls ->
      Format.sprintf "nls_%d+(%s,%s,%s)" nls.min_len (var nls.first)
        (var nls.top) (var nls.next)

let pp_atom (fmt : Format.formatter) (atom : Formula.atom) =
  Format.fprintf fmt "%s" (atom_to_string atom)

let pp_formula (fmt : Format.formatter) (formula : Formula.t) =
  formula |> List.map atom_to_string
  |> List.map (fun s -> "(" ^ s ^ ")")
  |> String.concat " "
  |> Format.fprintf fmt "\n%s\n"

let pp_state (fmt : Format.formatter) (state : Formula.state) =
  Format.fprintf fmt "\n";

  List.iter
    (fun formula ->
      pp_formula fmt formula;
      Format.fprintf fmt "\n")
    state
