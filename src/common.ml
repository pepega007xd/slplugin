open Config
open Astral

let fail message = Self.fatal ~current:true message
let warning message = Self.warning ~current:true message
let debug message = Self.warning ~current:true message

let mk_fresh_var_from (base : SL.Variable.t) : SL.Variable.t =
  if base = SL.Variable.nil then SL.Variable.nil
  else
    SL.Variable.mk_fresh (SL.Variable.get_name base) (SL.Variable.get_sort base)

let is_fresh_var (var : SL.Variable.t) : bool =
  String.contains (SL.Variable.get_name var) '!'

let list_count (elem : 'a) (list : 'a List.t) : int =
  list |> List.filter (( = ) elem) |> List.length

let rec list_map_pairs (f : 'a -> 'a -> 'b) (list : 'a list) : 'b list =
  match list with
  | [] -> []
  | first :: rest -> List.map (f first) rest @ list_map_pairs f rest
