open Config
open Astral

let fail message = Self.fatal ~current:true message
let warning message = Self.warning ~current:true message
let debug message = Self.warning ~current:true message

let mk_fresh_var_from (base : SSL.Variable.t) : SSL.Variable.t =
  SSL.Variable.mk_fresh
    (SSL.Variable.get_name base)
    (SSL.Variable.get_sort base)

let is_fresh_var (var : SSL.Variable.t) : bool =
  String.contains (SSL.Variable.get_name var) '!'

let list_count (elem : 'a) (list : 'a List.t) : int =
  list |> List.filter (( = ) elem) |> List.length
