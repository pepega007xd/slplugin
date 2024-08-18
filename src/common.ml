open Config
open Astral

(* state stored by each CFG node in dataflow analysis *)
type state = SSL.t list

(* state of dataflow analysis is stored here *)
let results : (Cil_types.stmt, SSL.t list) Hashtbl.t ref =
  ref (Hashtbl.create 113)

module StringSet = Set.Make (String)

let local_vars_for_stmt : (Cil_types.stmt, StringSet.t) Hashtbl.t ref =
  ref @@ Hashtbl.create 113

let fail message = Self.abort ~current:true message

let mk_fresh_var_from (base : SSL.Variable.t) : SSL.Variable.t =
  SSL.Variable.mk_fresh
    (SSL.Variable.get_name base)
    (SSL.Variable.get_sort base)

let is_fresh_var (var : SSL.Variable.t) : bool =
  String.contains (SSL.Variable.get_name var) '!'

let list_count (elem : 'a) (list : 'a List.t) : int =
  list |> List.filter (( = ) elem) |> List.length
