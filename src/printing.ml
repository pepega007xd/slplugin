open Astral
open Cil_types
open Common

(* categories of debug output, enable with [-sl-msg-key <category1,category2,...>] *)
let do_instr = Config.Self.register_category "do_instr"
let combine_predecessors = Config.Self.register_category "combine_predecessors"
let do_guard = Config.Self.register_category "do_guard"
let do_edge = Config.Self.register_category "do_edge"

let pp_state (fmt : Format.formatter) (state : state) =
  List.iter (SSL.pp fmt) state
