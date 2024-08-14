open Astral
open Cil_types
open Common

(* categories of debug output, enable with `-sl-msg-key <category1,category2,...>` *)
let do_instr = Config.Self.register_category "do_instr"
let combine_predecessors = Config.Self.register_category "combine_predecessors"
let do_guard = Config.Self.register_category "do_guard"
let do_edge = Config.Self.register_category "do_edge"

let print_control (s : string) =
  if Out_channel.isatty Out_channel.stdout then print_string s

let print_warn (msg : string) =
  print_control "\x1b[31;1m";
  print_string msg;
  print_char '\n';
  print_control "\x1b[0m"

let print_stmt (stmt : Cil_types.stmt) =
  (* print in yellow color *)
  print_control "\x1b[33m";

  let stmt = Format.asprintf "%a" Cil_datatype.Stmt.pretty stmt in
  (String.split_on_char '\n' stmt |> function
   | [] -> print_endline "<empty stmt>"
   | a :: [] -> print_endline a
   | [ a; b ] -> print_endline (a ^ "\n" ^ b)
   | a :: b :: _ -> print_endline (a ^ "\n" ^ b ^ "\n" ^ "..."));

  print_control "\x1b[0m"

let print_state (state : SSL.t list) =
  if List.is_empty state then print_endline "<empty state>"
  else
    let space = "    " in
    print_string space;
    List.map (fun f -> SSL.show @@ Simplifier.simplify f) state
    |> String.concat ("\n" ^ space)
    |> print_endline

let print_result (result : (stmt, SSL.t list) Hashtbl.t) =
  print_warn "Analysis results:";
  Hashtbl.to_seq result |> List.of_seq
  |> List.sort (fun a b ->
         let a, _ = a in
         let b, _ = b in
         a.sid - b.sid)
  |> List.iter (fun result ->
         let stmt, state = result in
         print_newline ();
         print_stmt stmt;
         print_state state)

let pp_state (fmt : Format.formatter) (state : state) =
  List.iter (SSL.pp fmt) state
