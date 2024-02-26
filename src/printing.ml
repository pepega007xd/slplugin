open Astral
open Cil_types

let print_warn (msg : string) =
  print_string "\x1b[31;1m";
  print_string msg;
  print_char '\n';
  print_string "\x1b[0m"

let print_stmt (stmt : Cil_types.stmt) =
  (* print in yellow color *)
  print_string "\x1b[33m";

  let stmt = Format.asprintf "%a" Cil_datatype.Stmt.pretty stmt in
  (String.split_on_char '\n' stmt |> function
   | [] -> print_endline "<empty stmt>"
   | a :: [] -> print_endline a
   | [ a; b ] -> print_endline (a ^ "\n" ^ b)
   | a :: b :: _ -> print_endline (a ^ "\n" ^ b ^ "\n" ^ "..."));

  print_string "\x1b[0m"

let print_state (state : SSL.t list) =
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
