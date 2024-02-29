open Cil_types
open Common
open Astral

let eprint_string (s : string) = Printf.eprintf "%s" s
let eprint_endline (s : string) = Printf.eprintf "%s\n" s
let eprint_char (c : char) = Printf.eprintf "%c" c

let print_warn (msg : string) =
  eprint_string "\x1b[31;1m";
  eprint_string msg;
  eprint_char '\n';
  eprint_string "\x1b[0m"

let print_stmt (stmt : Cil_types.stmt) =
  (* print in yellow color *)
  eprint_string "\x1b[33m";

  let stmt = Format.asprintf "%a" Cil_datatype.Stmt.pretty stmt in
  (String.split_on_char '\n' stmt |> function
   | [] -> eprint_endline "<empty stmt>"
   | a :: [] -> eprint_endline a
   | [ a; b ] -> eprint_endline (a ^ "\n" ^ b)
   | a :: b :: _ -> eprint_endline (a ^ "\n" ^ b ^ "\n" ^ "..."));

  eprint_string "\x1b[0m"

let print_state (state : SSL.t list) =
  let space = "    " in
  eprint_string space;
  List.map (fun f -> SSL.show @@ Simplifier.simplify f) state
  |> String.concat ("\n" ^ space)
  |> eprint_endline

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

let x_var = SSL.Variable.mk "x" Sort.loc_ls
let y_var = SSL.Variable.mk "y" Sort.loc_ls
let z_var = SSL.Variable.mk "z" Sort.loc_ls
let x'_var = SSL.Variable.mk "x!" Sort.loc_ls
let y'_var = SSL.Variable.mk "y!" Sort.loc_ls
let z'_var = SSL.Variable.mk "z!" Sort.loc_ls
let nil_var = SSL.Variable.nil
let mk_var (name : string) : SSL.t = SSL.mk_var name Sort.loc_ls
let x = mk_var "x"
let y = mk_var "y"
let z = mk_var "z"
let x' = mk_var "x!"
let y' = mk_var "y!"
let z' = mk_var "z!"
let nil = SSL.mk_nil ()
let print (formula : SSL.t) = print_warn @@ SSL.show formula

let is_equal (lhs : SSL.t) (rhs : SSL.t) : bool =
  let lhs = List.sort SSL.compare @@ get_atoms lhs in
  let rhs = List.sort SSL.compare @@ get_atoms rhs in
  List.equal SSL.equal lhs rhs
