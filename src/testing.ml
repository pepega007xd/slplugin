open Astral
include Formula

let print_warn (msg : string) =
  prerr_string "\027[31;1m";
  prerr_string msg;
  prerr_string "\027[0m"

let print_warn_nl (msg : string) =
  print_warn msg;
  print_warn "\n"

let show_formula (f : t) : unit =
  List.map atom_to_string f |> String.concat " * " |> print_warn_nl

let show_state (state : state) : unit = List.iter show_formula state
let u = SL.Variable.mk "u" SL_builtins.loc_ls
let v = SL.Variable.mk "v" SL_builtins.loc_ls
let w = SL.Variable.mk "w" SL_builtins.loc_ls
let x = SL.Variable.mk "x" SL_builtins.loc_ls
let y = SL.Variable.mk "y" SL_builtins.loc_ls
let z = SL.Variable.mk "z" SL_builtins.loc_ls

(* fresh vars *)
let u' = SL.Variable.mk "u'" SL_builtins.loc_ls
let v' = SL.Variable.mk "v!" SL_builtins.loc_ls
let w' = SL.Variable.mk "w!" SL_builtins.loc_ls
let x' = SL.Variable.mk "x!" SL_builtins.loc_ls
let y' = SL.Variable.mk "y!" SL_builtins.loc_ls
let z' = SL.Variable.mk "z!" SL_builtins.loc_ls

(* DLS sort vars *)
module DLS = struct
  let u = SL.Variable.mk "u" SL_builtins.loc_dls
  let v = SL.Variable.mk "v" SL_builtins.loc_dls
  let w = SL.Variable.mk "w" SL_builtins.loc_dls
  let x = SL.Variable.mk "x" SL_builtins.loc_dls
  let y = SL.Variable.mk "y" SL_builtins.loc_dls
  let z = SL.Variable.mk "z" SL_builtins.loc_dls

  (* fresh vars *)
  let u' = SL.Variable.mk "u'" SL_builtins.loc_dls
  let v' = SL.Variable.mk "v!" SL_builtins.loc_dls
  let w' = SL.Variable.mk "w!" SL_builtins.loc_dls
  let x' = SL.Variable.mk "x!" SL_builtins.loc_dls
  let y' = SL.Variable.mk "y!" SL_builtins.loc_dls
  let z' = SL.Variable.mk "z!" SL_builtins.loc_dls
end

let assert_eq (lhs : t) (rhs : t) : bool =
  if canonicalize lhs = canonicalize rhs then true
  else (
    print_warn_nl "Formulas do not match:";
    print_warn "RESULT: ";
    show_formula lhs;
    print_warn "EXPECTED: ";
    show_formula rhs;
    false)

let assert_eq_state (lhs : state) (rhs : state) : bool =
  if canonicalize_state lhs = canonicalize_state rhs then true
  else (
    print_warn_nl "States do not match:";
    print_warn "RESULT: ";
    show_state lhs;
    print_warn "EXPECTED: ";
    show_state rhs;
    false)

(* tests for Formula cannot be in the Formula module due to circular dependency *)
module Tests = struct
  (* we want the bounds sorted [1+, 2+, 3+, ...] so that during deduplication 
     the lowest possible bound is picked first *)
  let%test "compare_bounds" =
    let input = [ [ mk_ls x y 1 ]; [ mk_ls x y 2 ] ] in
    input |> List.sort compare_bounds = input

  let%test "compare_bounds_2" =
    let input = [ [ mk_dls z y u' v' 3 ]; [ mk_dls x y u v 2 ] ] in
    let expected = [ [ mk_dls x y u v 2 ]; [ mk_dls z y u' v' 3 ] ] in
    input |> List.sort compare_bounds = expected
end
