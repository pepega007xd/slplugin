open Cil_types
open Astral
open SSL.Struct
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

let x = SSL.Variable.mk "x" Sort.loc_ls
let y = SSL.Variable.mk "y" Sort.loc_ls
let z = SSL.Variable.mk "z" Sort.loc_ls
let x' = SSL.Variable.mk "x!" Sort.loc_ls
let y' = SSL.Variable.mk "y!" Sort.loc_ls
let z' = SSL.Variable.mk "z!" Sort.loc_ls

let assert_eq (lhs : t) (rhs : t) : bool =
  if SSL.equal (to_astral lhs) (to_astral rhs) then true
  else (
    print_warn_nl "Formulas do not match:";
    print_warn "RESULT: ";
    show_formula lhs;
    print_warn "EXPECTED: ";
    show_formula rhs;
    false)
