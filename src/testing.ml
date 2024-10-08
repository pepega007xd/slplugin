open Cil_types
open Astral
open Formula

let print_warn (msg : string) =
  prerr_string "\027[31;1m";
  prerr_endline msg;
  prerr_string "\027[0m"

let show_formula (f : t) : unit =
  "F: " ^ (List.map atom_to_string f |> String.concat " * ") |> print_warn

let x = SSL.Variable.mk "x" Sort.loc_ls
let y = SSL.Variable.mk "y" Sort.loc_ls
let z = SSL.Variable.mk "z" Sort.loc_ls
let xf = SSL.Variable.mk "x!" Sort.loc_ls
let yf = SSL.Variable.mk "y!" Sort.loc_ls
let zf = SSL.Variable.mk "z!" Sort.loc_ls

let assert_eq (lhs : t) (rhs : t) : bool =
  if SSL.equal (to_astral lhs) (to_astral rhs) then true
  else (
    print_warn "Formulas do not match:";
    show_formula lhs;
    show_formula rhs;
    false)
