open Cil_types
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
  (* order insensitive compare *)
  if
    List.for_all (fun atom -> List.mem atom lhs) rhs
    && List.for_all (fun atom -> List.mem atom rhs) lhs
  then true
  else (
    print_warn_nl "Formulas do not match:";
    print_warn "RESULT: ";
    show_formula lhs;
    print_warn "EXPECTED: ";
    show_formula rhs;
    false)
