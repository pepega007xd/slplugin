open Astral
open Cil_types
open Common

type summary_input = Kernel_function.t * Formula.t

type function_context = {
  results : (Cil_types.stmt, Formula.state) Hashtbl.t;
  loop_cycles : (stmt, int) Hashtbl.t;
}

let summaries : (summary_input, Formula.state) Hashtbl.t = Hashtbl.create 113

(** for running dataflow analysis recursively on other functions *)
let compute_function : (Cil_types.stmt list -> unit) ref =
  ref (fun _ -> fail "called compute_function without initializing it")

let get_default_context () =
  { results = Hashtbl.create 113; loop_cycles = Hashtbl.create 113 }

(** All data specific for the analysis of a single function *)
let function_context = ref @@ get_default_context ()

(** stores past results from analysis of all visited functions, used to show all
    generated states in Ivette at the end of analysis *)
let previous_results : (Cil_types.stmt, Formula.state) Hashtbl.t list ref =
  ref []

let get_anchor (var : Formula.var) : Formula.var =
  let name = SL.Variable.get_name var in
  SL.Variable.mk
    (* '$' is used not to collide with an existing var name, eg. x!0 -> x_0,
       it has no special meaning, x$0 behaves like a regular program variable *)
    ("A_" ^ String.map (function '!' -> '$' | c -> c) name)
    (SL.Variable.get_sort var)

let run_analysis (func : Kernel_function.t) (formula : Formula.t) :
    Formula.state =
  let first_stmt = Kernel_function.find_first_stmt func in
  let return_stmt = Kernel_function.find_return func in
  (* backup current state of analysis into a variable *)
  let current_context = !function_context in

  function_context := get_default_context ();
  Hashtbl.add !function_context.results first_stmt [ formula ];
  !compute_function [ first_stmt ];
  let result_state = Hashtbl.find !function_context.results return_stmt in

  (* save generated states to display them later *)
  previous_results := !function_context.results :: !previous_results;

  (* restore current state of analysis *)
  function_context := current_context;

  result_state

let get_result_state (func : Kernel_function.t) (formula : Formula.t) :
    Formula.state =
  (* TODO: maybe canonicalize formula before creating a summary? *)
  let summary_input = Formula.standardize_fresh_var_names formula in
  Hashtbl.find_opt summaries (func, summary_input) |> function
  | Some result -> result
  | None ->
      let result_state = run_analysis func formula in
      Hashtbl.add summaries (func, summary_input) result_state;
      result_state

let func_call (args : Formula.var list) (func : varinfo) (formula : Formula.t)
    (lhs_opt : Formula.var option) : Formula.state =
  let func = Globals.Functions.get func in
  let fundec = Kernel_function.get_definition func in
  let return_stmt = Kernel_function.find_return func in

  let params =
    Kernel_function.get_formals func
    |> List.filter Types.is_relevant_var
    |> List.map Types.varinfo_to_var
  in
  let locals =
    params
    @ (Kernel_function.get_locals func
      |> List.filter Types.is_relevant_var
      |> List.map Types.varinfo_to_var)
  in

  let end_of_scope_stack_vars =
    fundec.sbody.blocals
    |> List.filter Types.is_relevant_var
    |> List.map Types.varinfo_to_var
    |> List.filter (fun var -> List.mem var !Preprocessing.stack_allocated_vars)
  in

  let reachable, unreachable = Formula.split_by_reachability args formula in
  let reachable_vars =
    reachable |> Formula.get_fresh_vars
    |> List.filter (fun var -> Formula.is_spatial_target var unreachable)
  in

  let rename_anchors (formula : Formula.t) =
    List.fold_left2
      (fun formula param arg ->
        Formula.substitute ~var:(get_anchor param) ~by:arg formula)
      formula params args
  in

  let remove_anchors (formula : Formula.t) =
    List.fold_left
      (fun formula var ->
        Formula.substitute ~var:(get_anchor var) ~by:var formula)
      formula reachable_vars
  in

  let convert_back_result (result : Formula.t) =
    let result = (result |> rename_anchors |> remove_anchors) @ unreachable in

    (match lhs_opt with
    | Some lhs ->
        let return_var =
          match return_stmt.skind with
          | Return (Some { enode = Lval (Var var, NoOffset); _ }, _) ->
              SL.Variable.mk var.vname (SL.Variable.get_sort lhs)
          | _ ->
              fail
                "function which is supposed to return something does not have \
                 a return value"
        in
        result
        |> Formula.substitute_by_fresh lhs
        |> Formula.substitute ~var:return_var ~by:lhs
    | None -> result)
    |> Simplification.remove_ptos_from_vars end_of_scope_stack_vars
    |> Simplification.convert_vars_to_fresh locals
  in

  let reachable =
    List.fold_left2
      (fun formula param arg ->
        formula
        (* add anchor variables *)
        |> Formula.add_eq param (get_anchor param)
        (* substitute argument names with parameter names *)
        |> Formula.substitute ~var:arg ~by:param)
      reachable params args
  in

  (* add anchors for variables reachable from the unreachable part of formula *)
  let reachable =
    List.fold_left
      (fun formula var -> Formula.add_eq var (get_anchor var) formula)
      reachable reachable_vars
  in

  let result_state = get_result_state func reachable in

  Config.Self.debug ~current:true ~dkey:Printing.func_call
    "input formula:\n%a\nreachable subformula:\n%a\nunreachable subformula:\n%a"
    Formula.pp_formula formula Formula.pp_formula reachable Formula.pp_formula
    unreachable;

  List.map convert_back_result result_state

let func_call (args : Formula.var list) (func : varinfo) (formula : Formula.t)
    (lhs_opt : Formula.var option) : Formula.state =
  try func_call args func formula lhs_opt
  with Kernel_function.No_Definition | Kernel_function.No_Statement ->
    warning "skipping function %s (no definition)" func.vname;
    [ formula ]

let merge_all_results () =
  List.iter
    (Hashtbl.iter (Hashtbl.replace !function_context.results))
    !previous_results
