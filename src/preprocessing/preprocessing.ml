open Cil
open Cil_types
open Common
open Astral
module Printer = Frama_c_kernel.Printer
open Constants

let remove_casts =
  object
    inherit nopCilVisitor

    method! vexpr expr =
      match expr.enode with
      | CastE (_, inner) -> ChangeDoChildrenPost (inner, fun x -> x)
      | _ -> DoChildren
  end

let replace_constants =
  object
    inherit Visitor.frama_c_inplace

    method! vexpr (expr : exp) =
      match expr.enode with
      | CastE (TPtr (_, _), { enode = Const _; _ }) ->
          ChangeTo (evar nullptr_var)
      | Const _ | SizeOf _ | SizeOfE _ | SizeOfStr _ ->
          ChangeTo (evar const_var)
      | _ -> DoChildren
  end

let remove_local_init =
  object
    inherit Visitor.frama_c_inplace

    method! vinst (instr : instr) =
      match instr with
      | Local_init (varinfo, local_init, location) -> (
          let lval = (Var varinfo, NoOffset) in
          match local_init with
          (* Type var = exp; *)
          | AssignInit (SingleInit exp) ->
              ChangeTo [ Ast_info.mkassign lval exp location ]
          (* Type var = func(); *)
          | ConsInit (func, params, Plain_func) ->
              ChangeTo [ Call (Some lval, evar func, params, location) ]
          | _ -> fail "Unsupported instruction: %a" Printer.pp_instr instr)
      | _ -> SkipChildren
  end

let remove_non_list_stmts =
  object
    inherit Visitor.frama_c_inplace

    method! vinst (instr : instr) =
      let loc = Cil_datatype.Instr.loc instr in
      let skip = Skip loc in
      let assert_allocated param =
        Ast_info.mkassign (Var const_var, NoOffset) (evar param) loc
      in
      let is_list_type var =
        var.vname = null_var_name || Types.is_struct_ptr var.vtype
      in
      match Instruction_type.get_instr_type instr with
      | Assign_simple (lhs, _) when is_list_type lhs -> SkipChildren
      | Assign_simple (_, _) -> ChangeTo [ skip ]
      | Assign_rhs_field (lhs, _, _) when is_list_type lhs -> SkipChildren
      | Assign_rhs_field (_, rhs, _) when is_list_type rhs ->
          ChangeTo [ assert_allocated rhs ]
      | Assign_lhs_field (_, _, rhs) when is_list_type rhs -> SkipChildren
      | Assign_lhs_field (lhs, _, _) when is_list_type lhs ->
          ChangeTo [ assert_allocated lhs ]
      | Call (Some lhs, _, _) when is_list_type lhs -> SkipChildren
      | Call (_, fn, args) ->
          ChangeTo [ Call (None, evar fn, List.map evar args, loc) ]
      | _ -> ChangeTo [ skip ]
  end

let remove_useless_assignments =
  object
    inherit Visitor.frama_c_inplace

    method! vinst (instr : instr) =
      match Instruction_type.get_instr_type instr with
      | Assign_simple (lhs, rhs) when lhs.vname = rhs.vname ->
          ChangeTo [ Skip (Cil_datatype.Instr.loc instr) ]
      | _ -> SkipChildren
  end

let unique_names (functions : fundec list) =
  object (self)
    inherit Visitor.frama_c_inplace

    method! vvrbl (var : varinfo) =
      let fundec = self#current_func |> Option.get in
      let other_funcs =
        List.filter (fun f -> f.svar.vid <> fundec.svar.vid) functions
      in
      List.iter (fun f -> refresh_local_name f var) other_funcs;
      SkipChildren
  end

let remove_unused_call_args =
  object
    inherit Visitor.frama_c_inplace

    method! vinst (instr : instr) =
      match instr with
      | Call (lval_opt, fn, args, location) ->
          let args =
            List.filter (fun arg -> typeOf arg |> Types.is_struct_ptr) args
          in
          ChangeTo [ Call (lval_opt, fn, args, location) ]
      | _ -> SkipChildren
  end

let preprocess () =
  let file = Ast.get () in

  uniqueVarNames file;

  let functions =
    List.filter_map
      (function GFun (func, _) -> Some func | _ -> None)
      file.globals
  in

  Visitor.visitFramacFileFunctions (unique_names functions) file;
  Visitor.visitFramacFileFunctions replace_constants file;
  visitCilFileFunctions remove_casts file;
  Visitor.visitFramacFileFunctions remove_local_init file;
  Visitor.visitFramacFileFunctions remove_unused_call_args file;
  Visitor.visitFramacFileFunctions Stmt_split.split_complex_stmts file;
  Visitor.visitFramacFileFunctions Condition_split.split_conditions file;
  Visitor.visitFramacFileFunctions remove_useless_assignments file;
  Visitor.visitFramacFileFunctions remove_non_list_stmts file;
  Types.process_types file;

  (* this must run after adding statements *)
  Ast.mark_as_changed ();
  Cfg.clearFileCFG file;
  Cfg.computeFileCFG file;
  Async.yield ()
