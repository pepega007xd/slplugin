open Cil_types
open Cil
open Common

(** This module implements the splitting of assignments and function calls into
    basic instructions *)

(** Break down a complex assignment stmt into a series of simpler ones *)
let split_assignment (func : fundec) (lhs : lval) (rhs : exp)
    (location : location) : stmtkind =
  let block = mkBlock [] in
  let var_offset_to_exp, exp_to_var =
    Block_builder.get_block_builder func block location
  in

  let assign_stmt =
    match (lhs, rhs.enode) with
    (* var = exp; *)
    | (Var _, NoOffset), Lval (Mem inner_rhs, offset) ->
        (* lhs is already a variable - we can leave a single offset on rhs *)
        (* var = var; case is unreachable, [convert_set] would not be run on simple stmt *)
        let inner_rhs_var, _ = exp_to_var inner_rhs in
        let rhs_var_exp = var_offset_to_exp inner_rhs_var offset in
        Ast_info.mkassign_statement lhs rhs_var_exp location
    (* var->field->... = exp; *)
    | (Mem inner_lhs, offset), _ ->
        (* lhs has an offset - we must convert rhs to a variable without offset *)
        let inner_lhs_var, _ = exp_to_var inner_lhs in
        let rhs_var, _ = exp_to_var rhs in

        let rhs_var_exp = evar ~loc:location rhs_var in

        let inner_lhs_exp = evar ~loc:location inner_lhs_var in
        Ast_info.mkassign_statement
          (Mem inner_lhs_exp, offset)
          rhs_var_exp location
    | _ -> fail "Unsupported lval: %a" Printer.pp_lval lhs
  in

  block.bstmts <- List.rev @@ (assign_stmt :: block.bstmts);
  Block block

let split_call (outer_func : fundec) (lhs_opt : lval option) (func_exp : exp)
    (params : exp list) (location : location) : stmtkind =
  let block = mkBlock [] in
  let _, exp_to_var =
    Block_builder.get_block_builder outer_func block location
  in

  (* simplify parameters to variables *)
  let params =
    List.map (fun param -> exp_to_var param |> fst |> evar ~loc:location) params
  in

  let last_stmt =
    match lhs_opt with
    | Some (Mem inner_exp, offset) ->
        (* simplify lhs, to which call result is assigned - last stmt is assignment *)
        let lval_var, orig_name = exp_to_var inner_exp in

        let lval_exp = evar ~loc:location lval_var in

        let call_result_var =
          makeLocalVar outer_func ~scope:block
            (get_unique_name orig_name)
            (Option.get lhs_opt |> typeOfLval)
        in

        let call_instr =
          Call (Some (Var call_result_var, NoOffset), func_exp, params, location)
        in
        let new_call_stmt = mkStmtOneInstr ~valid_sid:true call_instr in
        block.bstmts <- new_call_stmt :: block.bstmts;

        Ast_info.mkassign_statement (Mem lval_exp, offset)
          (evar ~loc:location call_result_var)
          location
    | lhs ->
        (* recreate the call stmt with simplified params - last stmt is call *)
        let call_instr = Call (lhs, func_exp, params, location) in
        mkStmtOneInstr ~valid_sid:true call_instr
  in

  block.bstmts <- List.rev @@ (last_stmt :: block.bstmts);
  Block block

let split_complex_stmts =
  object (self)
    inherit Visitor.frama_c_inplace

    method! vstmt_aux (stmt : stmt) =
      let fundec = self#current_func |> Option.get in

      match stmt.skind with
      | Instr instr when Instruction_type.get_instr_type instr = ComplexInstr ->
          let new_stmtkind =
            match instr with
            | Set (lhs, rhs, location) -> (
                match rhs.enode with
                (* lhs lhost is ptr type -> includes [expr = &var] *)
                | _ when Types.is_relevant_type @@ typeOfLhost @@ fst lhs ->
                    split_assignment fundec lhs rhs location
                (* rhs lhost is ptr type -> includes [int x = var->data] *)
                | Lval rhs_lval
                  when Types.is_relevant_type @@ typeOfLhost @@ fst rhs_lval ->
                    split_assignment fundec lhs rhs location
                | _ -> Instr (Skip location))
            | Call (lval_opt, func_exp, params, location) ->
                split_call fundec lval_opt func_exp params location
            | _ -> stmt.skind
          in
          stmt.skind <- new_stmtkind;
          SkipChildren
      | _ -> DoChildren
  end
