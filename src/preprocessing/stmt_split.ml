open Cil_types
open Cil
open Common

(* breaks down a complex assignment stmt into a series of simpler ones *)
let split_assignment (func : fundec) (lhs : lval) (rhs : lval)
    (location : location) : stmtkind =
  let block = mkBlock [] in
  let var_offset_to_exp, lval_to_var =
    Block_builder.get_utility_functions func block location
  in

  let assign_stmt =
    match lhs with
    (* var = exp; *)
    | Var _, NoOffset -> (
        (* lhs is already a variable - we can leave a single offset on rhs *)
        match rhs with
        (* var = var; case is unreachable, [convert_set] would not be run on simple stmt *)
        | Mem { enode = Lval inner_lval; _ }, Field (fieldinfo, NoOffset) ->
            let inner_rhs_var, _ = lval_to_var inner_lval in
            let rhs_var_exp =
              var_offset_to_exp inner_rhs_var (Field (fieldinfo, NoOffset))
            in
            Ast_info.mkassign_statement lhs rhs_var_exp location
        | _ -> fail "Unsupported lval: %a" Printer.pp_lval rhs)
    (* var->field->... = exp; *)
    | Mem { enode = Lval inner_lval; _ }, Field (fieldinfo, NoOffset) ->
        (* lhs has an offset - we must convert rhs to a variable without offset *)
        let inner_lhs_var, _ = lval_to_var inner_lval in
        let rhs_var, _ = lval_to_var rhs in

        let rhs_var_exp = evar ~loc:location rhs_var in

        let inner_lhs_exp = evar ~loc:location inner_lhs_var in
        Ast_info.mkassign_statement
          (Mem inner_lhs_exp, Field (fieldinfo, NoOffset))
          rhs_var_exp location
    | _ -> fail "Unsupported lval: %a" Printer.pp_lval lhs
  in

  block.bstmts <- List.rev @@ (assign_stmt :: block.bstmts);
  Block block

let split_call (outer_func : fundec) (lhs_opt : lval option) (func_exp : exp)
    (params : exp list) (location : location) : stmtkind =
  let block = mkBlock [] in
  let _, lval_to_var =
    Block_builder.get_utility_functions outer_func block location
  in

  (* simplify parameters to variables *)
  let convert_param (param : exp) : exp =
    match param.enode with
    | Lval (Var _, NoOffset) -> param
    | Lval lval ->
        let var, _ = lval_to_var lval in
        evar ~loc:location var
    | _ -> fail "Unsupported exp: %a" Printer.pp_exp param
  in
  let params = List.map convert_param params in

  let last_stmt =
    match lhs_opt with
    | Some (Mem { enode = Lval inner_lval; _ }, Field (fieldinfo, NoOffset)) ->
        (* simplify lhs, to which call result is assigned - last stmt is assignment *)
        let lval_var, orig_name = lval_to_var inner_lval in

        let lval_exp = evar ~loc:location lval_var in

        let call_result_var =
          makeLocalVar outer_func ~scope:block
            (get_unique_name orig_name)
            fieldinfo.ftype
        in

        let call_instr =
          Call (Some (Var call_result_var, NoOffset), func_exp, params, location)
        in
        let new_call_stmt = mkStmtOneInstr ~valid_sid:true call_instr in
        block.bstmts <- new_call_stmt :: block.bstmts;

        Ast_info.mkassign_statement
          (Mem lval_exp, Field (fieldinfo, NoOffset))
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
            | Set (lhs, { enode = Lval rhs; _ }, location) ->
                let lhs_lhost, _ = lhs in
                let rhs_lhost, _ = rhs in
                if
                  (Types.is_struct_ptr @@ typeOfLhost lhs_lhost)
                  || (Types.is_struct_ptr @@ typeOfLhost rhs_lhost)
                then split_assignment fundec lhs rhs location
                else Instr (Skip location)
            | Call (lval_opt, func_exp, params, location) ->
                split_call fundec lval_opt func_exp params location
            | _ -> stmt.skind
          in
          stmt.skind <- new_stmtkind;
          SkipChildren
      | _ -> DoChildren
  end
