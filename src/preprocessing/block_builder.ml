open Cil_types
open Cil
open Common

let get_block_builder (func : fundec) (block : block) (location : location) =
  let var_offset_to_exp (var : varinfo) (offset : offset) : exp =
    let inner_exp = evar ~loc:location var in
    new_exp ~loc:location (Lval (Mem inner_exp, offset))
  in

  let rec exp_to_var (exp : exp) : varinfo * string =
    match exp.enode with
    (* var *)
    | Lval (Var var, NoOffset) -> (var, var.vname)
    (* var->field->field->... *)
    | Lval (Mem inner_exp, offset) ->
        let inner_var, orig_name = exp_to_var inner_exp in
        let inner_var_exp = var_offset_to_exp inner_var offset in

        let new_var =
          makeLocalVar func ~scope:block
            (get_unique_name orig_name)
            (typeOf exp)
        in

        let assign_stmt =
          Ast_info.mkassign_statement (Var new_var, NoOffset) inner_var_exp
            location
        in

        block.bstmts <- assign_stmt :: block.bstmts;
        (new_var, orig_name)
    (* &var *)
    | AddrOf (Var inner_var, NoOffset) ->
        let new_var =
          makeLocalVar func ~scope:block
            (get_unique_name inner_var.vname)
            (typeOf exp)
        in

        let assign_stmt =
          Ast_info.mkassign_statement (Var new_var, NoOffset) exp location
        in

        block.bstmts <- assign_stmt :: block.bstmts;
        (new_var, inner_var.vname)
    | _ -> fail "Unsupported lval: %a" Printer.pp_exp exp
  in
  (var_offset_to_exp, exp_to_var)
