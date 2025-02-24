open Cil_types
open Cil
open Common

let get_utility_functions (func : fundec) (block : block) (location : location)
    =
  let var_offset_to_exp (var : varinfo) (offset : offset) : exp =
    let inner_exp = evar ~loc:location var in
    new_exp ~loc:location (Lval (Mem inner_exp, offset))
  in

  let rec lval_to_var (lval : lval) : varinfo * string =
    match lval with
    (* var *)
    | Var var, NoOffset -> (var, var.vname)
    (* var->field->field->... *)
    | Mem { enode = Lval inner_lval; _ }, Field (fieldinfo, _) ->
        let inner_var, orig_name = lval_to_var inner_lval in
        let inner_var_exp =
          var_offset_to_exp inner_var (Field (fieldinfo, NoOffset))
        in

        let new_var =
          makeLocalVar func ~scope:block
            (get_unique_name orig_name)
            fieldinfo.ftype
        in

        let assign_stmt =
          Ast_info.mkassign_statement (Var new_var, NoOffset) inner_var_exp
            location
        in

        block.bstmts <- assign_stmt :: block.bstmts;
        (new_var, orig_name)
    | _ -> fail "Unsupported lval: %a" Printer.pp_lval lval
  in
  (var_offset_to_exp, lval_to_var)
