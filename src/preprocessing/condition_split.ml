open Cil_types
open Cil
open Constants

let convert_condition (func : fundec) (condition : exp) (th : block)
    (el : block) (location : location) : stmtkind =
  let block = mkBlock [] in
  let _, lval_to_var =
    Block_builder.get_utility_functions func block location
  in

  let new_if condition =
    If (new_exp ~loc:location condition, th, el, location)
  in
  let nondeterministic = new_if (Lval (Var const_var, NoOffset)) in

  let new_if_stmt =
    match condition.enode with
    | Lval lval ->
        let var, _ = lval_to_var lval in
        new_if (BinOp (Ne, evar var, evar nullptr_var, Cil_const.intType))
    | UnOp (BNot, { enode = Lval lval; _ }, _) ->
        let var, _ = lval_to_var lval in
        new_if (BinOp (Eq, evar var, evar nullptr_var, Cil_const.intType))
    | BinOp (operator, { enode = Lval lhs; _ }, { enode = Lval rhs; _ }, _) ->
        let lhs, _ = lval_to_var lhs in
        let rhs, _ = lval_to_var rhs in
        new_if (BinOp (operator, evar lhs, evar rhs, Cil_const.intType))
    | _ -> nondeterministic
  in
  block.bstmts <-
    List.rev @@ (mkStmt ~valid_sid:true new_if_stmt :: block.bstmts);
  Block block

let split_conditions =
  object (self)
    inherit Visitor.frama_c_inplace

    method! vstmt_aux (stmt : stmt) =
      let func = self#current_func |> Option.get in
      match stmt.skind with
      | If (condition, th, el, location) ->
          let new_if condition =
            If (new_exp ~loc:location condition, th, el, location)
          in
          let nondeterministic = new_if (Lval (Var const_var, NoOffset)) in
          let new_stmtkind =
            match condition.enode with
            | Lval (Var var, NoOffset) ->
                if Types.is_struct_ptr var.vtype then
                  new_if
                    (BinOp (Ne, evar var, evar nullptr_var, Cil_const.intType))
                else nondeterministic
            | UnOp (BNot, { enode = Lval (Var var, NoOffset); _ }, _) ->
                if Types.is_struct_ptr var.vtype then
                  new_if
                    (BinOp (Eq, evar var, evar nullptr_var, Cil_const.intType))
                else nondeterministic
            | BinOp
                ( (Eq | Ne),
                  { enode = Lval (Var lhs, NoOffset); _ },
                  { enode = Lval (Var _, NoOffset); _ },
                  _ ) ->
                if Types.is_struct_ptr lhs.vtype then stmt.skind
                else nondeterministic
            | _ -> convert_condition func condition th el location
          in
          stmt.skind <- new_stmtkind;
          DoChildren
      | _ -> DoChildren
  end
