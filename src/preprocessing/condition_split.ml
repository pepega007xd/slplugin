open Cil_types
open Cil
open Constants

let convert_condition (func : fundec) (condition : exp) (th : block)
    (el : block) (location : location) : stmtkind =
  let block = mkBlock [] in
  let _, exp_to_var = Block_builder.get_block_builder func block location in

  let new_if condition =
    If (new_exp ~loc:location condition, th, el, location)
  in
  let nondeterministic = new_if (Lval (Var const_var, NoOffset)) in

  let new_if_stmt =
    match condition.enode with
    | Lval _ ->
        let var, _ = exp_to_var condition in
        new_if (BinOp (Ne, evar var, evar nullptr_var, var.vtype))
    | UnOp (LNot, inner_exp, _) ->
        let var, _ = exp_to_var inner_exp in
        new_if (BinOp (Eq, evar var, evar nullptr_var, var.vtype))
    | BinOp (operator, lhs, rhs, _) ->
        let lhs, _ = exp_to_var lhs in
        let rhs, _ = exp_to_var rhs in
        if operator = Eq || operator = Ne then
          new_if (BinOp (operator, evar lhs, evar rhs, lhs.vtype))
        else nondeterministic
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
                if Types.is_relevant_var var then
                  new_if (BinOp (Ne, evar var, evar nullptr_var, var.vtype))
                else nondeterministic
            | UnOp (LNot, { enode = Lval (Var var, NoOffset); _ }, _) ->
                if Types.is_relevant_var var then
                  new_if (BinOp (Eq, evar var, evar nullptr_var, var.vtype))
                else nondeterministic
            | BinOp
                ( (Eq | Ne),
                  { enode = Lval (Var lhs, NoOffset); _ },
                  { enode = Lval (Var rhs, NoOffset); _ },
                  _ ) ->
                if Types.is_relevant_var lhs || Types.is_relevant_var rhs then
                  stmt.skind
                else nondeterministic
            | _ -> convert_condition func condition th el location
          in
          stmt.skind <- new_stmtkind;
          DoChildren
      | _ -> DoChildren
  end
