open Cil_types

type instr_type =
  | Assign_simple of varinfo * varinfo
  | Assign_rhs_field of varinfo * varinfo * fieldinfo
  | Assign_lhs_field of varinfo * fieldinfo * varinfo
  | Assign_deref_rhs of varinfo * varinfo
  | Assign_deref_lhs of varinfo * varinfo
  | Assign_ref of varinfo * varinfo
  | Call of varinfo option * varinfo * varinfo list
  | ComplexInstr
  | Ignored

let get_instr_type (instr : instr) : instr_type =
  let get_func_params (params : exp list) : varinfo list option =
    let vars =
      List.map
        (fun param ->
          match param.enode with
          | Lval (Var var, NoOffset) -> Some var
          | _ -> None)
        params
    in
    if List.for_all Option.is_some vars then Some (List.map Option.get vars)
    else None
  in

  match instr with
  | Set (lval, exp, _) -> (
      match (lval, exp.enode) with
      (* var = var; *)
      | (Var lhs, NoOffset), Lval (Var rhs, NoOffset) -> Assign_simple (lhs, rhs)
      (* var = var->field; *)
      | ( (Var lhs, NoOffset),
          Lval
            ( Mem { enode = Lval (Var rhs, NoOffset); _ },
              Field (rhs_field, NoOffset) ) ) ->
          Assign_rhs_field (lhs, rhs, rhs_field)
      (* var->field = var; *)
      | ( ( Mem { enode = Lval (Var lhs, NoOffset); _ },
            Field (lhs_field, NoOffset) ),
          Lval (Var rhs, NoOffset) ) ->
          Assign_lhs_field (lhs, lhs_field, rhs)
      (* *var = var; *)
      | ( (Mem { enode = Lval (Var lhs, NoOffset); _ }, NoOffset),
          Lval (Var rhs, NoOffset) ) ->
          Assign_deref_lhs (lhs, rhs)
      (* var = *var; *)
      | ( (Var lhs, NoOffset),
          Lval (Mem { enode = Lval (Var rhs, NoOffset); _ }, NoOffset) ) ->
          Assign_deref_rhs (lhs, rhs)
      (* var = &var; *)
      | (Var lhs, NoOffset), AddrOf (Var rhs, NoOffset) -> Assign_ref (lhs, rhs)
      | _ -> ComplexInstr)
  | Call (lval_opt, func, params, _) -> (
      match (lval_opt, func.enode, get_func_params params) with
      (* func(...) *)
      | None, Lval (Var func, NoOffset), Some params ->
          Call (None, func, params) (* var = func(...) *)
      | Some (Var lhs, NoOffset), Lval (Var func, NoOffset), Some params ->
          Call (Some lhs, func, params)
      | _ -> ComplexInstr)
  | _ -> Ignored
