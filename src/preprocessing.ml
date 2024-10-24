open Cil
open Cil_types
open Common
open Astral
module Printer = Frama_c_kernel.Printer

let null_var_name = "_nil"
let const_var_name = "_const"
let nullptr_var = makeVarinfo false false null_var_name voidPtrType
let const_var = makeVarinfo false false const_var_name voidPtrType

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

type list_type = Sll | Dll | Nl | Other
type field_type = Next | Prev | Top | Data

let rec get_self_and_sll_fields (structure : compinfo) :
    fieldinfo list * fieldinfo list =
  let fields = Option.get structure.cfields in

  let self_pointers, other_pointers =
    List.filter
      (fun field ->
        match field.ftype with TPtr (TComp (_, _), _) -> true | _ -> false)
      fields
    |> List.partition (fun field ->
           match field.ftype with
           | TPtr (TComp (target_struct, _), _) ->
               target_struct.ckey = structure.ckey
           | _ -> false)
  in

  let sll_pointers =
    List.filter
      (fun field ->
        match field.ftype with
        | TPtr (typ, _) -> get_list_type typ = Sll
        | _ -> false)
      other_pointers
  in
  (self_pointers, sll_pointers)

and get_list_type (t : typ) : list_type =
  match unrollTypeDeep t with
  | TPtr (TComp (structure, _), _) | TComp (structure, _) -> (
      let self_pointers, sll_pointers = get_self_and_sll_fields structure in

      match (self_pointers, sll_pointers) with
      | [ _ ], [] -> Sll
      | [ _ ], [ _ ] -> Nl
      | [ _; _ ], [] -> Dll
      | _ -> Other)
  | _ -> Other

let is_list_type (t : typ) : bool =
  match get_list_type t with Sll | Dll | Nl -> true | Other -> false

let list_type_to_sort : list_type -> Sort.t = function
  | Sll -> Sort.loc_ls
  | Dll -> Sort.loc_dls
  | Nl -> Sort.loc_nls
  | Other -> Sort.loc_nil (* this crashes astral when assigned to a variable *)

let varinfo_to_var (varinfo : Cil_types.varinfo) : SSL.Variable.t =
  if varinfo.vname = null_var_name then SSL.Variable.nil
  else
    SSL.Variable.mk varinfo.vname
      (varinfo.vtype |> get_list_type |> list_type_to_sort)

and get_field_type (field : fieldinfo) : field_type =
  let self_pointers, sll_pointers = get_self_and_sll_fields field.fcomp in

  match (self_pointers, sll_pointers) with
  (* SLL *)
  | [ next ], [] when field.forder = next.forder -> Next
  (* DLL *)
  | [ next; _ ], [] when field.forder = next.forder -> Next
  | [ _; prev ], [] when field.forder = prev.forder -> Prev
  (* NL *)
  | [ top ], [ _ ] when field.forder = top.forder -> Top
  | [ _ ], [ next ] when field.forder = next.forder -> Next
  | _ -> Data

let unique_counter = ref 0

let get_unique_name (name : string) : string =
  unique_counter := !unique_counter + 1;
  name ^ "_" ^ string_of_int !unique_counter

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

(* breaks down a complex assignment stmt into a series of simpler ones *)
let convert_set (func : fundec) (lhs : lval) (rhs : lval) (location : location)
    : stmtkind =
  let block = mkBlock [] in
  let var_offset_to_exp, lval_to_var =
    get_utility_functions func block location
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

let convert_call (outer_func : fundec) (lhs_opt : lval option) (func_exp : exp)
    (params : exp list) (location : location) : stmtkind =
  let block = mkBlock [] in
  let _, lval_to_var = get_utility_functions outer_func block location in

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
        (* recreate the call stmt with simplified params *)
        let call_instr = Call (lhs, func_exp, params, location) in
        mkStmtOneInstr ~valid_sid:true call_instr
  in

  block.bstmts <- List.rev @@ (last_stmt :: block.bstmts);
  Block block

type instr_type =
  | Assign_simple of varinfo * varinfo
  | Assign_rhs_field of varinfo * varinfo * fieldinfo
  | Assign_lhs_field of varinfo * fieldinfo * varinfo
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

let convert_condition (func : fundec) (condition : exp) (th : block)
    (el : block) (location : location) : stmtkind =
  let block = mkBlock [] in
  let _, lval_to_var = get_utility_functions func block location in

  let new_if condition =
    If (new_exp ~loc:location condition, th, el, location)
  in
  let nondeterministic = new_if (Lval (Var const_var, NoOffset)) in

  let new_if_stmt =
    match condition.enode with
    | Lval lval ->
        let var, _ = lval_to_var lval in
        new_if (BinOp (Ne, evar var, evar nullptr_var, intType))
    | UnOp (BNot, { enode = Lval lval; _ }, _) ->
        let var, _ = lval_to_var lval in
        new_if (BinOp (Eq, evar var, evar nullptr_var, intType))
    | BinOp (operator, { enode = Lval lhs; _ }, { enode = Lval rhs; _ }, _) ->
        let lhs, _ = lval_to_var lhs in
        let rhs, _ = lval_to_var rhs in
        new_if (BinOp (operator, evar lhs, evar rhs, intType))
    | _ -> nondeterministic
  in
  block.bstmts <-
    List.rev @@ (mkStmt ~valid_sid:true new_if_stmt :: block.bstmts);
  Block block

let remove_local_init =
  object
    inherit Visitor.frama_c_inplace

    method! vstmt_aux (stmt : stmt) =
      match stmt.skind with
      | Instr (Local_init (varinfo, local_init, location)) -> (
          let lval = (Var varinfo, NoOffset) in
          match local_init with
          (* Type var = exp; *)
          | AssignInit (SingleInit exp) ->
              stmt.skind <- Instr (Ast_info.mkassign lval exp location);
              SkipChildren
          (* Type var = func(); *)
          | ConsInit (func, params, Plain_func) ->
              stmt.skind <-
                Instr (Call (Some lval, evar func, params, location));
              SkipChildren
          | _ -> fail "Unsupported statement: %a" Printer.pp_stmt stmt)
      | _ -> DoChildren
  end

let split_complex_stmts =
  object (self)
    inherit Visitor.frama_c_inplace

    method! vstmt_aux (stmt : stmt) =
      let fundec = self#current_func |> Option.get in

      match stmt.skind with
      | Instr instr when get_instr_type instr = ComplexInstr ->
          let new_stmtkind =
            match instr with
            | Set (lhs, { enode = Lval rhs; _ }, location) ->
                let lhs_lhost, _ = lhs in
                let rhs_lhost, _ = rhs in

                if
                  (is_list_type @@ typeOfLhost lhs_lhost)
                  || (is_list_type @@ typeOfLhost rhs_lhost)
                then convert_set fundec lhs rhs location
                else Instr (Skip location)
            | Call (lval_opt, func_exp, params, location) ->
                convert_call fundec lval_opt func_exp params location
            | _ -> stmt.skind
          in
          stmt.skind <- new_stmtkind;
          SkipChildren
      | _ -> DoChildren
  end

let remove_non_list_stmts =
  object
    inherit Visitor.frama_c_inplace

    method! vstmt_aux (stmt : stmt) =
      match stmt.skind with
      | Instr instr ->
          let loc = Cil_datatype.Instr.loc instr in
          let skip = Skip loc in
          let assert_allocated param =
            Ast_info.mkassign (Var const_var, NoOffset) (evar param) loc
          in
          let is_list_type var =
            var.vname = null_var_name || is_list_type var.vtype
          in
          let new_stmtkind =
            match get_instr_type instr with
            | Assign_simple (lhs, _) when is_list_type lhs -> instr
            | Assign_simple (_, _) -> skip
            | Assign_rhs_field (lhs, _, _) when is_list_type lhs -> instr
            | Assign_rhs_field (_, rhs, _) when is_list_type rhs ->
                assert_allocated rhs
            | Assign_lhs_field (_, _, rhs) when is_list_type rhs -> instr
            | Assign_lhs_field (lhs, _, _) when is_list_type lhs ->
                assert_allocated lhs
            | _ -> instr
          in
          stmt.skind <- Instr new_stmtkind;
          SkipChildren
      | _ -> DoChildren
  end

let simplify_conditions =
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
                if is_list_type var.vtype then
                  new_if (BinOp (Ne, evar var, evar nullptr_var, intType))
                else nondeterministic
            | UnOp (BNot, { enode = Lval (Var var, NoOffset); _ }, _) ->
                if is_list_type var.vtype then
                  new_if (BinOp (Eq, evar var, evar nullptr_var, intType))
                else nondeterministic
            | BinOp
                ( (Eq | Ne),
                  { enode = Lval (Var lhs, NoOffset); _ },
                  { enode = Lval (Var _, NoOffset); _ },
                  _ ) ->
                if is_list_type lhs.vtype then stmt.skind else nondeterministic
            | _ -> convert_condition func condition th el location
          in
          stmt.skind <- new_stmtkind;
          SkipChildren
      | _ -> DoChildren
  end

let remove_useless_assignments =
  object
    inherit Visitor.frama_c_inplace

    method! vstmt_aux (stmt : stmt) =
      match stmt.skind with
      | Instr instr ->
          (match get_instr_type instr with
          | Assign_simple (lhs, rhs) when lhs.vname = rhs.vname ->
              stmt.skind <- Instr (Skip (Cil_datatype.Instr.loc instr))
          | _ -> ());
          SkipChildren
      | _ -> DoChildren
  end

let preprocess () =
  let file = Ast.get () in

  uniqueVarNames file;

  Visitor.visitFramacFileFunctions replace_constants file;
  visitCilFileFunctions remove_casts file;
  Visitor.visitFramacFileFunctions remove_local_init file;
  Visitor.visitFramacFileFunctions split_complex_stmts file;
  Visitor.visitFramacFileFunctions remove_non_list_stmts file;
  Visitor.visitFramacFileFunctions simplify_conditions file;
  Visitor.visitFramacFileFunctions remove_useless_assignments file;

  (* this must run after adding statements *)
  Ast.mark_as_changed ();
  Cfg.clearFileCFG file;
  Cfg.computeFileCFG file
