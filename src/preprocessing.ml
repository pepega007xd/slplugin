open Cil
open Cil_types
open Common

let null_var_name = "_nil"

class remove_casts =
  object
    inherit Cil.nopCilVisitor

    method! vexpr expr =
      match expr.enode with
      | CastE (_, inner) -> ChangeDoChildrenPost (inner, fun x -> x)
      | _ -> DoChildren
  end

class print_source =
  object
    inherit Cil.nopCilVisitor

    method! vstmt stmt =
      print_int stmt.sid;
      print_string ": ";
      Printer.pp_stmt Format.std_formatter stmt;
      print_newline ();
      SkipChildren
  end

let get_dummy_location () : Cil_types.location =
  let dummy_position : Filepath.position =
    { pos_path = Filepath.pwd (); pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }
  in
  (dummy_position, dummy_position)

class replace_nulls =
  object
    inherit Visitor.frama_c_inplace

    method! vexpr (expr : exp) =
      let nullptr_var = makeVarinfo false false null_var_name voidPtrType in
      match expr.enode with
      | Const (CInt64 (_, _, _)) -> ChangeTo (evar nullptr_var)
      | _ -> DoChildren
  end

class insert_nullptr_variable =
  object (self)
    inherit Visitor.frama_c_inplace

    method! vstmt_aux (stmt : stmt) =
      let func = Option.get self#current_func in
      match func.sbody.bstmts with
      | first :: _ when first.sid = stmt.sid ->
          let nullptr_varinfo = makeLocalVar func null_var_name voidPtrType in

          let nullptr_lval : lval = (Var nullptr_varinfo, NoOffset) in
          let dummy_location = get_dummy_location () in
          let zero = Cil.zero ~loc:dummy_location in
          let assign_nullptr =
            Ast_info.mkassign nullptr_lval zero dummy_location
          in

          self#queueInstr [ assign_nullptr ];

          DoChildren
      | _ -> DoChildren
  end

let is_next_field (fieldinfo : fieldinfo) : bool =
  let this_struct = fieldinfo.fcomp in
  match fieldinfo.ftype with
  | TPtr (TComp (target_struct, _), _) -> target_struct.ckey = this_struct.ckey
  | _ -> false

class field_access_to_deref =
  object (self)
    inherit Visitor.frama_c_inplace

    method! vlval (lval : lval) =
      let lhost, offset = lval in
      match offset with
      | Field (fieldinfo, next_offset) ->
          if is_next_field fieldinfo then ChangeTo (lhost, next_offset)
          else
            let stmt = Option.get self#current_stmt in
            stmt.skind <- Instr (Skip (get_dummy_location ()));
            DoChildren
      | _ -> DoChildren
  end

class get_local_vars =
  object
    inherit Visitor.frama_c_inplace
    val mutable locals : string list list = []

    method! vblock (block : block) =
      let this_block_locals =
        List.map (fun local -> local.vname) block.blocals
      in
      locals <- this_block_locals :: locals;
      List.iter
        (fun stmt ->
          Hashtbl.add !local_vars_for_stmt stmt
          @@ StringSet.of_list @@ List.flatten locals)
        block.bstmts;
      ChangeDoChildrenPost
        ( block,
          fun block ->
            (match locals with
            | _ :: rest -> locals <- rest
            | [] -> Common.fail "list of scopes should not be empty");
            block )
  end

let preprocess () =
  let file = Ast.get () in

  uniqueVarNames file;

  Visitor.visitFramacFileFunctions (new replace_nulls) file;

  Visitor.visitFramacFileFunctions (new insert_nullptr_variable) file;

  (* this must run after adding statements *)
  Ast.mark_as_changed ();
  Cfg.clearFileCFG file;
  Cfg.computeFileCFG file;

  Visitor.visitFramacFileFunctions (new field_access_to_deref) file;

  Visitor.visitFramacFileFunctions (new get_local_vars) file;

  visitCilFileFunctions (new remove_casts) file
