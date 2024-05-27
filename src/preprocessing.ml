open Cil
open Cil_types
open Common

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

class replace_nulls =
  object
    inherit Cil.nopCilVisitor

    method! vexpr (expr : exp) =
      let nullptr_var = makeVarinfo false false "__nullptr" voidPtrType in
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
          let nullptr_varinfo = makeLocalVar func "__nullptr" voidPtrType in

          let nullptr_lval : lval = (Var nullptr_varinfo, NoOffset) in
          let dummy_position : Filepath.position =
            {
              pos_path = Filepath.pwd ();
              pos_lnum = 0;
              pos_bol = 0;
              pos_cnum = 0;
            }
          in
          let dummy_location : Cil_types.location =
            (dummy_position, dummy_position)
          in
          let zero = Cil.zero ~loc:dummy_location in
          let assign_nullptr =
            Ast_info.mkassign_statement nullptr_lval zero dummy_location
          in

          let new_first_stmt = Cil.mkStmtCfgBlock [ assign_nullptr; first ] in

          ChangeTo new_first_stmt
      | _ -> DoChildren
  end

let preprocess () =
  let file = Ast.get () in

  visitCilFileSameGlobals (new replace_nulls) file;

  Visitor.visitFramacFileFunctions (new insert_nullptr_variable) file;
  (* this must run after adding statements *)
  Ast.mark_as_changed ();
  Cfg.clearFileCFG file;
  Cfg.computeFileCFG file;

  visitCilFileSameGlobals (new remove_casts) file
