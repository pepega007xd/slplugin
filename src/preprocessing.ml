open Cil

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
      Printer.pp_stmt Format.std_formatter stmt;
      Printer.pp_file Format.std_formatter (Ast.get ());
      print_newline ();
      SkipChildren
  end

let preprocess () =
  let file = Ast.get () in
  ignore (visitCilFileSameGlobals (new remove_casts) file)
