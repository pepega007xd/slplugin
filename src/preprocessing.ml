open Cil_types
open Cil
module M = Plugin.Register

class remove_casts =
  object
    inherit Cil.nopCilVisitor

    method! vexpr expr =
      match expr.enode with
      | CastE (ty, inner) -> ChangeDoChildrenPost (inner, fun x -> x)
      | other -> DoChildren
  end

let remove_casts () =
  let file = Ast.get () in
  ignore (visitCilFileSameGlobals (new remove_casts) file)
