let print_state (fmt : Format.formatter) (loc : Printer_tag.localizable) : unit
    =
  let result =
    match loc with
    | Printer_tag.PStmtStart (_, stmt) ->
        Hashtbl.find_opt !Func_call.results stmt
    | _ -> None
  in
  Option.iter (Formula.pp_state fmt) result

let () =
  Server.Kernel_ast.Information.register ~id:"slplugin.stmt_state"
    ~label:"state" ~title:"final state"
    ~descr:"final state reached for this statement" print_state

let print_type_heuristic (fmt : Format.formatter)
    (loc : Printer_tag.localizable) : unit =
  let get_list_type typ =
    match Preprocessing.get_struct_type typ with
    | Sll -> Some "Singly linked list"
    | Dll -> Some "Doubly linked list"
    | Nl -> Some "Nested list"
    | Other -> Some "Not a list"
  in
  let result =
    match loc with
    | Printer_tag.PType typ -> get_list_type typ
    | Printer_tag.PVDecl (_, _, var) -> get_list_type var.vtype
    | _ -> None
  in
  Option.iter (Format.pp_print_string fmt) result

let () =
  Server.Kernel_ast.Information.register ~id:"slplugin.type_heuristic"
    ~label:"type heurisic" ~title:"type heuristic"
    ~descr:"result of type heurisitic (which list type is this?)"
    print_type_heuristic

let print_type_heuristic_on_field (fmt : Format.formatter)
    (loc : Printer_tag.localizable) : unit =
  let result =
    match loc with
    | Printer_tag.PLval (_, _, (_, Field (field, _))) -> (
        match Preprocessing.get_field_type field with
        | Next -> Some "next"
        | Prev -> Some "prev"
        | Top -> Some "top"
        | Other name -> Some ("other: " ^ name)
        | Data -> Some "data")
    | _ -> None
  in
  Option.iter (Format.pp_print_string fmt) result

let () =
  Server.Kernel_ast.Information.register ~id:"slplugin.field_type_heuristic"
    ~label:"field type heurisic" ~title:"field type heuristic"
    ~descr:"result of field type heurisitic (which list field type is this?)"
    print_type_heuristic_on_field

let print_statement_id (fmt : Format.formatter) (loc : Printer_tag.localizable)
    : unit =
  match loc with
  | Printer_tag.PStmt (_, stmt) -> Format.pp_print_int fmt stmt.sid
  | _ -> ()

let () =
  Server.Kernel_ast.Information.register ~id:"slplugin.statement_id"
    ~label:"statement id" ~title:"statement id" print_statement_id
