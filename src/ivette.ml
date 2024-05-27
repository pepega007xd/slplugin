let format_stmt (fmt : Format.formatter) (loc : Printer_tag.localizable) : unit
    =
  let result =
    match loc with
    | Printer_tag.PStmtStart (_, stmt) ->
        Option.to_list @@ Hashtbl.find_opt !Analysis.results stmt
        |> List.flatten
    | _ -> []
  in
  let result_string =
    match result with
    | [] -> "<no data>"
    | data -> List.map Astral.SSL.show data |> String.concat "\n"
  in
  ignore @@ Format.pp_print_string fmt result_string

let () =
  Server.Kernel_ast.Information.register ~id:"slplugin" ~label:"test"
    ~title:"title string" ~descr:"desc string" format_stmt
