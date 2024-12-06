(* categories of debug output, enable with [-sl-msg-key <category1,category2,...>] *)
let do_instr = Config.Self.register_category "do_instr"
let combine_predecessors = Config.Self.register_category "combine_predecessors"
let do_guard = Config.Self.register_category "do_guard"
let do_edge = Config.Self.register_category "do_edge"
let func_call = Config.Self.register_category "func_call"
