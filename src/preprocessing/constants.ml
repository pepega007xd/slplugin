let null_var_name = "_nil"
let const_var_name = "_const"

let nullptr_var =
  Cil.makeVarinfo false false null_var_name Cil_const.voidPtrType

let const_var = Cil.makeVarinfo false false const_var_name Cil_const.voidPtrType
