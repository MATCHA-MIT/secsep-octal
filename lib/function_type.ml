open Code_type

module FunctionType = struct
  exception FunctionTypeError of string

  let function_type_error msg = raise (FunctionTypeError ("[Function Type Error] " ^ msg))

  type taint_var_id = int

  type taint_exp =
    | TaintConst of bool (* true for tainted and false for untainted*)
    | TaintVar of taint_var_id

  type taint_type_exp = {
    type_exp: CodeType.type_full_exp;
    taint_exp: taint_exp;
  }




end