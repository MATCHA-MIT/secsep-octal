open Type_full_exp

module FunctionType = struct
  exception FunctionTypeError of string

  let function_type_error msg = raise (FunctionTypeError ("[Function Type Error] " ^ msg))

  type taint_var_id = int

  type taint_exp =
    | TaintConst of bool (* true for tainted and false for untainted*)
    | TaintVar of taint_var_id

  type taint_type_exp = {
    type_exp: TypeFullExp.t;
    taint_exp: taint_exp;
  }




end