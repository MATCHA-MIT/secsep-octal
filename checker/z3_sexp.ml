open Z3
open Sexplib


module Z3Expr = struct
include Expr

  exception Z3ExprError of string

  let z3_expr_error msg = raise (Z3ExprError ("[Z3 Expr Error] " ^ msg))

  let sexp_of_expr (exp: expr) : Sexp.t =
    exp |> Expr.ast_of_expr |> AST.to_sexpr |> Sexp.of_string

  let expr_of_sexp (_: Sexp.t) : expr =
    z3_expr_error "expr_of_sexp not implemented yet"

end
