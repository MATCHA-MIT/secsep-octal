open Z3

let bit_width = 64
let ctx = mk_context []
(* let solver = Solver.mk_solver ctx *)

let x = BitVector.mk_const_s ctx "x" bit_width
let y = BitVector.mk_const_s ctx "y" bit_width
let z = BitVector.mk_add_no_overflow ctx x y true
let w = BitVector.mk_add_no_underflow ctx x y

let _ = Printf.printf "x = %s\n" (Expr.to_string x)
let _ = Printf.printf "y = %s\n" (Expr.to_string y)
let _ = Printf.printf "z = %s\n" (Expr.to_string z)
let _ = Printf.printf "w = %s\n" (Expr.to_string w)