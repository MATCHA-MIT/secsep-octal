open Z3

let bit_width = 64
let ctx = mk_context []
let solver = Solver.mk_solver ctx None

let x = BitVector.mk_const_s ctx "x" bit_width
let y = BitVector.mk_const_s ctx "y" bit_width
let z = BitVector.mk_add_no_overflow ctx x y true
let w = BitVector.mk_add_no_underflow ctx x y

let _ = Printf.printf "x = %s\n" (Expr.to_string x)
let _ = Printf.printf "y = %s\n" (Expr.to_string y)
let _ = Printf.printf "z = %s\n" (Expr.to_string z)
let _ = Printf.printf "w = %s\n" (Expr.to_string w)

let a = BitVector.mk_numeral ctx "111" bit_width
let b = BitVector.mk_numeral ctx "111" bit_width
let c = BitVector.mk_numeral ctx "222" bit_width

let result_to_string (result: Solver.status) =
  match result with
  | Solver.UNKNOWN -> "UNKNOWN"
  | Solver.SATISFIABLE -> "SATISFIABLE"
  | Solver.UNSATISFIABLE -> "UNSATISFIABLE"

let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ Boolean.mk_eq ctx a b ]))
let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ Boolean.mk_eq ctx a c ]))

let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ Boolean.mk_eq ctx x y ]))
let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ Boolean.mk_not ctx (Boolean.mk_eq ctx x y) ]))
let _ = Solver.add solver [ Boolean.mk_eq ctx x y ]
let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ Boolean.mk_eq ctx x y ]))
let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ Boolean.mk_not ctx (Boolean.mk_eq ctx x y) ]))
