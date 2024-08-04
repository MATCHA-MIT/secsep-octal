(* First, you need to load the Z3 module *)
open Z3

(* Create a Z3 context *)
let ctx = mk_context [];;

(* Create variables x and y *)
let x = BitVector.mk_const_s ctx "x" 64;;
let y = BitVector.mk_numeral ctx "10" 64;;
let z = BitVector.mk_numeral ctx "13" 64;;

(* Create the constraints *)
let xpy = BitVector.mk_add ctx y x;;
let xpz = BitVector.mk_add ctx z x;;
let xpy_noo = BitVector.mk_add_no_overflow ctx x y true;;
let xpz_noo = BitVector.mk_add_no_overflow ctx x z true;;
let constr1 = BitVector.mk_sle ctx xpy xpz;;
let simp_constr1 = Expr.simplify constr1 None;;
let constr2 = BitVector.mk_sle ctx (BitVector.mk_numeral ctx "0" 64) (BitVector.mk_sub ctx xpz xpy);;
let simp_constr2 = Expr.simplify constr2 None;;

(* Print the constraints *)
Printf.printf "Constr1           : %s\n" (Expr.to_string constr1);;
Printf.printf "Simplified constr1: %s\n" (Expr.to_string simp_constr1);;
Printf.printf "Constr2           : %s\n" (Expr.to_string constr2);;
Printf.printf "Simplified constr2: %s\n" (Expr.to_string simp_constr2);;

(* Create a solver *)
let solver = Solver.mk_solver ctx None;;

Solver.add solver [constr1; xpy_noo; xpz_noo];;
let result = Solver.check solver [];;
Printf.printf "%s\n%s\n" (Solver.string_of_status result) (Solver.to_string solver);;
let model = Solver.get_model solver;;
if Option.is_some model then
  Printf.printf "model:\n%s\n\n" (Model.to_string (Option.get model));;
