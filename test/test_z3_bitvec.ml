(* First, you need to load the Z3 module *)
open Z3

(* Create a Z3 context *)
let ctx = mk_context [];;

(* Create variables x and y *)
let x = BitVector.mk_const_s ctx ("x") 64;;
let z = BitVector.mk_numeral ctx "13" 64;;
let zero = BitVector.mk_numeral ctx "0" 64;;

(* Create the constraints *)
let xpy = BitVector.mk_add ctx x z;;
let constraint1 = BitVector.mk_uge ctx xpy z;;
let constraint2 = BitVector.mk_ule ctx x zero;;

(* Create a solver *)
let solver = Solver.mk_solver ctx None;;

Solver.push solver;;
Solver.add solver [constraint1; constraint2];;
let result1 = Solver.check solver [];;
Printf.printf "%s\n%s\n" (Solver.string_of_status result1) (Solver.to_string solver);;
let model1 = Solver.get_model solver;;
if Option.is_some model1 then
  Printf.printf "%s\n" (Model.to_string (Option.get model1));;
Solver.pop solver 1;;

Solver.push solver;;
Solver.add solver [Boolean.mk_not ctx constraint1; constraint2];;
let result2 = Solver.check solver [];;
Printf.printf "%s\n%s\n" (Solver.string_of_status result2) (Solver.to_string solver);;
let model2 = Solver.get_model solver;;
if Option.is_some model2 then
  Printf.printf "%s\n" (Model.to_string (Option.get model2));;

let foo (s: Solver.solver) =
  Solver.add s [Boolean.mk_xor ctx constraint1 constraint2]
;;

foo solver;;
Printf.printf "%s\n" (Solver.to_string solver);;


(*

x + y + 32
x + y + 34

x + 4
x + 68

*)
