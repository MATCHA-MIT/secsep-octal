(* First, you need to load the Z3 module *)
open Z3

(* Create a Z3 context *)
let ctx = mk_context [];;

(* Create variables x and y *)
let x = BitVector.mk_const_s ctx ("x") 64;;
let z = BitVector.mk_numeral ctx "13" 64;;

(* Create the constraints *)
let xpz = BitVector.mk_add ctx x z;;
let xpz_noo = BitVector.mk_add_no_overflow ctx x z true;;
let constraint1 = BitVector.mk_sge ctx xpz x;;

(* Create a solver *)
let solver = Solver.mk_solver ctx None;;

Solver.push solver;;
Solver.add solver [constraint1; xpz_noo];;
let result = Solver.check solver [];;
Printf.printf "%s\n%s\n" (Solver.string_of_status result) (Solver.to_string solver);;
let model = Solver.get_model solver;;
if Option.is_some model then
  Printf.printf "model:\n%s\n\n" (Model.to_string (Option.get model));;
Solver.pop solver 1;;

let foo (s: Solver.solver) =
  Solver.add s [Boolean.mk_not ctx constraint1; xpz_noo]
;;

foo solver;;
let result = Solver.check solver [];;
Printf.printf "%s\n%s\n" (Solver.string_of_status result) (Solver.to_string solver);;
let model = Solver.get_model solver;;
if Option.is_some model then
  Printf.printf "model:\n%s\n" (Model.to_string (Option.get model));;


(*

x + y + 32
x + y + 34

x + 4
x + 68

*)
