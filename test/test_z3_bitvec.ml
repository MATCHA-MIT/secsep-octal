(* First, you need to load the Z3 module *)
open Z3

(* Create a Z3 context *)
let ctx = mk_context [];;

(* Create variables x and y *)
let x = BitVector.mk_const_s ctx "s6" 64;;
let s = BitVector.mk_const_s ctx "s-5" 64;;

(*
(assert (bvsle (bvadd #x0000000000000300 (bvmul #x0000000000000300 x) s-5)
       (bvadd #x0000000000006000 s-5)))
*)
let assert1 = BitVector.mk_sle ctx
  (BitVector.mk_add ctx
    (BitVector.mk_add ctx (BitVector.mk_numeral ctx "768" 64) (BitVector.mk_mul ctx (BitVector.mk_numeral ctx "768" 64) x))
    s
  )
  (BitVector.mk_add ctx (BitVector.mk_numeral ctx "24576" 64) s);;
;;
(*
(assert (not 
  (bvsle
    (bvadd #x0000000000000300 (bvmul #x0000000000000300 x) s-5)
    (bvadd #x00000000000002f0 (bvmul #x0000000000000300 x) s-5)
  )
))
*)
let assert2 = Boolean.mk_not ctx (
    BitVector.mk_sle ctx
    (BitVector.mk_add ctx
      (BitVector.mk_add ctx (BitVector.mk_numeral ctx "768" 64) (BitVector.mk_mul ctx (BitVector.mk_numeral ctx "768" 64) x))
      s
    )
    (BitVector.mk_add ctx
      (BitVector.mk_add ctx (BitVector.mk_numeral ctx "752" 64) (BitVector.mk_mul ctx (BitVector.mk_numeral ctx "768" 64) x))
      s
    )
);;
(*
(assert (bvsle s-5 (bvadd #x00000000000002f0 (bvmul #x0000000000000300 x) s-5)))
(assert (bvsle s-5 (bvadd #x00000000000001f0 (bvmul #x0000000000000300 x) s-5)))
(assert (bvsle s-5 (bvadd #x00000000000000f0 (bvmul #x0000000000000300 x) s-5)))
*)
let assert3 = BitVector.mk_sle ctx
  s
  (BitVector.mk_add ctx
    (BitVector.mk_add ctx (BitVector.mk_numeral ctx "752" 64) (BitVector.mk_mul ctx (BitVector.mk_numeral ctx "768" 64) x))
    s
  )
;;
let assert4 = BitVector.mk_sle ctx
  s
  (BitVector.mk_add ctx
    (BitVector.mk_add ctx (BitVector.mk_numeral ctx "496" 64) (BitVector.mk_mul ctx (BitVector.mk_numeral ctx "768" 64) x))
    s
  )
;;
let assert5 = BitVector.mk_sle ctx
  s
  (BitVector.mk_add ctx
    (BitVector.mk_add ctx (BitVector.mk_numeral ctx "240" 64) (BitVector.mk_mul ctx (BitVector.mk_numeral ctx "768" 64) x))
    s
  )
;;

(*
(let
   ((a!1
     (bvadd s6
      (bvmul s6 #x0000000000000002))))
   (bvadd
    (bvshl a!1 #x0000000000000008)
    s-5
   ))
*)
let bound_l = BitVector.mk_add ctx
  (BitVector.mk_mul ctx (BitVector.mk_numeral ctx "3" 64) x)
  s;;
let bound_r = BitVector.mk_add ctx (BitVector.mk_numeral ctx "16" 64) bound_l;;

(* Create a solver *)
let solver = Solver.mk_solver ctx None;;
Solver.add solver [assert1; assert2; assert3; assert4; assert5];;

(* Add the new assertions *)

(* Check satisfiability *)
let result = Solver.check solver [];;
Printf.printf "original solver's result:\n%s\n%s\n" (Solver.string_of_status result) (Solver.to_string solver);;

let result = Solver.check solver [
  BitVector.mk_sle ctx s bound_l;
  BitVector.mk_sle ctx bound_r (BitVector.mk_add ctx s (BitVector.mk_numeral ctx "24576" 64))
];;
Printf.printf "new solver's result:\n%s\n%s\n" (Solver.string_of_status result) (Solver.to_string solver);;
