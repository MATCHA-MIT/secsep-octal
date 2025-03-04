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

(* let _ =
  let ctx = Z3.mk_context [] in
  let bv1 = Z3.Expr.mk_const_s ctx "bv1" (Z3.BitVector.mk_sort ctx 8) in
  let bv2 = Z3.Expr.mk_const_s ctx "bv2" (Z3.BitVector.mk_sort ctx 16) in
  let c1 = Z3.Expr.mk_numeral_int ctx 234 (Z3.BitVector.mk_sort ctx 8) in
  let c2 = Z3.Expr.mk_numeral_int ctx 123 (Z3.BitVector.mk_sort ctx 16) in

  let constr1 = Z3.Boolean.mk_eq ctx bv1 c1 in
  let constr2 = Z3.Boolean.mk_eq ctx bv2 c2 in

  (* let assertion = Z3.BitVector.mk_sge ctx bv1 bv2 in *)
  let ext_bits = Z3.BitVector.get_size (Z3.Expr.get_sort bv2) - Z3.BitVector.get_size (Z3.Expr.get_sort bv1) in
  let assertion = Z3.BitVector.mk_sge ctx (Z3.BitVector.mk_zero_ext ctx ext_bits bv1) bv2 in

  (* test whether target holds when constr 1 & 2 *)
  let solver = Z3.Solver.mk_solver ctx None in
  Z3.Solver.add solver [constr1; constr2];
  let res = Z3.Solver.check solver [assertion] in

  let _ = match res with
  | Z3.Solver.UNSATISFIABLE -> print_endline "UNSAT"
  | Z3.Solver.SATISFIABLE -> print_endline "SAT"
  | Z3.Solver.UNKNOWN -> print_endline "UNKNOWN"
  in
  ()
 *)

(* substitution size mismatch test *)
let _ =
  let ctx = Z3.mk_context [] in
  let bv1 = Z3.Expr.mk_const_s ctx "bv1" (Z3.BitVector.mk_sort ctx 16) in
  let bv1_alias = Z3.Expr.mk_const_s ctx "bv1" (Z3.BitVector.mk_sort ctx 16) in
  let c = Z3.Expr.mk_numeral_int ctx 123 (Z3.BitVector.mk_sort ctx 16) in

  let subed = Z3.Expr.substitute bv1 [bv1_alias] [c] in
  Printf.printf "original: %s\n" (Z3.Expr.to_string bv1);
  Printf.printf "subed   : %s\n" (Z3.Expr.to_string subed);

  ()
