(* First, you need to load the Z3 module *)
open Z3

(* Create a Z3 context *)
let ctx = mk_context [];;

(* Create variables x and y *)
let x = Arithmetic.Integer.mk_const_s ctx "x";;
let y = Arithmetic.Integer.mk_const_s ctx "x";;

(* Create the constraints *)
let constraint1 = Arithmetic.mk_le ctx (Arithmetic.mk_add ctx [x; Arithmetic.Integer.mk_numeral_i ctx 4]) (Arithmetic.mk_add ctx [x; y; Arithmetic.Integer.mk_numeral_i ctx 32]);;
let constraint2 = Arithmetic.mk_ge ctx (Arithmetic.mk_add ctx [x; Arithmetic.Integer.mk_numeral_i ctx 68]) (Arithmetic.mk_add ctx [x; y; Arithmetic.Integer.mk_numeral_i ctx 34]);;
let constraint3 = Arithmetic.mk_ge ctx y (Arithmetic.Integer.mk_numeral_i ctx 0);;
let constraint4 = Arithmetic.mk_ge ctx y (Arithmetic.Integer.mk_numeral_i ctx 34);;

(* Create a solver *)
let solver = Solver.mk_solver ctx None;;

(* Add the constraints to the solver *)
Solver.add solver [constraint1; constraint2; constraint3; constraint4];;

(* Check for satisfiability *)
let result = Solver.check solver [];;

(* Function to print the result *)
let print_result = function
  | Solver.SATISFIABLE ->
      let model = Solver.get_model solver in
      (match model with
      | None -> print_endline "No model found."
      | Some m ->
          let x_val = Model.eval m x true in
          let y_val = Model.eval m y true in
          (match x_val, y_val with
          | Some xv, Some yv ->
              Printf.printf "Solution found:\n";
              Printf.printf "x = %s\n" (Expr.to_string xv);
              Printf.printf "y = %s\n" (Expr.to_string yv)
          | _ -> print_endline "Could not evaluate variables."))
  | Solver.UNSATISFIABLE -> print_endline "Constraints are unsatisfiable."
  | Solver.UNKNOWN -> print_endline "Solver returned unknown."

(* Print the result *)
let () = print_result result


(*

x + y + 32
x + y + 34

x + 4
x + 68

*)