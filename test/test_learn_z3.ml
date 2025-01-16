open Z3

let bit_width = 64
let ctx = mk_context []
let solver = Solver.mk_solver ctx None

let x = BitVector.mk_const_s ctx "x" bit_width
let y = BitVector.mk_const_s ctx "y" bit_width
(* let z = BitVector.mk_add_no_overflow ctx x y true
let w = BitVector.mk_add_no_underflow ctx x y *)

let _ = Printf.printf "x = %s\n" (Expr.to_string x)
let _ = Printf.printf "y = %s\n" (Expr.to_string y)
(* let _ = Printf.printf "z = %s\n" (Expr.to_string z)
let _ = Printf.printf "w = %s\n" (Expr.to_string w) *)

(* let a = BitVector.mk_numeral ctx "111" bit_width
let b = BitVector.mk_numeral ctx "111" bit_width
let c = BitVector.mk_numeral ctx "222" bit_width *)

let result_to_string (result: Solver.status) =
  match result with
  | Solver.UNKNOWN -> "UNKNOWN"
  | Solver.SATISFIABLE -> "SATISFIABLE"
  | Solver.UNSATISFIABLE -> "UNSATISFIABLE"

(* let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ Boolean.mk_eq ctx a b ]))
let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ Boolean.mk_eq ctx a c ]))

let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ Boolean.mk_eq ctx x y ]))
let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ Boolean.mk_not ctx (Boolean.mk_eq ctx x y) ]))
let _ = Solver.add solver [ Boolean.mk_eq ctx x y ]
let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ Boolean.mk_eq ctx x y ]))
let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ Boolean.mk_not ctx (Boolean.mk_eq ctx x y) ])) *)

(* let _ = Solver.add solver [ BitVector.mk_slt ctx x y ]
let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ Boolean.mk_not ctx (Boolean.mk_eq ctx a b) ]))
let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ Boolean.mk_not ctx (Boolean.mk_eq ctx a c) ]))
let _ = Solver.add solver [ BitVector.mk_sgt ctx x y ]
let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ Boolean.mk_not ctx (Boolean.mk_eq ctx a b) ]))
let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ Boolean.mk_eq ctx a b ])) *)

let s7 = BitVector.mk_const_s ctx "s7" bit_width
let s27 = BitVector.mk_const_s ctx "s27" bit_width
let s115 = BitVector.mk_const_s ctx "s115" bit_width
let eight = BitVector.mk_numeral ctx "8" bit_width
let neg8 = BitVector.mk_numeral ctx "-8" bit_width
let seven = BitVector.mk_numeral ctx "7" bit_width
let zero = BitVector.mk_numeral ctx "0" bit_width

(* let e1 =
  BitVector.mk_sub ctx
    (BitVector.mk_add ctx s7 s27)
    (BitVector.mk_and ctx (BitVector.mk_add ctx s7 s27) neg8)

let e2 =
  BitVector.mk_sub ctx
    (BitVector.mk_add ctx (BitVector.mk_and ctx s7 neg8) s27)
    (BitVector.mk_and ctx (BitVector.mk_add ctx (BitVector.mk_and ctx s7 neg8) s27) neg8)

let _ = Printf.printf "%s\n%s\n" (Expr.to_string (Expr.simplify e1 None)) (Expr.to_string (Expr.simplify e2 None)) *)

let e1 = BitVector.mk_and ctx (BitVector.mk_add ctx eight s27) neg8
let e2 = BitVector.mk_add ctx eight (BitVector.mk_and ctx s27 neg8)

let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ 
  Boolean.mk_eq ctx e1 e2 
]))

let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ 
  Boolean.mk_not ctx (Boolean.mk_eq ctx e1 e2) 
]))

let e1 = BitVector.mk_and ctx (BitVector.mk_add ctx s7 s27) neg8
let e2 = BitVector.mk_add ctx s27 (BitVector.mk_and ctx s7 s115)

let _ = Solver.add solver [ Boolean.mk_eq ctx (BitVector.mk_and ctx s7 seven) zero ]

let _ = Printf.printf "HH %s\n" (Solver.to_string solver)

let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ 
  Boolean.mk_eq ctx e1 e2 
]))

let _ = Printf.printf "%s\n" (result_to_string (Z3.Solver.check solver [ 
  Boolean.mk_not ctx (Boolean.mk_eq ctx e1 e2) 
]))

let _ =
  Printf.printf "==================\n";
  let num = BitVector.mk_numeral ctx "1234567890" bit_width in
  let num2 = BitVector.mk_numeral ctx "0987654321" bit_width in
  Printf.printf "num = %s\n" (Expr.to_string num);
  let get_extract (high_bit: int) (low_bit: int) : Expr.expr =
    Expr.simplify (BitVector.mk_extract ctx high_bit low_bit num) None
  in
  Printf.printf "extract 4 0 num = %s\n" (get_extract 4 0 |> Expr.to_string);
  Printf.printf "extract 3 0 num = %s\n" (get_extract 3 0 |> Expr.to_string);
  Printf.printf "extract 4 1 num = %s\n" (get_extract 4 1 |> Expr.to_string);
  Printf.printf "extract 8 4 num = %s\n" (get_extract 8 4 |> Expr.to_string);
  Printf.printf "extract 7 4 num = %s\n" (get_extract 7 4 |> Expr.to_string);
  Printf.printf "extract 5 2 num = %s\n" (get_extract 5 2 |> Expr.to_string);
  let x = get_extract 8 4 in
  let y = get_extract 7 3 in
  let z = get_extract 9 5 in
  let xy = Expr.simplify (BitVector.mk_add ctx x y) None in
  let yz = Expr.simplify (BitVector.mk_concat ctx y z) None in
  Printf.printf "x=%s\ny=%s\nz=%s\nx+y=%s\ny+z=%s\n" 
    (Expr.to_string x) (Expr.to_string y) (Expr.to_string z) (Expr.to_string xy) (Expr.to_string yz);
  Printf.printf "x+y no overflow =\n%s\n" (Expr.to_string (BitVector.mk_add_no_overflow ctx num num2 true));
  Printf.printf "x+y no overflow =\n%s\n" (Expr.to_string (BitVector.mk_add_no_overflow ctx num num2 false));
  Printf.printf "x+y no underflow =\n%s\n" (Expr.to_string (BitVector.mk_add_no_underflow ctx num num2));
  Printf.printf "neg num mk_neg_no_overflow %s\n" (Expr.to_string (BitVector.mk_neg_no_overflow ctx num));
  Printf.printf "is bool %b\n" (Boolean.is_bool (BitVector.mk_add_no_overflow ctx num num2 true));
  Printf.printf "x size %d\n" (BitVector.get_size (Expr.get_sort x));
  Printf.printf "e2 %s size %d\n" (Expr.to_string e2) (BitVector.get_size (Expr.get_sort e2));
  Printf.printf "e1 %s num args %d\n%s\n" 
    (Expr.to_string e1) (Expr.get_num_args e1)
    (String.concat " " (List.map Expr.to_string (Expr.get_args e1)));
  Printf.printf "e2 %s num args %d\n%s\n" 
    (Expr.to_string e2) (Expr.get_num_args e2)
    (String.concat " " (List.map Expr.to_string (Expr.get_args e2)));
  Printf.printf "e2 repl %s\n" (Expr.to_string (Expr.substitute e2 [ s27 ] [ neg8 ]));
