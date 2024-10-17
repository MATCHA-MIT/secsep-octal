open Type

let salsa20_global : Single_type_infer.SingleTypeInfer.ArchType.MemType.t = [
  -2, [ ((SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], SingleTop) ];
  -3, [ ((SingleConst 0L, SingleConst 8L), RangeConst [(SingleConst 0L, SingleConst 8L)], SingleTop) ];
  -4, [ ((SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop) ];
]

let r = Isa_basic.IsaBasic.get_reg_idx

(* Note that salsa20_global is not real global var, so I only put it in _start's mem interface *)
let standalone_salsa20 : Base_func_interface.t = [
  "salsa20_words", [
    r RSP, [ ((SingleConst 0L, SingleConst 0L), RangeConst [], SingleTop) ];
    r RDI, [ ((SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop) ];
    r RSI, [ ((SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop) ];
  ];
  "salsa20_block", [
    r RSP, [ ((SingleConst 0L, SingleConst 0L), RangeConst [], SingleTop) ];
    r RDI, [ ((SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop) ];
    r RSI, [ ((SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], SingleTop) ];
  ];
  "salsa20", [
    r RSP, [ ((SingleConst 0L, SingleConst 0L), RangeConst [], SingleTop) ];
    r RDI, [ ((SingleConst 0L, SingleVar (r RSI)), RangeConst [(SingleConst 0L, SingleVar (r RSI))], SingleTop) ];
    r RDX, [ ((SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], SingleTop) ]
  ];
  "_start", salsa20_global @ [
    r RSP, [ ((SingleConst 0L, SingleConst 0L), RangeConst [], SingleTop) ]
  ];
]


let () = 
  let open Sexplib in
  let channel = open_out "./interface/standalone_salsa20" in
  Sexp.output_hum channel (Base_func_interface.sexp_of_t standalone_salsa20)



