open Type

let r = Isa_basic.IsaBasic.get_reg_idx
let total_reg_num = Isa_basic.IsaBasic.total_reg_num

let bench_sha512_plain : Base_func_interface.t = 
  let k512 = -2 in
  let message = -3 in
  let key = -4 in
  [
    "sha512_block_data_order", [
      r RDI, [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop ];
      r RSI, [ (SingleConst 0L, SingleBExp (SingleMul, SingleConst 128L, SingleVar (r RDX))), RangeConst [(SingleConst 0L, SingleBExp (SingleMul, SingleConst 128L, SingleVar (r RDX)))], SingleTop ];
      k512,  [ (SingleConst 0L, SingleConst 640L), RangeConst [(SingleConst 0L, SingleConst 640L)], SingleTop ];
    ];
    "SHA512_Final", [
      r RDI, [
        (SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop;
      ];
      r RSI, [
        (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop;
        (SingleConst 64L, SingleConst 72L), RangeConst [(SingleConst 64L, SingleConst 72L)], SingleVar (total_reg_num + 1);
        (SingleConst 72L, SingleConst 80L), RangeConst [(SingleConst 72L, SingleConst 80L)], SingleVar (total_reg_num + 2);
        (SingleConst 80L, SingleConst 208L), RangeConst [(SingleConst 80L, SingleConst 208L)], SingleTop;
        (SingleConst 208L, SingleConst 212L), RangeConst [(SingleConst 208L, SingleConst 212L)], SingleVar (total_reg_num + 3);
        (SingleConst 212L, SingleConst 216L), RangeConst [(SingleConst 212L, SingleConst 216L)], SingleVar (total_reg_num + 4);
      ];
      k512,  [ (SingleConst 0L, SingleConst 640L), RangeConst [(SingleConst 0L, SingleConst 640L)], SingleTop ];
    ];
    "SHA512", [
      r RDI, [ (SingleConst 0L, SingleVar (r RSI)), RangeConst [(SingleConst 0L, SingleVar (r RSI))], SingleTop ];
      r RDX, [ (SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop ];
    ];
    "main", [
      message, [ (SingleConst 0L, SingleConst 256L), RangeConst [(SingleConst 0L, SingleConst 256L)], SingleTop ];
      key, [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop ];
    ]
  ]

let () = 
  (* Stack_layout.StackLayout.test(); *)
  let open Sexplib in
  let channel = open_out "./interface/bench_sha512_plain.mem_interface" in
  let bench_sha512_stack = Stack_layout.StackLayout.from_file "./out/bench_sha512_plain.stack_layout" in
  Sexp.output_hum channel (Base_func_interface.sexp_of_t (Base_func_interface.add_stack_layout bench_sha512_plain bench_sha512_stack))
