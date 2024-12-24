open Type

let r = Isa_basic.IsaBasic.get_reg_idx
let total_reg_num = Isa_basic.IsaBasic.total_reg_num

let get_reg_taint (reg_taint: (Isa_basic.IsaBasic.register * bool) list) : (bool option) list =
  List.fold_left (
    fun (acc: (bool option) list) (reg, taint) ->
      List.mapi (
        fun i x ->
          if i = r reg then Some taint else x
      ) acc
  ) (List.init Isa_basic.IsaBasic.total_reg_num (fun _ -> None)) reg_taint

let standalone_salsa20_global : External_layouts.GlobalSymbolLayout.t = [
  "key", [ ((SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], (SingleTop, TaintConst true)) ];
  "nonce", [ ((SingleConst 0L, SingleConst 8L), RangeConst [(SingleConst 0L, SingleConst 8L)], (SingleTop, TaintConst true)) ];
  "msg", [ ((SingleConst 0L, SingleConst 128L), RangeConst [(SingleConst 0L, SingleConst 128L)], (SingleTop, TaintConst true)) ];
]

let standalone_salsa20 : Base_func_interface.t = [
  "salsa20_words", [
    r RDI, [ ((SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop) ];
    r RSI, [ ((SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop) ];
  ];
  "salsa20_block", [
    r RDI, [ ((SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop) ];
    r RSI, [ ((SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], SingleTop) ];
  ];
  "salsa20", [
    r RDI, [ ((SingleConst 0L, SingleVar (r RSI)), RangeConst [(SingleConst 0L, SingleVar (r RSI))], SingleTop) ];
    r RDX, [ ((SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], SingleTop) ]
  ];
  "_start", [
  ]
]

let standalone_salsa20_taint_api : Taint_api.TaintApi.t = [
  "_start",
  get_reg_taint [],
  [
    r RSP, [];
  ];
]

let standalone_salsa20_noinline_global : External_layouts.GlobalSymbolLayout.t = [
  "key", [ ((SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], (SingleTop, TaintConst true)) ];
  "nonce", [ ((SingleConst 0L, SingleConst 8L), RangeConst [(SingleConst 0L, SingleConst 8L)], (SingleTop, TaintConst true)) ];
  "msg", [ ((SingleConst 0L, SingleConst 128L), RangeConst [(SingleConst 0L, SingleConst 128L)], (SingleTop, TaintConst true)) ];
]

let standalone_salsa20_noinline : Base_func_interface.t = [
  "salsa20_words", [
    r RDI, [ ((SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop) ];
    r RSI, [ ((SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop) ];
  ];
  "salsa20_block", [
    r RDI, [ ((SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop) ];
    r RSI, [ ((SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], SingleTop) ];
  ];
  "salsa20", [
    r RDI, [ ((SingleConst 0L, SingleVar (r RSI)), RangeConst [(SingleConst 0L, SingleVar (r RSI))], SingleTop) ];
    r RDX, [ ((SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], SingleTop) ]
  ];
  "_start", [
  ]
]

let standalone_salsa20_noinline_taint_api : Taint_api.TaintApi.t = [
  "_start",
  get_reg_taint [],
  [
    r RSP, [];
  ];
]

let bench_sha512_plain_global : External_layouts.GlobalSymbolLayout.t = [
  "K512",  [ (SingleConst 0L, SingleConst 640L), RangeConst [(SingleConst 0L, SingleConst 640L)], (SingleTop, TaintConst true) ];
  "message", [ (SingleConst 0L, SingleConst 256L), RangeConst [(SingleConst 0L, SingleConst 256L)], (SingleTop, TaintConst true) ];
  "out", [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], (SingleTop, TaintConst true) ];
]

let bench_sha512_plain : Base_func_interface.t = [
  "sha512_block_data_order", [
    r RDI, [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop ];
    r RSI, [ (SingleConst 0L, SingleBExp (SingleMul, SingleConst 128L, SingleVar (r RDX))), RangeConst [(SingleConst 0L, SingleBExp (SingleMul, SingleConst 128L, SingleVar (r RDX)))], SingleTop ];
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
  ];
  "SHA512", [
    r RDI, [ (SingleConst 0L, SingleVar (r RSI)), RangeConst [(SingleConst 0L, SingleVar (r RSI))], SingleTop ];
    r RDX, [ (SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop ];
  ];
  "main", [
  ]
]

let bench_sha512_plain_taint_api : Taint_api.TaintApi.t = [
  "main",
  get_reg_taint [],
  [
    r RSP, [];
  ];
]

let bench_ed25519_plain_global : External_layouts.GlobalSymbolLayout.t = [
  "K512", [ (SingleConst 0L, SingleConst 640L), RangeConst [(SingleConst 0L, SingleConst 640L)], (SingleTop, TaintConst true) ];
  "k25519Precomp", [ ((SingleConst 0L, SingleConst 24576L), RangeConst [(SingleConst 0L, SingleConst 24576L)], (SingleTop, TaintConst true)) ];
  "public_key", [ (SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], (SingleTop, TaintConst true) ];
  "private_key", [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], (SingleTop, TaintConst true) ];
  "message", [ (SingleConst 0L, SingleConst 256L), RangeConst [(SingleConst 0L, SingleConst 256L)], (SingleTop, TaintConst true) ];
  "signature", [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], (SingleTop, TaintConst true) ];
  ".LCPI10_0", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst true) ];
  ".LCPI10_1", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst true) ];
  ".LCPI12_0", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst true) ];
  ".LCPI12_1", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst true) ];
  ".LCPI13_0", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst true) ];
  ".LCPI13_1", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst true) ];
  ".LCPI14_0", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst true) ];
  ".LCPI16_0", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst true) ];
]

let bench_ed25519_plain : Base_func_interface.t = [
  "SHA512_Init", [
    r RDI, [ 
      (SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop;
      (SingleConst 64L, SingleConst 72L), RangeConst [], SingleVar (total_reg_num + 1);
      (SingleConst 72L, SingleConst 80L), RangeConst [], SingleVar (total_reg_num + 2);
      (SingleConst 80L, SingleConst 208L), RangeConst [], SingleTop;
      (SingleConst 208L, SingleConst 212L), RangeConst [], SingleVar (total_reg_num + 3);
      (SingleConst 212L, SingleConst 216L), RangeConst [], SingleVar (total_reg_num + 4);
    ];
  ];
  "sha512_block_data_order", [
    r RDI, [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop ];
    r RSI, [ (SingleConst 0L, SingleBExp (SingleMul, SingleConst 128L, SingleVar (r RDX))), RangeConst [(SingleConst 0L, SingleBExp (SingleMul, SingleConst 128L, SingleVar (r RDX)))], SingleTop ];
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
  ];
  "OPENSSL_cleanse", [
    r RDI, [
      (SingleConst 0L, SingleVar (r RSI)), RangeConst [], SingleTop;
    ];
  ];
  "fe_mul_impl", [
    r RDI, [ (SingleConst 0L, SingleConst 40L), RangeConst [], SingleTop ];
    r RSI, [ (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop ];
    r RDX, [ (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop ];
  ];
  "ge_madd", [
    r RDI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [], SingleTop;
      (SingleConst 120L, SingleConst 160L), RangeConst [], SingleTop;
    ];
    r RSI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [(SingleConst 40L, SingleConst 80L)], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [(SingleConst 80L, SingleConst 120L)], SingleTop;
      (SingleConst 120L, SingleConst 160L), RangeConst [(SingleConst 120L, SingleConst 160L)], SingleTop;
    ];
    r RDX, [
      (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [(SingleConst 40L, SingleConst 80L)], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [(SingleConst 80L, SingleConst 120L)], SingleTop;
    ];
  ];
  "ge_p2_dbl", [
    r RDI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [], SingleTop;
      (SingleConst 120L, SingleConst 160L), RangeConst [], SingleTop;
    ];
    r RSI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [(SingleConst 40L, SingleConst 80L)], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [(SingleConst 80L, SingleConst 120L)], SingleTop;
    ];
  ];
  "fe_tobytes", [
    r RDI, [ (SingleConst 0L, SingleConst 32L), RangeConst [], SingleTop ];
    r RSI, [ (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop ];
  ];
  "x25519_ge_p1p1_to_p2", [
    r RDI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [], SingleTop;
    ];
    r RSI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [(SingleConst 40L, SingleConst 80L)], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [(SingleConst 80L, SingleConst 120L)], SingleTop;
      (SingleConst 120L, SingleConst 160L), RangeConst [(SingleConst 120L, SingleConst 160L)], SingleTop;
    ];
  ];
  "x25519_ge_p1p1_to_p3", [
    r RDI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [], SingleTop;
      (SingleConst 120L, SingleConst 160L), RangeConst [], SingleTop;
    ];
    r RSI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [(SingleConst 40L, SingleConst 80L)], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [(SingleConst 80L, SingleConst 120L)], SingleTop;
      (SingleConst 120L, SingleConst 160L), RangeConst [(SingleConst 120L, SingleConst 160L)], SingleTop;
    ];
  ];
  "CRYPTO_memcmp", [
    r RDI, [ (SingleConst 0L, SingleVar (r RDX)), RangeConst [(SingleConst 0L, SingleVar (r RDX))], SingleTop ];
    r RSI, [ (SingleConst 0L, SingleVar (r RDX)), RangeConst [(SingleConst 0L, SingleVar (r RDX))], SingleTop ];
  ];
  "SHA512_Update", [
    r RDI, [
      (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop;
      (SingleConst 64L, SingleConst 72L), RangeConst [(SingleConst 64L, SingleConst 72L)], SingleVar (total_reg_num + 1);
      (SingleConst 72L, SingleConst 80L), RangeConst [(SingleConst 72L, SingleConst 80L)], SingleVar (total_reg_num + 2);
      (SingleConst 80L, SingleConst 208L), RangeConst [(SingleConst 80L, SingleConst 208L)], SingleTop;
      (SingleConst 208L, SingleConst 212L), RangeConst [(SingleConst 208L, SingleConst 212L)], SingleVar (total_reg_num + 3);
      (SingleConst 212L, SingleConst 216L), RangeConst [(SingleConst 212L, SingleConst 216L)], SingleVar (total_reg_num + 4);
    ];
    r RSI, [ (SingleConst 0L, SingleVar (r RDX)), RangeConst [(SingleConst 0L, SingleVar (r RDX))], SingleTop ];
  ];
  "SHA512", [
    r RDI, [ (SingleConst 0L, SingleVar (r RSI)), RangeConst [(SingleConst 0L, SingleVar (r RSI))], SingleTop ];
    r RDX, [ (SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop ];
  ];
  "x25519_sc_reduce", [
    r RDI, [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop ];
  ];
  "table_select", [
    r RDI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [], SingleTop;
    ];
  ];
  "x25519_ge_scalarmult_base", [
    r RDI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [], SingleTop;
      (SingleConst 120L, SingleConst 160L), RangeConst [], SingleTop;
    ];
    r RSI, [ (SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], SingleTop ];
  ];
  "ED25519_sign", [
    r RDI, [ (SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop ];
    r RSI, [ (SingleConst 0L, SingleVar (r RDX)), RangeConst [(SingleConst 0L, SingleVar (r RDX))], SingleTop ];
    r RCX, [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop ];
  ];
  "main", [
  ];
]

let bench_ed25519_plain_noinline_global : External_layouts.GlobalSymbolLayout.t = [
  "K512", [ (SingleConst 0L, SingleConst 640L), RangeConst [(SingleConst 0L, SingleConst 640L)], (SingleTop, TaintConst true) ];
  "k25519Precomp", [ ((SingleConst 0L, SingleConst 24576L), RangeConst [(SingleConst 0L, SingleConst 24576L)], (SingleTop, TaintConst true)) ];
  "public_key", [ (SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], (SingleTop, TaintConst true) ];
  "private_key", [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], (SingleTop, TaintConst true) ];
  "message", [ (SingleConst 0L, SingleConst 256L), RangeConst [(SingleConst 0L, SingleConst 256L)], (SingleTop, TaintConst true) ];
  "signature", [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], (SingleTop, TaintConst true) ];
  ".LCPI10_0", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst false) ];
  ".LCPI10_1", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst false) ];
  ".LCPI12_0", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst false) ];
  ".LCPI12_1", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst false) ];
  ".LCPI13_0", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst false) ];
  ".LCPI13_1", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst false) ];
  ".LCPI15_0", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst false) ];
  ".LCPI18_0", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst false) ];
  ".LCPI20_0", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst false) ];
  ".LCPI21_0", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst false) ];
]

let bench_ed25519_plain_noinline : Base_func_interface.t = [
  "SHA512_Init", [
    r RDI, [ 
      (SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop;
      (SingleConst 64L, SingleConst 72L), RangeConst [], SingleVar (total_reg_num + 1);
      (SingleConst 72L, SingleConst 80L), RangeConst [], SingleVar (total_reg_num + 2);
      (SingleConst 80L, SingleConst 208L), RangeConst [], SingleTop;
      (SingleConst 208L, SingleConst 212L), RangeConst [], SingleVar (total_reg_num + 3);
      (SingleConst 212L, SingleConst 216L), RangeConst [], SingleVar (total_reg_num + 4);
    ];
  ];
  "sha512_block_data_order", [
    r RDI, [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop ];
    r RSI, [ (SingleConst 0L, SingleBExp (SingleMul, SingleConst 128L, SingleVar (r RDX))), RangeConst [(SingleConst 0L, SingleBExp (SingleMul, SingleConst 128L, SingleVar (r RDX)))], SingleTop ];
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
  ];
  "OPENSSL_cleanse", [
    r RDI, [
      (SingleConst 0L, SingleVar (r RSI)), RangeConst [], SingleTop;
    ];
  ];
  "fe_mul_impl", [
    r RDI, [ (SingleConst 0L, SingleConst 40L), RangeConst [], SingleTop ];
    r RSI, [ (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop ];
    r RDX, [ (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop ];
  ];
  "fe_mul_impl_self2", [
    r RDI, [ (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop ];
    r RSI, [ (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop ];
  ];
  "fe_mul_ttt_self1", [
    r RDI, [ (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop ];
    r RSI, [ (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop ];
  ];
  "ge_madd", [
    r RDI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [], SingleTop;
      (SingleConst 120L, SingleConst 160L), RangeConst [], SingleTop;
    ];
    r RSI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [(SingleConst 40L, SingleConst 80L)], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [(SingleConst 80L, SingleConst 120L)], SingleTop;
      (SingleConst 120L, SingleConst 160L), RangeConst [(SingleConst 120L, SingleConst 160L)], SingleTop;
    ];
    r RDX, [
      (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [(SingleConst 40L, SingleConst 80L)], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [(SingleConst 80L, SingleConst 120L)], SingleTop;
    ];
  ];
  "ge_p2_dbl", [
    r RDI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [], SingleTop;
      (SingleConst 120L, SingleConst 160L), RangeConst [], SingleTop;
    ];
    r RSI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [(SingleConst 40L, SingleConst 80L)], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [(SingleConst 80L, SingleConst 120L)], SingleTop;
    ];
  ];
  "fe_tobytes", [
    r RDI, [ (SingleConst 0L, SingleConst 32L), RangeConst [], SingleTop ];
    r RSI, [ (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop ];
  ];
  "x25519_ge_p1p1_to_p2", [
    r RDI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [], SingleTop;
    ];
    r RSI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [(SingleConst 40L, SingleConst 80L)], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [(SingleConst 80L, SingleConst 120L)], SingleTop;
      (SingleConst 120L, SingleConst 160L), RangeConst [(SingleConst 120L, SingleConst 160L)], SingleTop;
    ];
  ];
  "x25519_ge_p1p1_to_p3", [
    r RDI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [], SingleTop;
      (SingleConst 120L, SingleConst 160L), RangeConst [], SingleTop;
    ];
    r RSI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [(SingleConst 40L, SingleConst 80L)], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [(SingleConst 80L, SingleConst 120L)], SingleTop;
      (SingleConst 120L, SingleConst 160L), RangeConst [(SingleConst 120L, SingleConst 160L)], SingleTop;
    ];
  ];
  "CRYPTO_memcmp", [
    r RDI, [ (SingleConst 0L, SingleVar (r RDX)), RangeConst [(SingleConst 0L, SingleVar (r RDX))], SingleTop ];
    r RSI, [ (SingleConst 0L, SingleVar (r RDX)), RangeConst [(SingleConst 0L, SingleVar (r RDX))], SingleTop ];
  ];
  "SHA512_Update", [
    r RDI, [
      (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop;
      (SingleConst 64L, SingleConst 72L), RangeConst [(SingleConst 64L, SingleConst 72L)], SingleVar (total_reg_num + 1);
      (SingleConst 72L, SingleConst 80L), RangeConst [(SingleConst 72L, SingleConst 80L)], SingleVar (total_reg_num + 2);
      (SingleConst 80L, SingleConst 208L), RangeConst [(SingleConst 80L, SingleConst 208L)], SingleTop;
      (SingleConst 208L, SingleConst 212L), RangeConst [(SingleConst 208L, SingleConst 212L)], SingleVar (total_reg_num + 3);
      (SingleConst 212L, SingleConst 216L), RangeConst [(SingleConst 212L, SingleConst 216L)], SingleVar (total_reg_num + 4);
    ];
    r RSI, [ (SingleConst 0L, SingleVar (r RDX)), RangeConst [(SingleConst 0L, SingleVar (r RDX))], SingleTop ];
  ];
  "SHA512", [
    r RDI, [ (SingleConst 0L, SingleVar (r RSI)), RangeConst [(SingleConst 0L, SingleVar (r RSI))], SingleTop ];
    r RDX, [ (SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop ];
  ];
  "x25519_sc_reduce", [
    r RDI, [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop ];
  ];
  "table_select", [
    r RDI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [], SingleTop;
    ];
  ];
  "x25519_ge_scalarmult_base", [
    r RDI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [], SingleTop;
      (SingleConst 120L, SingleConst 160L), RangeConst [], SingleTop;
    ];
    r RSI, [ (SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], SingleTop ];
  ];
  "sc_muladd", [
    r RDI, [ (SingleConst 0L, SingleConst 32L), RangeConst [], SingleTop ];
    r RSI, [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop ];
    r RDX, [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop ];
    r RCX, [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop ];
  ];
  "ge_p3_tobytes", [
    r RDI, [ (SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], SingleTop ];
    r RSI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [], SingleTop;
      (SingleConst 120L, SingleConst 160L), RangeConst [], SingleTop;
    ];
  ];
  "ED25519_sign", [
    r RDI, [ (SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop ];
    r RSI, [ (SingleConst 0L, SingleVar (r RDX)), RangeConst [(SingleConst 0L, SingleVar (r RDX))], SingleTop ];
    r RCX, [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop ];
  ];
  "main", [
  ];
]

let bench_ed25519_plain_noinline_taint_api : Taint_api.TaintApi.t = [
  "main",
  get_reg_taint [],
  [
    r RSP, [];
  ];
]

let update_reg_taint 
    (reg_type: Taint_type_infer.TaintTypeInfer.ArchType.RegType.t) 
    (reg_taint: (bool option) list) :
    Taint_type_infer.TaintTypeInfer.ArchType.RegType.t =
  List.map2 (
    fun (single, taint) (taint_opt: bool option) ->
      match taint_opt with
      | Some b -> single, Taint_exp.TaintExp.TaintConst b
      | None -> single, taint
  ) reg_type reg_taint


let memset_interface: Taint_type_infer.TaintTypeInfer.FuncInterface.t = 
  let start_var: Taint_entry_type.TaintEntryType.t = (SingleVar 0, TaintVar 0) in
  let _, default_reg_type = Taint_type_infer.TaintTypeInfer.ArchType.RegType.init_reg_type start_var in
  let in_reg : Taint_type_infer.TaintTypeInfer.ArchType.RegType.t =
    update_reg_taint default_reg_type
      (get_reg_taint [
        RDI, false;
        RDX, false;
      ])
  in
  let out_reg = List.mapi (
    fun (i: int) entry ->
      if Isa_basic.IsaBasic.is_reg_idx_callee_saved i then entry
      else Single_exp.SingleExp.SingleTop, Taint_exp.TaintExp.TaintConst true 
  ) in_reg
  in
  let in_mem : Taint_type_infer.TaintTypeInfer.ArchType.MemType.t = Taint_type_infer.TaintTypeInfer.ArchType.MemType.add_base_to_offset [
    r RDI, [ 
      (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
      RangeConst [], 
      (Single_exp.SingleExp.SingleTop, Taint_exp.TaintExp.TaintVar (Isa_basic.IsaBasic.total_reg_num)) 
    ];
    r RSP, [ (SingleConst 0L, SingleConst 8L), RangeConst [], (SingleTop, TaintConst true) ];
  ] in
  let mem_context = 
    Taint_type_infer.TaintTypeInfer.ArchType.MemType.get_all_mem_constraint
      (Taint_type_infer.TaintTypeInfer.ArchType.MemType.add_ret_addr_stack_slot in_mem)
  in
  {
    func_name = "memset";
    in_reg = in_reg;
    in_mem = in_mem;
    in_context = mem_context;
    in_taint_context = [];
    out_reg = out_reg;
    out_mem = Taint_type_infer.TaintTypeInfer.ArchType.MemType.add_base_to_offset [
      r RDI, [ 
        (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
        RangeConst [(Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX))], 
        (Single_exp.SingleExp.SingleTop, Taint_exp.TaintExp.TaintVar (Isa_basic.IsaBasic.total_reg_num)) 
      ];
      r RSP, [ (SingleConst 0L, SingleConst 8L), RangeConst [], (SingleTop, TaintConst true) ];
    ];
    out_context = [];
    out_single_subtype_list = [];
    base_info = Taint_type_infer.TaintTypeInfer.ArchType.MemType.add_base_to_offset [
      r RDI, [ 
        (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
        RangeConst [], 
        Call_anno_type.CallAnno.BaseAsReg RDI
      ];
      r RSP, [ (SingleConst 0L, SingleConst 8L), RangeConst [], BaseAsReg RSP ];
    ];
  }

let memcpy_interface: Taint_type_infer.TaintTypeInfer.FuncInterface.t = 
  let start_var: Taint_entry_type.TaintEntryType.t = (SingleVar 0, TaintVar 0) in
  let _, default_reg_type = Taint_type_infer.TaintTypeInfer.ArchType.RegType.init_reg_type start_var in
  let in_reg : Taint_type_infer.TaintTypeInfer.ArchType.RegType.t =
    update_reg_taint default_reg_type
      (get_reg_taint [
        RDI, false; (* dest ptr *)
        RSI, false; (* src ptr *)
        RDX, false;
      ])
  in
  let out_reg = List.mapi (
    fun (i: int) entry ->
      if Isa_basic.IsaBasic.is_reg_idx_callee_saved i then entry
      else Single_exp.SingleExp.SingleTop, Taint_exp.TaintExp.TaintConst true 
  ) in_reg
  in
  let dest_taint = Taint_exp.TaintExp.TaintVar (Isa_basic.IsaBasic.total_reg_num) in
  let src_taint = Taint_exp.TaintExp.TaintVar (Isa_basic.IsaBasic.total_reg_num + 1) in
  let in_mem : Taint_type_infer.TaintTypeInfer.ArchType.MemType.t = Taint_type_infer.TaintTypeInfer.ArchType.MemType.add_base_to_offset [
    r RDI, [ 
      (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
      RangeConst [], 
      (Single_exp.SingleExp.SingleTop, dest_taint) 
    ];
    r RSI, [ 
      (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
      RangeConst [(Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX))], 
      (Single_exp.SingleExp.SingleTop, src_taint) 
    ];
    r RSP, [ (SingleConst 0L, SingleConst 8L), RangeConst [], (SingleTop, TaintConst true) ];
  ] in
  let mem_context = 
    Taint_type_infer.TaintTypeInfer.ArchType.MemType.get_all_mem_constraint
      (Taint_type_infer.TaintTypeInfer.ArchType.MemType.add_ret_addr_stack_slot in_mem)
  in
  {
    func_name = "memcpy";
    in_reg = in_reg;
    in_mem = in_mem;
    in_context = mem_context;
    in_taint_context = [ src_taint, dest_taint ];
    out_reg = out_reg;
    out_mem = Taint_type_infer.TaintTypeInfer.ArchType.MemType.add_base_to_offset [
      r RDI, [ 
        (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
        RangeConst [(Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX))], 
        (Single_exp.SingleExp.SingleTop, dest_taint) 
      ];
      r RSI, [ 
      (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
      RangeConst [(Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX))], 
      (Single_exp.SingleExp.SingleTop, src_taint) 
      ];
      r RSP, [ (SingleConst 0L, SingleConst 8L), RangeConst [], (SingleTop, TaintConst true) ];
    ];
    out_context = [];
    out_single_subtype_list = [];
    base_info = Taint_type_infer.TaintTypeInfer.ArchType.MemType.add_base_to_offset [
      r RDI, [ 
        (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
        RangeConst [], 
        Call_anno_type.CallAnno.BaseAsReg RDI
      ];
      r RSI, [ 
        (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
        RangeConst [(Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX))], 
        Call_anno_type.CallAnno.BaseAsReg RSI 
      ];
      r RSP, [ (SingleConst 0L, SingleConst 8L), RangeConst [], BaseAsReg RSP ];
    ];
  }


let add_general_func_suffix
    (func_interface: Taint_type_infer.TaintTypeInfer.FuncInterface.t) :
    Taint_type_infer.TaintTypeInfer.FuncInterface.t list =
  [func_interface;
  {func_interface with func_name = func_interface.func_name ^ "@PLT"}]

let () = 
  (* Stack_layout.StackLayout.test(); *)
  let open Sexplib in

  let channel = open_out "./interface/general_func_interface.func_interface" in
  Sexp.output_hum channel (Sexplib.Std.sexp_of_list Taint_type_infer.TaintTypeInfer.FuncInterface.sexp_of_t 
    (List.concat_map add_general_func_suffix [memset_interface; memcpy_interface]));

  let channel = open_out "./interface/standalone_salsa20.symbol_layout" in
  Sexp.output_hum channel (External_layouts.GlobalSymbolLayout.sexp_of_t standalone_salsa20_global);

  let channel = open_out "./interface/standalone_salsa20.mem_interface" in
  let stack = External_layouts.StackLayout.from_file "./out/standalone_salsa20.stack_layout" in
  Sexp.output_hum channel (Base_func_interface.sexp_of_t (Base_func_interface.add_stack_layout standalone_salsa20 stack));

  let channel = open_out "./interface/standalone_salsa20.taint_api" in
  Sexp.output_hum channel (Taint_api.TaintApi.sexp_of_t standalone_salsa20_taint_api);

  let channel = open_out "./interface/standalone_salsa20_noinline.symbol_layout" in
  Sexp.output_hum channel (External_layouts.GlobalSymbolLayout.sexp_of_t standalone_salsa20_noinline_global);

  let channel = open_out "./interface/standalone_salsa20_noinline.mem_interface" in
  let stack = External_layouts.StackLayout.from_file "./out/standalone_salsa20_noinline.stack_layout" in
  Sexp.output_hum channel (Base_func_interface.sexp_of_t (Base_func_interface.add_stack_layout standalone_salsa20_noinline stack));

  let channel = open_out "./interface/standalone_salsa20_noinline.taint_api" in
  Sexp.output_hum channel (Taint_api.TaintApi.sexp_of_t standalone_salsa20_noinline_taint_api);

  let channel = open_out "./interface/bench_sha512_plain.symbol_layout" in
  Sexp.output_hum channel (External_layouts.GlobalSymbolLayout.sexp_of_t bench_sha512_plain_global);

  let channel = open_out "./interface/bench_sha512_plain.mem_interface" in
  let stack = External_layouts.StackLayout.from_file "./out/bench_sha512_plain.stack_layout" in
  Sexp.output_hum channel (Base_func_interface.sexp_of_t (Base_func_interface.add_stack_layout bench_sha512_plain stack));

  let channel = open_out "./interface/bench_sha512_plain.taint_api" in
  Sexp.output_hum channel (Taint_api.TaintApi.sexp_of_t bench_sha512_plain_taint_api);

  let channel = open_out "./interface/bench_ed25519_plain.symbol_layout" in
  Sexp.output_hum channel (External_layouts.GlobalSymbolLayout.sexp_of_t bench_ed25519_plain_global);

  let channel = open_out "./interface/bench_ed25519_plain.mem_interface" in
  let stack = External_layouts.StackLayout.from_file "./out/bench_ed25519_plain.stack_layout" in
  Sexp.output_hum channel (Base_func_interface.sexp_of_t (Base_func_interface.add_stack_layout bench_ed25519_plain stack));

  let channel = open_out "./interface/bench_ed25519_plain_noinline.symbol_layout" in
  Sexp.output_hum channel (External_layouts.GlobalSymbolLayout.sexp_of_t bench_ed25519_plain_noinline_global);

  let channel = open_out "./interface/bench_ed25519_plain_noinline.mem_interface" in
  let stack = External_layouts.StackLayout.from_file "./out/bench_ed25519_plain_noinline.stack_layout" in
  Sexp.output_hum channel (Base_func_interface.sexp_of_t (Base_func_interface.add_stack_layout bench_ed25519_plain_noinline stack));

  let channel = open_out "./interface/bench_ed25519_plain_noinline.taint_api" in
  Sexp.output_hum channel (Taint_api.TaintApi.sexp_of_t bench_ed25519_plain_noinline_taint_api);
