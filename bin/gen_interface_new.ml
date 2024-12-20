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
  "salsa20_words",
  get_reg_taint [
    RDI, false;
    RSI, false;
  ],
  [
    r RSP, [];
    r RDI, [ ((SingleConst 0L, SingleConst 64L), RangeConst [], Some true) ];
    r RSI, [ ((SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], Some true) ];
  ];

  "salsa20_block",
  get_reg_taint [
    RDI, false;
    RSI, false;
    RDX, true;
    RCX, true;
  ],
  [
    r RSP, [];
    r RDI, [ ((SingleConst 0L, SingleConst 64L), RangeConst [], Some true) ];
    r RSI, [ ((SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], Some true) ];
  ];

  "salsa20",
  get_reg_taint [
    RDI, false;
    RSI, false;
    RDX, false;
    RCX, true;
  ],
  [
    r RSP, [];
    r RDI, [ ((SingleConst 0L, SingleVar (r RSI)), RangeConst [(SingleConst 0L, SingleVar (r RSI))], Some true) ];
    r RDX, [ ((SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], Some true) ]
  ];

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
  ".LCPI10_0", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst true) ];
  ".LCPI10_1", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst true) ];
  ".LCPI12_0", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst true) ];
  ".LCPI12_1", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst true) ];
  ".LCPI13_0", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst true) ];
  ".LCPI13_1", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst true) ];
  ".LCPI14_0", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst true) ];
  ".LCPI17_0", [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], (SingleTop, TaintConst true) ];
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
  "ED25519_sign", [
    r RDI, [ (SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop ];
    r RSI, [ (SingleConst 0L, SingleVar (r RDX)), RangeConst [(SingleConst 0L, SingleVar (r RDX))], SingleTop ];
    r RCX, [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop ];
  ];
  "main", [
  ];
]

let () = 
  (* Stack_layout.StackLayout.test(); *)
  let open Sexplib in

  let channel = open_out "./interface/standalone_salsa20.symbol_layout" in
  Sexp.output_hum channel (External_layouts.GlobalSymbolLayout.sexp_of_t standalone_salsa20_global);

  let channel = open_out "./interface/standalone_salsa20.mem_interface" in
  let stack = External_layouts.StackLayout.from_file "./out/standalone_salsa20.stack_layout" in
  Sexp.output_hum channel (Base_func_interface.sexp_of_t (Base_func_interface.add_stack_layout standalone_salsa20 stack));

  let channel = open_out "./interface/standalone_salsa20.taint_api" in
  Sexp.output_hum channel (Taint_api.TaintApi.sexp_of_t standalone_salsa20_taint_api);

  let channel = open_out "./interface/bench_sha512_plain.symbol_layout" in
  Sexp.output_hum channel (External_layouts.GlobalSymbolLayout.sexp_of_t bench_sha512_plain_global);

  let channel = open_out "./interface/bench_sha512_plain.mem_interface" in
  let stack = External_layouts.StackLayout.from_file "./out/bench_sha512_plain.stack_layout" in
  Sexp.output_hum channel (Base_func_interface.sexp_of_t (Base_func_interface.add_stack_layout bench_sha512_plain stack));

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
