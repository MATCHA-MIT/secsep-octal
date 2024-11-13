open Type

let salsa20_global : Single_type_infer.SingleTypeInfer.ArchType.MemType.t = [
  -2, [ ((SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], SingleTop) ];
  -3, [ ((SingleConst 0L, SingleConst 8L), RangeConst [(SingleConst 0L, SingleConst 8L)], SingleTop) ];
  -4, [ ((SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop) ];
]

let r = Isa_basic.IsaBasic.get_reg_idx
let total_reg_num = Isa_basic.IsaBasic.total_reg_num

(* Note that salsa20_global is not real global var, so I only put it in _start's mem interface *)
let standalone_salsa20 : Base_func_interface.t = [
  "salsa20_words", [
    (* r RSP, [ ((SingleConst 0L, SingleConst 0L), RangeConst [], SingleTop) ]; *)
    r RDI, [ ((SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop) ];
    r RSI, [ ((SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop) ];
  ];
  "salsa20_block", [
    (* r RSP, [ ((SingleConst 0L, SingleConst 0L), RangeConst [], SingleTop) ]; *)
    r RDI, [ ((SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop) ];
    r RSI, [ ((SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], SingleTop) ];
  ];
  "salsa20", [
    (* r RSP, [ ((SingleConst 0L, SingleConst 0L), RangeConst [], SingleTop) ]; *)
    r RDI, [ ((SingleConst 0L, SingleVar (r RSI)), RangeConst [(SingleConst 0L, SingleVar (r RSI))], SingleTop) ];
    r RDX, [ ((SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], SingleTop) ]
  ];
  "_start", salsa20_global @ [
    r RSP, [ ((SingleConst 0L, SingleConst 0L), RangeConst [], SingleTop) ]
  ];
]

let get_reg_taint (reg_taint: (Isa_basic.IsaBasic.register * bool) list) : (bool option) list =
  List.fold_left (
    fun (acc: (bool option) list) (reg, taint) ->
      List.mapi (
        fun i x ->
          if i = r reg then Some taint else x
      ) acc
  ) (List.init Isa_basic.IsaBasic.total_reg_num (fun _ -> None)) reg_taint

let standalone_salsa20_taint_api : Taint_api.TaintApi.t = [
  "salsa20",
  get_reg_taint [
    RDI, false;
    RSI, false;
    RDX, false;
    RCX, true;
  ], [
    r RSP, [];
    r RDI, [ ((SingleConst 0L, SingleVar (r RSI)), RangeConst [(SingleConst 0L, SingleVar (r RSI))], Some true) ];
    r RDX, [ ((SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], Some true) ]
  ];
  "_start",
  get_reg_taint [], [
    -2, [ ((SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], Some true) ];
    -3, [ ((SingleConst 0L, SingleConst 8L), RangeConst [(SingleConst 0L, SingleConst 8L)], Some true) ];
    -4, [ ((SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], Some true) ];
    r RSP, []
  ]
]

let demo : Base_func_interface.t = [
  "table_select", [
    -2, [ ((SingleConst 0L, SingleConst 24576L), RangeConst [(SingleConst 0L, SingleConst 24576L)], SingleTop) ];
    (* r RSP, [ ((SingleConst 0L, SingleConst 0L), RangeConst [], SingleTop) ]; *)
  ];
]

let bench_ed25519_plain : Base_func_interface.t = [
  "fiat_25519_carry_mul", [
    r RDI, [ (SingleConst 0L, SingleConst 40L), RangeConst [], SingleTop ];
    r RSI, [ (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop ];
    r RDX, [ (SingleConst 0L, SingleConst 40L), RangeConst [(SingleConst 0L, SingleConst 40L)], SingleTop ];
  ];
  "sc_muladd", [
    r RDI, [ (SingleConst 0L, SingleConst 32L), RangeConst [], SingleTop ];
    r RSI, [ (SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], SingleTop ];
    r RDX, [ (SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], SingleTop ];
    r RCX, [ (SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], SingleTop ];
  ];
  "sha512_block_data_order", [
    r RDI, [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop ];
    r RSI, [ (SingleConst 0L, SingleBExp (SingleMul, SingleConst 128L, SingleVar (r RDX))), RangeConst [(SingleConst 0L, SingleBExp (SingleMul, SingleConst 128L, SingleVar (r RDX)))], SingleTop ];
    -2 (* K512 *),  [ (SingleConst 0L, SingleConst 640L), RangeConst [(SingleConst 0L, SingleConst 640L)], SingleTop ];
  ];
  "sha512_final_impl", [
    r RDI, [ (SingleConst 0L, SingleVar (r RSI)), RangeConst [], SingleTop ];
    r RDX, [ 
      (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop;
      (SingleConst 64L, SingleConst 72L), RangeConst [(SingleConst 64L, SingleConst 72L)], SingleVar (total_reg_num + 1);
      (SingleConst 72L, SingleConst 80L), RangeConst [(SingleConst 72L, SingleConst 80L)], SingleVar (total_reg_num + 2);
      (SingleConst 80L, SingleConst 208L), RangeConst [(SingleConst 80L, SingleConst 208L)], SingleTop;
      (SingleConst 208L, SingleConst 212L), RangeConst [(SingleConst 208L, SingleConst 212L)], SingleVar (total_reg_num + 3);
      (SingleConst 212L, SingleConst 216L), RangeConst [(SingleConst 212L, SingleConst 216L)], SingleVar (total_reg_num + 4);
      (* (SingleConst 0L, SingleConst 216L), RangeConst [(SingleConst 0L, SingleConst 216L)], SingleTop  *)
    ];
    -2 (* K512 *),  [ (SingleConst 0L, SingleConst 640L), RangeConst [(SingleConst 0L, SingleConst 640L)], SingleTop ];
  ];
  "table_select", [
    r RDI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [], SingleTop;
    ];
    -3 (* k25519Precomp *), [ ((SingleConst 0L, SingleConst 24576L), RangeConst [(SingleConst 0L, SingleConst 24576L)], SingleTop) ];
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
  "SHA512_Init", [
    r RDI, [ 
      (SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop;
      (SingleConst 64L, SingleConst 72L), RangeConst [], SingleVar (total_reg_num + 1);
      (SingleConst 72L, SingleConst 80L), RangeConst [], SingleVar (total_reg_num + 2);
      (SingleConst 80L, SingleConst 208L), RangeConst [], SingleTop;
      (SingleConst 208L, SingleConst 212L), RangeConst [], SingleVar (total_reg_num + 3);
      (SingleConst 212L, SingleConst 216L), RangeConst [], SingleVar (total_reg_num + 4);
    ];
    -4 (* .LC0 *), [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], SingleTop ];
    -5 (* .LC4 *), [ (SingleConst 0L, SingleConst 8L), RangeConst [(SingleConst 0L, SingleConst 8L)], SingleTop ];
    -6 (* .LC1 *), [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], SingleTop ];
    -7 (* .LC2 *), [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], SingleTop ];
    -8 (* .LC3 *), [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], SingleTop ];
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
  "x25519_ge_scalarmult_base", [
    r RDI, [
      (SingleConst 0L, SingleConst 40L), RangeConst [], SingleTop;
      (SingleConst 40L, SingleConst 80L), RangeConst [], SingleTop;
      (SingleConst 80L, SingleConst 120L), RangeConst [], SingleTop;
      (SingleConst 120L, SingleConst 160L), RangeConst [], SingleTop;
    ];
    r RSI, [ (SingleConst 0L, SingleConst 32L), RangeConst [(SingleConst 0L, SingleConst 32L)], SingleTop ];
    -9 (* .LC6 *), [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], SingleTop ];
  ];
  "ED25519_sign", [
    r RDI, [ (SingleConst 0L, SingleConst 64L), RangeConst [], SingleTop ];
    r RSI, [ (SingleConst 0L, SingleVar (r RDX)), RangeConst [(SingleConst 0L, SingleVar (r RDX))], SingleTop ];
    r RCX, [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop ];
    -4 (* .LC0 *), [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], SingleTop ];
    -6 (* .LC1 *), [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], SingleTop ];
    -7 (* .LC2 *), [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], SingleTop ];
    -8 (* .LC3 *), [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], SingleTop ];
    -10 (* .LC8 *), [ (SingleConst 0L, SingleConst 16L), RangeConst [(SingleConst 0L, SingleConst 16L)], SingleTop ];
  ];
  "main", [
    -11, [ (SingleConst 0L, SingleConst 256L), RangeConst [(SingleConst 0L, SingleConst 256L)], SingleTop ];
    -12, [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop ];
    -13, [ (SingleConst 0L, SingleConst 64L), RangeConst [(SingleConst 0L, SingleConst 64L)], SingleTop ];
  ]
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
  let start_var: Taint_type_infer.TaintTypeInfer.TaintEntryType.t = (SingleVar 0, TaintVar 0) in
  let _, default_reg_type = Taint_type_infer.TaintTypeInfer.ArchType.RegType.init_reg_type start_var in
  let in_reg : Taint_type_infer.TaintTypeInfer.ArchType.RegType.t =
    update_reg_taint default_reg_type
      (get_reg_taint [
        RDI, false;
        RSI, false;
        RDX, false;
        RCX, true;
      ])
  in
  let out_reg = List.mapi (
    fun (i: int) entry ->
      if Isa_basic.IsaBasic.is_reg_idx_callee_saved i then entry
      else Single_exp.SingleExp.SingleTop, Taint_exp.TaintExp.TaintConst true 
  ) in_reg
  in
  {
    func_name = "memset";
    in_reg = in_reg;
    in_mem = [
      r RDI, [ 
        (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
        RangeConst [], 
        (Single_exp.SingleExp.SingleTop, Taint_exp.TaintExp.TaintVar (Isa_basic.IsaBasic.total_reg_num)) 
      ];
    ];
    context = []; (* TODO: Add mem implicit constraints here, boundary + non-overlap *)
    out_reg = out_reg;
    out_mem = [
      r RDI, [ 
        (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
        RangeConst [(Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX))], 
        (Single_exp.SingleExp.SingleTop, Taint_exp.TaintExp.TaintVar (Isa_basic.IsaBasic.total_reg_num)) 
      ];
    ];
    base_info = [
      r RDI, [ 
        (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
        RangeConst [], 
        BaseAsReg RDI
      ];
    ];
  }


let () = 
  let open Sexplib in
  let channel = open_out "./interface/standalone_salsa20.mem_interface" in
  Sexp.output_hum channel (Base_func_interface.sexp_of_t standalone_salsa20);
  let channel = open_out "./interface/standalone_salsa20.taint_api" in
  Sexp.output_hum channel (Taint_api.TaintApi.sexp_of_t standalone_salsa20_taint_api);
  let channel = open_out "./interface/demo.mem_interface" in
  Sexp.output_hum channel (Base_func_interface.sexp_of_t demo);
  let channel = open_out "./interface/bench_ed25519_plain.mem_interface" in
  Sexp.output_hum channel (Base_func_interface.sexp_of_t bench_ed25519_plain)


