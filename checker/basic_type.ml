open Type.Isa_basic
open Type.Smt_emitter
open Isa_flag_config
open Z3
open Z3_sexp
open Sexplib.Std

module DepType = struct
  exception DepTypeError of string

  let dep_type_error msg = raise (DepTypeError ("[Dep Type Error] " ^ msg))

  type exp_t = Z3Expr.expr
  [@@deriving sexp]

  type t =
    | Exp of exp_t
    | Top of int
  [@@deriving sexp]
  
  type map_t = (int * t) list (* Check the dict type *)
  [@@deriving sexp]

  type ctx_t = exp_t list (* Used to specify requirements on dep types *)
  [@@deriving sexp]
  let top_bool_size = -1 (* Intend to distinguish Top bv and Top bool*)
  let top_unknown_size = 0 (* Intend to represent mem data type where size is not gained from basic types *)

  let get_exp (e: t) : exp_t =
    match e with
    | Exp e -> e
    | Top _ -> dep_type_error "get_exp of top"

  let is_bv (e: t) : bool =
    match e with
    | Exp e -> BitVector.is_bv e
    | Top size -> size <> top_bool_size

  let is_bool (e: t) : bool =
    match e with
    | Exp e -> Boolean.is_bool e
    | Top size -> size = top_bool_size
  
  let get_exp_bit_size (e: exp_t) : int =
    Z3.BitVector.get_size (Z3.Expr.get_sort e)

  let get_bit_size (e: t) : int =
    match e with
    | Exp e -> get_exp_bit_size e
    | Top size -> size

  let get_dep_var_string = SmtEmitter.expr_var_str_of_single_var

  let substitute_exp_t
      (ctx: context)
      (context_var_map: map_t)
      (e: exp_t) : t =
    (* Used as repl_context_var *)
    (* Dirty implementation to judge whether a variable mapped to top appears in the exp *)
    let bv_top_var_str = "s-top-bv" in
    let bool_top_var_str = "s-top-bool" in
    let var_list, exp_list =
      List.map (
        fun (var_idx, exp) ->
          let var_exp =
            if is_bool exp then
              Boolean.mk_const_s ctx (get_dep_var_string var_idx)
            else if is_bv exp then
              BitVector.mk_const_s ctx (get_dep_var_string var_idx) (get_bit_size exp)
            else
              dep_type_error (Printf.sprintf "invalid dep exp %s" (Sexplib.Sexp.to_string_hum (sexp_of_t exp)))
          in
          match exp with
          | Exp exp -> (var_exp, exp)
          | Top size ->
            let top_exp = if size = top_bool_size then 
              Z3.Expr.mk_fresh_const ctx bool_top_var_str (Boolean.mk_sort ctx)
            else if size != top_unknown_size then
              Z3.Expr.mk_fresh_const ctx bv_top_var_str (BitVector.mk_sort ctx size)
            else begin
              (* TODO: check this, should not appear after replacing those size-undetermined SingleVar with SingleTop *)
              Printf.printf "substitute_exp_t: using 64-bit bv for Top top_unknown_size\n";
              Z3.Expr.mk_fresh_const ctx bv_top_var_str (BitVector.mk_sort ctx 64)
            end
            in
            (var_exp, top_exp)
      ) context_var_map |> List.split
    in
    let e_sub = Expr.substitute e var_list exp_list in
    let e_sub_string = Expr.to_string e_sub in
    if Str.string_match (Str.regexp {|.*s-top-.*|}) e_sub_string 0 then
      Top (get_exp_bit_size e)
    else
      Exp e_sub

  let substitute_exp_exp
      (exp_substitute: exp_t -> t)
      (e: exp_t) : exp_t =
    match exp_substitute e with
    | Top _ -> 
      dep_type_error 
      (Printf.sprintf "substitute_exp_exp get top for e %s" (Sexplib.Sexp.to_string_hum (sexp_of_exp_t e)))
    | Exp e -> e

  let substitute
      (exp_substitute: exp_t -> t)
      (e: t) : t =
    (* Used as repl_context_var *)
    match e with
    | Top _ -> e
    | Exp e -> exp_substitute e

  let check_eq
      (smt_ctx: SmtEmitter.t)
      (exp1: exp_t) (exp2: exp_t) : bool =
    SmtEmitter.check_compliance smt_ctx
      [ Boolean.mk_eq (fst smt_ctx) exp1 exp2 ]
    = SatYes

  let to_common_const (smt_ctx: SmtEmitter.t) (exp: exp_t) : int64 option =
    let ctx, _ = smt_ctx in
    let eq_int (exp: exp_t) (i: int64) : bool =
      check_eq smt_ctx exp (BitVector.mk_numeral ctx (Int64.to_string i) (get_exp_bit_size exp))
    in
    List.find_opt (eq_int exp) [1L; 2L; 4L; 8L; 16L]

  let get_const_exp (ctx: context) (c: int64) (size: int) : exp_t =
    BitVector.mk_numeral ctx (Int64.to_string c) size

  let get_const_type (ctx: context) (c: int64) (size: int) : t =
    Exp (get_const_exp ctx c size)


  let get_imm_exp_size_expected (ctx: context) (imm: IsaBasic.immediate) (expected_size: int64 option): exp_t =
    let imm = IsaBasic.simplify_imm imm in
    (* these sizes are in bytes *)
    match imm with
    | ImmNum (x, Some size) ->
      if Option.is_none expected_size || (size = Option.get expected_size) then
        get_const_exp ctx x ((Int64.to_int size) * 8)
      else
        dep_type_error "get_imm_exp_size_expected: size is unexpected"
    | ImmNum (x, None) ->
      get_const_exp ctx x ((Option.get expected_size |> Int64.to_int) * 8)
    | ImmLabel (label_var_id, Some size) ->
      if Option.is_none expected_size || (size = Option.get expected_size) then
        BitVector.mk_const_s ctx (get_dep_var_string label_var_id) ((Int64.to_int size) * 8)
      else
        dep_type_error "get_imm_exp_size_expected: size is unexpected"
    | ImmLabel (label_var_id, None) ->
      BitVector.mk_const_s ctx (get_dep_var_string label_var_id) ((Option.get expected_size |> Int64.to_int) * 8)
    | _ -> dep_type_error "unexpected ImmLabel / ImmBExp in get_imm_exp"

  let get_imm_exp (ctx: context) (imm: IsaBasic.immediate) : exp_t =
    get_imm_exp_size_expected ctx imm None

  let get_imm_type (ctx: context) (imm: IsaBasic.immediate) : t =
    Exp (get_imm_exp ctx imm)

  let get_mem_op_type 
      (ctx: context) 
      (disp: exp_t) (base: t)
      (index: t) (scale: exp_t) : t =
    match base, index with
    | Top _, _ | _, Top _ -> Top 64
    | Exp base, Exp index ->
      Exp (
        BitVector.mk_add ctx
          (BitVector.mk_add ctx base disp)
          (BitVector.mk_mul ctx index scale)
      )

  let check_start_end
      (start_byte: int) (end_byte: int)
      (total_bit: int) (write_bit: int option) : (int * int * int) option =
    if start_byte >= 0 && start_byte < end_byte then
      let start_bit = start_byte * 8 in
      let end_bit = end_byte * 8 in
      if total_bit != top_unknown_size && end_bit > total_bit then None
      else if write_bit = None || Option.get write_bit = end_bit - start_bit then
        Some (start_bit, end_bit, total_bit)
      else None
    else None

  let get_exp_start_end
      (ctx: context)
      (start_byte: int) (end_byte: int)
      (e: exp_t) : exp_t =
    match check_start_end start_byte end_byte (get_exp_bit_size e) None with
    | Some (start_bit, end_bit, _) ->
      BitVector.mk_extract ctx (end_bit - 1) start_bit e
    | None ->
      dep_type_error 
        (Printf.sprintf "get_exp_start_end invalid argument: start_byte %d, end_byte %d e %s\n" 
          start_byte end_byte (Sexplib.Sexp.to_string (sexp_of_exp_t e)))

  let get_start_end
      (ctx: context)
      (start_byte: int64) (end_byte: int64)
      (e: t) : t =
    let start_byte = Int64.to_int start_byte in
    let end_byte = Int64.to_int end_byte in
    if not (is_bv e) then dep_type_error "get_start_end of non bv e" else
    match e with
    | Exp e -> Exp (get_exp_start_end ctx start_byte end_byte e)
    | Top size ->
      begin match check_start_end start_byte end_byte size None with
      | Some (start_bit, end_bit, _) -> Top (end_bit - start_bit)
      | None -> 
        dep_type_error 
        (Printf.sprintf "get_start_end invalid argument: start_byte %d, end_byte %d e %s\n" 
          start_byte end_byte (Sexplib.Sexp.to_string (sexp_of_t e)))
      end

  let get_start_len (ctx: context) (start_byte: int64) (len_byte: int64) (e: t) : t =
    get_start_end ctx start_byte (Int64.add start_byte len_byte) e

  let get_top_len (len_byte: int64) (e: t) : t =
    (* This is used for memory slot whose dependent type cannot be tracked *)
    let len_bit = (Int64.to_int len_byte) * 8 in
    match e with
    | Top _ -> Top len_bit
    | Exp _ -> dep_type_error "this should be only called when the entry is top"

  let get_top () : t = Top top_unknown_size

  let set_exp_start_end
      (ctx: context)
      (set_default_zero: bool)
      (start_byte: int) (end_byte: int)
      (orig_e: exp_t) (new_e: exp_t) : 
      exp_t * bool = (* new_e, is_overwrite *)
    match check_start_end start_byte end_byte (get_exp_bit_size orig_e) (Some (get_exp_bit_size new_e)) with
    | None ->
      dep_type_error 
        (Printf.sprintf "set_exp_start_end invalid argument: start_byte %d, end_byte %d orig_e %s\n" 
          start_byte end_byte (Sexplib.Sexp.to_string (sexp_of_exp_t orig_e)))
    | Some (start_bit, end_bit, total_bit) ->
      let ext_bit = total_bit - end_bit in
      if set_default_zero then
        match start_bit, ext_bit with
        | (0, 0) -> new_e, true
        | (0, _) -> BitVector.mk_zero_ext ctx ext_bit new_e, true
        | (_, 0) -> 
          BitVector.mk_shl ctx (BitVector.mk_numeral ctx (Int.to_string start_bit) 64) new_e, true
        | (_, _) -> 
          BitVector.mk_zero_ext ctx ext_bit new_e |> 
            BitVector.mk_shl ctx (BitVector.mk_numeral ctx (Int.to_string start_bit) 64), true
      else
        match start_bit, ext_bit with
        | (0, 0) -> new_e, true
        | (0, _) ->
          let orig_high = BitVector.mk_extract ctx (total_bit - 1) end_bit orig_e in
          BitVector.mk_concat ctx orig_high new_e, false
        | (_, 0) ->
          let orig_low = BitVector.mk_extract ctx (start_bit - 1) 0 orig_e in
          BitVector.mk_concat ctx new_e orig_low, false
        | (_, _) ->
          let orig_high = BitVector.mk_extract ctx (total_bit - 1) end_bit orig_e in
          let orig_low = BitVector.mk_extract ctx (start_bit - 1) 0 orig_e in
          BitVector.mk_concat ctx new_e orig_low |> BitVector.mk_concat ctx orig_high, false

  let set_start_end 
      (ctx: context) (set_default_zero: bool)
      (start_byte: int64) (end_byte: int64) 
      (orig_e: t) (new_e: t) : 
      t * bool = (* new_e, is_overwrite *)
    let start_byte = Int64.to_int start_byte in
    let end_byte = Int64.to_int end_byte in
    if not (is_bv orig_e && is_bv new_e) then dep_type_error "set_start_end of non bv orig_e or new_e" else
    match orig_e, new_e with
    | Exp orig_e, Exp new_e ->
      let e, is_overwrite = set_exp_start_end ctx set_default_zero start_byte end_byte orig_e new_e in
      Exp e, is_overwrite
    | _, Top new_size ->
      begin match check_start_end start_byte end_byte (get_bit_size orig_e) (Some new_size) with
      | Some (start_bit, end_bit, total_bit) -> 
        Top total_bit,
        set_default_zero || (total_bit != top_unknown_size && start_bit = 0 && end_bit >= total_bit)
      | None ->
        dep_type_error 
          (Printf.sprintf "set_start_end invalid argument: start_byte %d, end_byte %d orig_e %s\n" 
            start_byte end_byte (Sexplib.Sexp.to_string (sexp_of_t orig_e)))
      end
    | Top total_bit, Exp new_e ->
      begin match check_start_end start_byte end_byte total_bit (Some (get_exp_bit_size new_e)) with
      | Some (start_bit, end_bit, total_bit) ->
        if set_default_zero || (total_bit != top_unknown_size && start_bit = 0 && end_bit >= total_bit) then
          let new_e = if set_default_zero then begin
            (* e.g., writing EAX zero-extends into RAX *)
            if start_bit != 0 then
              failwith "set_start_end: set_default_zero but start_bit != 0";
            BitVector.mk_zero_ext ctx (total_bit - end_bit) new_e
          end else
            new_e
          in
          Exp new_e, true (* Full overwrite *)
        else 
          Top total_bit, false
      | None ->
        dep_type_error 
          (Printf.sprintf "set_start_end invalid argument: start_byte %d, end_byte %d orig_e %s\n" 
            start_byte end_byte (Sexplib.Sexp.to_string (sexp_of_t orig_e)))
      end

  let set_start_len
      (ctx: context) (set_default_zero: bool)
      (start_byte: int64) (len_byte: int64) (orig_e: t) (new_e: t) : t * bool =
    set_start_end ctx set_default_zero start_byte (Int64.add start_byte len_byte) orig_e new_e

  let concat (ctx: context) (high: t) (low: t) : t =
    (* Used for concat type of multiple mem slots. If Top, return Top 0. *)
    match low, high with
    | Exp low_exp, Exp high_exp ->
      if BitVector.is_bv_numeral low_exp && BitVector.is_bv_numeral high_exp then
        Exp (BitVector.mk_concat ctx high_exp low_exp)
      else Top 0
    | _ -> Top 0

  let get_flag (e: t) : t =
    if is_bool e then e
    else dep_type_error "get_flag of non bool e"

  let set_flag (orig_e: t) (new_e: t) : t =
    if is_bool orig_e && is_bool new_e then new_e
    else dep_type_error "set_flag of non bool orig_e or new_e"

  let get_top_flag () : t = Top top_bool_size

  let extract_exp_or_top (e_list: t list) : (exp_t list) option =
    (* NOTE: I assume we can unify size of non-flag operands, so I assert sizes of all operands are the same.
       We may need to change this later to accomodate for different cases. *)
    let exp_list, top_size_list =
      List.partition_map (
        fun (x: t) ->
          match x with
          | Exp e -> Left e
          | Top size -> Right size
      ) e_list
    in
    match top_size_list with
    | [] -> Some exp_list
    | _ -> None

  (* NOTE: Actually, functions regarding bop, uop, and top should be merged.
     The current ugly code is due to the imperfect definition in inference tool *)
  (* TODO: deprecated? *)
  let bop_update_flag_list (op: IsaBasic.bop) : IsaBasic.flag list =
    match op with
    | Add | Adc | Sub | Sbb -> [ CF; PF; AF; ZF; SF; OF ]
    | Mul | Imul -> [ CF; OF ]
    | Sal | Sar | Shl | Shr -> [ CF; PF; ZF; SF; OF ] 
    | Rol | Ror -> [ CF; OF ]
    | Xor | And | Or -> [ CF; PF; ZF; SF; OF ]
    | CmovEq -> []
    | Bt -> [ CF ]
    | Punpck | Packxs -> []
    | Pshuf -> []
    | Padd | Psub | Pxor | Pandn | Pand | Por -> []
    | Psll | Psrl -> []
    | Xorp -> []

  (* TODO: deprecated? *)
  let uop_update_flag_list (op: IsaBasic.uop) : IsaBasic.flag list =
    match op with
    | Neg -> [ CF; PF; AF; ZF; SF; OF ]
    | Inc | Dec -> [ PF; AF; ZF; SF; OF ]
    | _ -> []

  (* TODO: deprecated? *)
  let top_update_flag_list (op: IsaBasic.top) : IsaBasic.flag list =
    match op with
    | Shld
    | Shrd -> [ CF; PF; ZF; SF; OF ]

  let get_top_flag_list (flag_list: IsaBasic.flag list) : (IsaBasic.flag * t) list =
    List.map (fun f -> f, get_top_flag ()) flag_list

  let get_top_flag_list_from_map (flag_list: (IsaBasic.flag * 'a) list) : (IsaBasic.flag * t) list =
    List.map (fun (f, _) -> f, get_top_flag ()) flag_list

  let set_flag_list 
      (orig_list: (IsaBasic.flag * t) list) 
      (update_flag_list: (IsaBasic.flag * t) list) : (IsaBasic.flag * t) list =
    List.fold_left (
      fun (acc: (IsaBasic.flag * t) list) (update_entry: IsaBasic.flag * t) ->
        let update_flag, _ = update_entry in
        let find, acc =
          List.fold_left_map (
            fun (acc: bool) (entry: IsaBasic.flag * t) ->
              if acc then acc, entry
              else if update_flag = fst entry then
                true, update_entry
              else false, entry
          ) false acc
        in
        if find then acc
        else dep_type_error "set_flag_list cannot find flag in orig_list"
    ) orig_list update_flag_list

  (* ************** *
   * FLAG UTILITIES *
   ******************)

  let ml_to_z3_bool (ctx: context) (b: bool) : exp_t =
    if b then (Boolean.mk_true ctx) else (Boolean.mk_false ctx)

  let bool_to_bv (ctx: context) (e: exp_t) (bv_size: int) : exp_t =
    Boolean.mk_ite ctx e
      (BitVector.mk_numeral ctx "1" bv_size)
      (BitVector.mk_numeral ctx "0" bv_size)

  let get_add_carry (ctx: context) (e0: exp_t) (e1: exp_t) : exp_t =
    BitVector.mk_add_no_overflow ctx e0 e1 false |> Boolean.mk_not ctx

  let get_sub_carry (ctx: context) (e0: exp_t) (e1: exp_t) : exp_t =
    BitVector.mk_sub_no_underflow ctx e0 e1 false |> Boolean.mk_not ctx
  
  let get_shl_carry (ctx: context) (e: exp_t) (cnt: exp_t) : exp_t =
    let bv_size = BitVector.get_size (Expr.get_sort e) - 1 in
    let top_bit = bv_size - 1 in
    let subcnt = BitVector.mk_sub ctx cnt (BitVector.mk_numeral ctx "1" bv_size) in
    let sub_shift = BitVector.mk_shl ctx e subcnt in
    ml_to_z3_bool ctx (BitVector.is_bv_bit1 (BitVector.mk_extract ctx top_bit top_bit sub_shift)) 

  let get_shr_carry (ctx: context) (e: exp_t) (cnt: exp_t) : exp_t =
    let bv_size = BitVector.get_size (Expr.get_sort e) in
    let subcnt = BitVector.mk_sub ctx cnt (BitVector.mk_numeral ctx "1" bv_size) in
    let sub_shift = BitVector.mk_lshr ctx e subcnt in
    ml_to_z3_bool ctx (BitVector.is_bv_bit1 (BitVector.mk_extract ctx 0 0 sub_shift)) 
 
  let get_rol_carry (ctx: context) (result: exp_t) : exp_t =
    ml_to_z3_bool ctx (BitVector.is_bv_bit1 (BitVector.mk_extract ctx 0 0 result))

  let get_ror_carry (ctx: context) (result: exp_t) : exp_t = 
    let top_bit = BitVector.get_size (Expr.get_sort result) in
    ml_to_z3_bool ctx (BitVector.is_bv_bit1 (BitVector.mk_extract ctx top_bit top_bit result)) 
  
  let get_parity (ctx: context) (e: exp_t) : exp_t =
    (* Extend the bits to 3-bit vectors so we can do addition *)
    let get_bit (idx: int) : exp_t =
      BitVector.mk_zero_ext ctx 2 (BitVector.mk_extract ctx idx idx e)
    in
    let rec sum_bits (acc: exp_t) (next: exp_t list) : exp_t =
      match next with
      | [] -> acc
      | first :: rest -> sum_bits (BitVector.mk_add ctx first acc) rest
    in 
    let bits = List.init 8 get_bit in
    let sum = sum_bits (List.hd bits) (List.tl bits) in
    ml_to_z3_bool ctx (BitVector.is_bv_bit0 (BitVector.mk_extract ctx 0 0 sum))
  
  let get_add_aux (ctx: context) (e0: exp_t) (e1: exp_t) : exp_t =
    let low_nib0 = BitVector.mk_extract ctx 3 0 e0 in
    let low_nib1 = BitVector.mk_extract ctx 3 0 e1 in
    get_add_carry ctx low_nib0 low_nib1

  let get_sub_aux (ctx: context) (e0: exp_t) (e1: exp_t) : exp_t =
    let high_nib0 = BitVector.mk_extract ctx 7 4 e0 in
    let high_nib1 = BitVector.mk_extract ctx 7 4 e1 in
    get_sub_carry ctx high_nib0 high_nib1 

  let get_zero (ctx: context) (e: exp_t) : exp_t =
    let bv_size = BitVector.get_size (Expr.get_sort e) in
    Boolean.mk_eq ctx e (BitVector.mk_numeral ctx "0" bv_size)

  let get_sign (ctx: context) (e: exp_t) : exp_t =
    let top_bit = (BitVector.get_size (Expr.get_sort e)) - 1 in
    ml_to_z3_bool ctx (BitVector.is_bv_bit1 (BitVector.mk_extract ctx top_bit top_bit e))

  let get_add_overflow (ctx: context) (e0: exp_t) (e1: exp_t) : exp_t =
    [ BitVector.mk_add_no_overflow ctx e0 e1 true; 
      BitVector.mk_add_no_underflow ctx e0 e1 ]
    |> Boolean.mk_and ctx |> Boolean.mk_not ctx

  let get_sub_overflow (ctx: context) (e0: exp_t) (e1: exp_t) : exp_t =
    [ BitVector.mk_sub_no_underflow ctx e0 e1 true; 
      BitVector.mk_sub_no_overflow ctx e0 e1 ]
    |> Boolean.mk_and ctx |> Boolean.mk_not ctx
  
  let get_shl_overflow (ctx: context) (dst: exp_t) (cnt: exp_t) (oflg: exp_t) : exp_t =
    let top_bit = (BitVector.get_size (Expr.get_sort dst)) - 1 in
    let next_bit = top_bit - 1 in
    let top_bv = BitVector.mk_extract ctx top_bit top_bit dst in
    let next_bv = BitVector.mk_extract ctx next_bit next_bit dst in
    let one = BitVector.mk_numeral ctx "1" (BitVector.get_size (Expr.get_sort cnt)) in
    Boolean.mk_ite ctx (Boolean.mk_eq ctx cnt one)
      (Boolean.mk_not ctx (Boolean.mk_eq ctx top_bv next_bv))
      oflg

  let get_ashr_overflow (ctx: context) (cnt: exp_t) (oflg: exp_t) : exp_t = 
    let one = BitVector.mk_numeral ctx "1" (BitVector.get_size (Expr.get_sort cnt)) in
    Boolean.mk_ite ctx (Boolean.mk_eq ctx cnt one)
      (Boolean.mk_false ctx)
      oflg

  let get_lshr_overflow (ctx: context) (dst: exp_t) (cnt: exp_t) (oflg: exp_t) : exp_t =
    let top_bit = (BitVector.get_size (Expr.get_sort dst)) - 1 in
    let one = BitVector.mk_numeral ctx "1" (BitVector.get_size (Expr.get_sort cnt)) in
    Boolean.mk_ite ctx (Boolean.mk_eq ctx cnt one)
      (ml_to_z3_bool ctx (BitVector.is_bv_bit1 (BitVector.mk_extract ctx top_bit top_bit dst)))
      oflg

  let get_rol_overflow (ctx: context) (result: exp_t) (cnt: exp_t) (oflg: exp_t) : exp_t =
    let top_bit = (BitVector.get_size (Expr.get_sort result)) - 1 in
    let top_bv = BitVector.mk_extract ctx top_bit top_bit result in
    let cflg_bv = BitVector.mk_extract ctx 0 0 result in
    let one = BitVector.mk_numeral ctx "1" (BitVector.get_size (Expr.get_sort cnt)) in
    Boolean.mk_ite ctx (Boolean.mk_eq ctx cnt one)
      (ml_to_z3_bool ctx (BitVector.is_bv_bit1 (Z3.BitVector.mk_xor ctx top_bv cflg_bv)))
      oflg

  let get_ror_overflow (ctx: context) (result: exp_t) (cnt: exp_t) (oflg: exp_t) : exp_t =
    let top_bit = (BitVector.get_size (Expr.get_sort result)) - 1 in
    let next_bit = top_bit - 1 in
    let top_bv = BitVector.mk_extract ctx top_bit top_bit result in 
    let next_bv = BitVector.mk_extract ctx next_bit next_bit result in
    let one = BitVector.mk_numeral ctx "1" (BitVector.get_size (Expr.get_sort cnt)) in
    Boolean.mk_ite ctx (Boolean.mk_eq ctx cnt one)
      (ml_to_z3_bool ctx (BitVector.is_bv_bit1 (Z3.BitVector.mk_xor ctx top_bv next_bv)))
      oflg
      

  (* ****************** *
   * SYMBOLIC EXECUTION *
   **********************)    

  type z3_binop = context -> exp_t -> exp_t -> exp_t
  [@@deriving sexp]
  
  type exe_result = t * ((IsaBasic.flag * t) list)
  [@@deriving sexp]
  
  type flag_func = (IsaBasic.flag * t) list -> (IsaBasic.flag * t) list
  [@@deriving sexp]
  
  let downsize_bv (ctx: context) (bv: exp_t) (size: int) : exp_t =
    let bv_size = BitVector.get_size (Expr.get_sort bv) in
    if bv_size <= size then bv
    else BitVector.mk_extract ctx (size - 1) 0 bv
  
  let exe_add (ctx: context) (dst: exp_t) (src: exp_t) (dest_size: int) : exe_result =
    let result = downsize_bv ctx (BitVector.mk_add ctx dst src) dest_size in
    Exp result, [
      CF, Exp (get_add_carry ctx dst src);
      PF, Exp (get_parity ctx result);
      AF, Exp (get_add_aux ctx dst src);
      ZF, Exp (get_zero ctx result);
      SF, Exp (get_sign ctx result);
      OF, Exp (get_add_overflow ctx dst src);
    ]

  let exe_adc (ctx: context) (dst: exp_t) (src: exp_t) (cflg: exp_t) (dest_size: int) : exe_result =
    let flag_inc = bool_to_bv ctx cflg dest_size in
    let addend = BitVector.mk_add ctx src flag_inc in
    exe_add ctx dst addend dest_size

  let exe_sub (ctx: context) (dst: exp_t) (src: exp_t) (dest_size: int) : exe_result =
    let result = downsize_bv ctx (BitVector.mk_sub ctx dst src) dest_size in
    Exp result, [
      CF, Exp (get_sub_carry ctx dst src);
      PF, Exp (get_parity ctx result);
      AF, Exp (get_sub_aux ctx dst src);
      ZF, Exp (get_zero ctx result);
      SF, Exp (get_sign ctx result);
      OF, Exp (get_sub_overflow ctx dst src);
    ]

  let exe_sbb (ctx: context) (dst: exp_t) (src: exp_t) (cflg: exp_t) (dest_size: int) : exe_result =
    let flag_inc = bool_to_bv ctx cflg dest_size in
    let subtrahend = BitVector.mk_add ctx src flag_inc in
    exe_sub ctx dst subtrahend dest_size

  let exe_multiplicative (ctx: context) (dst: exp_t) (src: exp_t) (dest_size: int) (signed: bool) : exe_result = 
    let src_size = dest_size / 2 in
    let ext_func = if signed then BitVector.mk_sign_ext else BitVector.mk_zero_ext in
    let src_ext = ext_func ctx src_size src in
    let dst_ext = ext_func ctx src_size dst in
    let result = BitVector.mk_mul ctx dst_ext src_ext in
    let top_half = BitVector.mk_extract ctx (dest_size - 1) src_size result in
    let zero = BitVector.mk_numeral ctx "0" src_size in
    let flags = Boolean.mk_not ctx (Boolean.mk_eq ctx top_half zero) in 
    Exp result, [
      CF, Exp flags;
      OF, Exp flags;
    ]

  let exe_shl (ctx: context) (dst: exp_t) (cnt: exp_t) (flag_lookup: IsaBasic.flag -> exp_t) : exe_result =
    let result = BitVector.mk_shl ctx dst cnt in
    Exp result, [
      CF, Exp (get_shl_carry ctx dst cnt);
      PF, Exp (get_parity ctx result); 
      ZF, Exp (get_zero ctx result);
      SF, Exp (get_sign ctx result);
      OF, Exp (get_shl_overflow ctx dst cnt (flag_lookup OF));
    ]

  let exe_sar (ctx: context) (dst: exp_t) (cnt: exp_t) (flag_lookup: IsaBasic.flag -> exp_t) : exe_result =
    let result = BitVector.mk_ashr ctx dst cnt in
    Exp result, [
      CF, Exp (get_shr_carry ctx dst cnt);
      PF, Exp (get_parity ctx result); 
      ZF, Exp (get_zero ctx result);
      SF, Exp (get_sign ctx result);
      OF, Exp (get_ashr_overflow ctx cnt (flag_lookup OF));
    ]

  let exe_shr (ctx: context) (dst: exp_t) (cnt: exp_t) (flag_lookup: IsaBasic.flag -> exp_t) : exe_result =
    (* cnt should also have the same size as the other operand *)
    let cnt = BitVector.mk_sign_ext ctx ((get_exp_bit_size dst) - (get_exp_bit_size cnt)) cnt in
    let result = BitVector.mk_lshr ctx dst cnt in
    Exp result, [
      CF, Exp (get_shr_carry ctx dst cnt);
      PF, Exp (get_parity ctx result); 
      ZF, Exp (get_zero ctx result);
      SF, Exp (get_sign ctx result);
      OF, Exp (get_lshr_overflow ctx dst cnt (flag_lookup OF));
    ]
     
  let exe_rol (ctx: context) (dst: exp_t) (cnt: exp_t) (flag_lookup: IsaBasic.flag -> exp_t) : exe_result =
    let result = BitVector.mk_ext_rotate_left ctx dst cnt in
    Exp result, [
      CF, Exp (get_rol_carry ctx result);
      OF, Exp (get_rol_overflow ctx result cnt (flag_lookup OF));
    ]

  let exe_ror (ctx: context) (dst: exp_t) (cnt: exp_t) (flag_lookup: IsaBasic.flag -> exp_t) : exe_result =
    let result = BitVector.mk_ext_rotate_right ctx dst cnt in
    Exp result, [
      CF, Exp (get_rol_carry ctx result);
      OF, Exp (get_ror_overflow ctx result cnt (flag_lookup OF));
    ]

  let exe_bitwise (ctx: context) (binop: z3_binop) (dst: exp_t) (src: exp_t) : exe_result =
    let result = binop ctx dst src in
    Exp result, [
      CF, Exp (Boolean.mk_false ctx);
      PF, Exp (get_parity ctx result);
      ZF, Exp (get_zero ctx result);
      SF, Exp (get_sign ctx result);
      OF, Exp (Boolean.mk_false ctx);
    ]
  
  let exe_bittest (ctx: context) (reg: exp_t) (idx: exp_t) : exe_result =
    let reg_size = BitVector.get_size (Expr.get_sort reg) in
    let one = BitVector.mk_numeral ctx "1" reg_size in
    let zero = BitVector.mk_numeral ctx "0" reg_size in
    let mask = BitVector.mk_shl ctx one idx in
    let result = BitVector.mk_and ctx reg mask in
    let flag = Boolean.mk_not ctx (Boolean.mk_eq ctx result zero) in
    Exp (BitVector.mk_numeral ctx "0" reg_size), [
      CF, Exp flag
    ]

  let exe_movz (ctx: context) (src: exp_t) (dest_size: int) : exe_result =
    let src_size = BitVector.get_size (Expr.get_sort src) in
    Exp (BitVector.mk_zero_ext ctx (dest_size - src_size) src), []

  let exe_movs (ctx: context) (src: exp_t) (dest_size: int) : exe_result =
    let src_size = BitVector.get_size (Expr.get_sort src) in
    Exp (BitVector.mk_sign_ext ctx (dest_size - src_size) src), []
  
  (* use sign-extend or extract to match the size *)
  let exe_mov (ctx: context) (src: exp_t) (dest_size: int) : exe_result =
    let src_size = BitVector.get_size (Expr.get_sort src) in
    if dest_size = src_size then
      Exp src, []
    else if dest_size < src_size then
      Exp (BitVector.mk_extract ctx (dest_size - 1) 0 src), []
    else
      Exp (BitVector.mk_sign_ext ctx (dest_size - src_size) src), []

  let exe_bswap (ctx: context) (src: exp_t) : exe_result =
    let byte_size = (BitVector.get_size (Expr.get_sort src)) / 8 in
    let rec get_rev_bytes ?(idx: int = 0) (acc: exp_t list) : exp_t list =
      if idx == byte_size then
        acc
      else
        let new_acc = (BitVector.mk_extract ctx (idx * 8 + 7) (idx * 8) src) :: acc in
        get_rev_bytes ~idx:(idx+1) new_acc
    in
    let rec concat_bytes (byte_list: exp_t list) : exp_t =
      match byte_list with
      | [] -> raise (dep_type_error "exe_bswap cannot run on empty bitvector")
      | [byte] -> byte
      | byte :: rest -> BitVector.mk_concat ctx byte (concat_bytes rest)
    in
    Exp (concat_bytes (get_rev_bytes [])), []

  let exe_neg (ctx: context) (src: exp_t) (dest_size: int) : exe_result =
    let zero = BitVector.mk_numeral ctx "0" dest_size in
    let result = BitVector.mk_sub ctx zero src in
    Exp result, [
      CF, Exp (Boolean.mk_not ctx (Boolean.mk_eq ctx zero src));
      PF, Exp (get_parity ctx result);
      AF, Exp (get_sub_aux ctx zero src);
      ZF, Exp (get_zero ctx result);
      SF, Exp (get_sign ctx result);
      OF, Exp (get_sub_overflow ctx zero src) 
    ]

  let exe_inc (ctx: context) (src: exp_t) (dest_size: int) : exe_result =
    let one = BitVector.mk_numeral ctx "1" dest_size in
    let result = BitVector.mk_add ctx src one in
    Exp result, [
      PF, Exp (get_parity ctx result);
      AF, Exp (get_add_aux ctx src one);
      ZF, Exp (get_zero ctx result);
      SF, Exp (get_sign ctx result);
      OF, Exp (get_add_overflow ctx src one);
    ]
  
  let exe_dec (ctx: context) (src: exp_t) (dest_size: int) : exe_result =
    let one = BitVector.mk_numeral ctx "1" dest_size in
    let result = BitVector.mk_sub ctx src one in
    Exp result, [
      PF, Exp (get_parity ctx result);
      AF, Exp (get_sub_aux ctx src one);
      ZF, Exp (get_zero ctx result);
      SF, Exp (get_sign ctx result);
      OF, Exp (get_sub_overflow ctx src one);
    ]
    

  let exe_shld (ctx: context) (dst: exp_t) (src: exp_t) (cnt: exp_t) (cflg: exp_t) (dest_size: int) : exe_result =
    let cflg_bv = bool_to_bv ctx cflg 1 in 
    let merged = BitVector.mk_concat ctx (BitVector.mk_concat ctx cflg_bv dst) src in
    let cnt = BitVector.mk_extract ctx 4 0 cnt in
    let shifted = BitVector.mk_shl ctx merged cnt in
    let result = BitVector.mk_extract ctx (dest_size - 1) 0 shifted in
    let dst_sign = BitVector.mk_extract ctx (dest_size - 1) (dest_size - 1) dst in
    let result_sign = BitVector.mk_extract ctx (dest_size - 1) (dest_size - 1) result in
    let cflg_bit = BitVector.mk_extract ctx dest_size dest_size shifted in
    let cflg = ml_to_z3_bool ctx (BitVector.is_bv_bit1 cflg_bit) in
    let oflg =
      let cnt_one = BitVector.mk_numeral ctx "1" (BitVector.get_size (Expr.get_sort cnt)) in
      Boolean.mk_ite ctx (Boolean.mk_eq ctx cnt cnt_one)
        (Boolean.mk_not ctx (Boolean.mk_eq ctx dst_sign result_sign))
        (Boolean.mk_false ctx)
    in
    Exp result, [
      CF, Exp cflg;
      PF, Exp (get_parity ctx result);
      ZF, Exp (get_zero ctx result);
      SF, Exp (get_sign ctx result);
      OF, Exp oflg;
    ]

  let exe_shrd (ctx: context) (dst: exp_t) (src: exp_t) (cnt: exp_t) (cflg: exp_t) (dest_size: int) : exe_result =
    let cflg_bv = bool_to_bv ctx cflg 1 in 
    let merged = BitVector.mk_concat ctx src (BitVector.mk_concat ctx dst cflg_bv) in
    let cnt = BitVector.mk_extract ctx 4 0 cnt in
    let shifted = BitVector.mk_lshr ctx merged cnt in
    let result = BitVector.mk_extract ctx dest_size 1 shifted in
    let dst_sign = BitVector.mk_extract ctx (dest_size - 1) (dest_size - 1) dst in
    let result_sign = BitVector.mk_extract ctx (dest_size - 1) (dest_size - 1) result in
    let cflg_bit = BitVector.mk_extract ctx 0 0 shifted in
    let cflg = ml_to_z3_bool ctx (BitVector.is_bv_bit1 cflg_bit) in
    let oflg =
      let cnt_one = BitVector.mk_numeral ctx "1" (BitVector.get_size (Expr.get_sort cnt)) in
      Boolean.mk_ite ctx (Boolean.mk_eq ctx cnt cnt_one)
        (Boolean.mk_not ctx (Boolean.mk_eq ctx dst_sign result_sign))
        (Boolean.mk_false ctx)
    in
    Exp result, [
      CF, Exp cflg;
      PF, Exp (get_parity ctx result);
      ZF, Exp (get_zero ctx result);
      SF, Exp (get_sign ctx result);
      OF, Exp oflg;
    ]

  (* <TODO> Check the following new API *)
  let exe_bop 
      (ctx: context) (op: IsaBasic.bop) 
      (src_list: t list) 
      (get_src_flag_func: IsaBasic.flag -> t) 
      (dest_size: int) (* in bits *)
      : exe_result =
    let src_flag_list, dest_flag_list = IsaFlagConfig.get_bop_config op in
    let src_flag_type_list = List.map get_src_flag_func src_flag_list in
    let top_flag_list = dest_flag_list |> get_top_flag_list_from_map in
    match extract_exp_or_top src_list, extract_exp_or_top src_flag_type_list  with
    | None, _ | _, None ->
      Top dest_size, top_flag_list
    | Some src_exp_list, Some src_flag_list ->
      let flag_lookup (flag: IsaBasic.flag) : exp_t = 
        match get_src_flag_func flag with
        | Top _ -> Expr.mk_const_s ctx "top" (Boolean.mk_sort ctx) (* TODO Isn't there a get_top function somewhere? I should use that instead to keep things consistent. *)
        | Exp flag_exp -> flag_exp
      in
      let dep_result, flag_vals = begin
      match op, src_exp_list, src_flag_list with
      | Add,    [ dst; src ], [ ]      -> exe_add ctx dst src dest_size
      | Adc,    [ dst; src ], [ cflg ] -> exe_adc ctx dst src cflg dest_size
      | Sub,    [ dst; src ], [ ]      -> exe_sub ctx dst src dest_size
      | Sbb,    [ dst; src ], [ cflg ] -> exe_sbb ctx dst src cflg dest_size
      | Mul,    [ dst; src ], [ ]      -> exe_multiplicative ctx dst src dest_size false
      | Imul,   [ dst; src ], [ ]      -> exe_multiplicative ctx dst src dest_size true
      | Sal,    [ dst; cnt ], [ ]
      | Shl,    [ dst; cnt ], [ ]      -> exe_shl ctx dst cnt flag_lookup
      | Sar,    [ dst; cnt ], [ ]      -> exe_sar ctx dst cnt flag_lookup
      | Shr,    [ dst; cnt ], [ ]      -> exe_shr ctx dst cnt flag_lookup
      | Rol,    [ dst; cnt ], [ ]      -> exe_rol ctx dst cnt flag_lookup
      | Ror,    [ dst; cnt ], [ ]      -> exe_ror ctx dst cnt flag_lookup
      | Xor,    [ dst; src ], [ ]      -> exe_bitwise ctx BitVector.mk_xor dst src
      | And,    [ dst; src ], [ ]      -> exe_bitwise ctx BitVector.mk_and dst src
      | Or,     [ dst; src ], [ ]      -> exe_bitwise ctx BitVector.mk_or  dst src
      | CmovEq, [ dst; src ], [ zf ]   -> Exp (Boolean.mk_ite ctx zf src dst), []
      | Bt,     [ reg; idx ], [ ]      -> exe_bittest ctx  reg idx
      | Punpck, [ _; _ ],     [ ]      -> Top 128, [ ]
      | Packxs, [ _; _ ],     [ ]      -> Top 128, [ ]
      | Padd,   [ _; _ ],     [ ]      -> Top 128, [ ]
      | Psub,   [ _; _ ],     [ ]      -> Top 128, [ ]
      | Pxor,   [ _; _ ],     [ ]      -> Top 128, [ ]
      | Pandn,  [ _; _ ],     [ ]      -> Top 128, [ ]
      | Pand,   [ _; _ ],     [ ]      -> Top 128, [ ]
      | Por,    [ _; _ ],     [ ]      -> Top 128, [ ]
      | Psll,   [ _; _ ],     [ ]      -> Top 128, [ ]
      | Psrl,   [ _; _ ],     [ ]      -> Top 128, [ ]
      | Xorp,   [ _; _ ],     [ ]      -> Top 128, [ ]
      | Pshuf,  [ _; _ ],     [ ]      -> Top 128, [ ]
      | _ -> dep_type_error "<TODO> not implemented yet"
      end
      in
      dep_result, set_flag_list top_flag_list flag_vals

  let exe_uop
      (ctx: context) (op: IsaBasic.uop) 
      (src_list: t list) 
      (get_src_flag_func: IsaBasic.flag -> t) 
      (dest_size: int) (* in bits *)
      : t * ((IsaBasic.flag * t) list) =
    let src_flag_list, dest_flag_list = IsaFlagConfig.get_uop_config op in
    let src_flag_type_list = List.map get_src_flag_func src_flag_list in
    let top_flag_list = dest_flag_list |> get_top_flag_list_from_map in
    match extract_exp_or_top src_list, extract_exp_or_top src_flag_type_list  with
    | None, _ | _, None ->
      Top dest_size, top_flag_list
    | Some src_exp_list, Some src_flag_list -> 
      let dep_result, flag_vals = begin
      match op, src_exp_list, src_flag_list with
      | Mov,   [ src ],      [ ] -> exe_movs ctx src dest_size
      | MovZ,  [ src ],      [ ] -> exe_movz ctx src dest_size
      | MovS,  [ src ],      [ ] -> exe_movs ctx src dest_size
      | Lea,   [ src ],      [ ] -> exe_mov ctx src dest_size
      | Not,   [ src ],      [ ] -> Exp (BitVector.mk_not ctx src), []
      | Bswap, [ src ],      [ ] -> exe_bswap ctx src
      | Neg,   [ src ],      [ ] -> exe_neg ctx src dest_size
      | Inc,   [ src ],      [ ] -> exe_inc ctx src dest_size
      | Dec,   [ src ],      [ ] -> exe_dec ctx src dest_size
      | _ -> dep_type_error "<TODO> not implemented yet"
      end
      in
      dep_result, set_flag_list top_flag_list flag_vals

  let exe_top
      (ctx: context) (op: IsaBasic.top) 
      (src_list: t list) 
      (get_src_flag_func: IsaBasic.flag -> t) 
      (dest_size: int) (* in bits *)
      : t * ((IsaBasic.flag * t) list) =
    let src_flag_list, dest_flag_list = IsaFlagConfig.get_top_config op in
    let src_flag_type_list = List.map get_src_flag_func src_flag_list in
    let top_flag_list = dest_flag_list |> get_top_flag_list_from_map in
    match extract_exp_or_top src_list, extract_exp_or_top src_flag_type_list  with
    | None, _ | _, None ->
      Top dest_size, top_flag_list
    | Some src_exp_list, Some src_flag_list -> 
      begin
      match op, src_exp_list, src_flag_list with
      | Shld, [ dst; src; cnt ], [ cflg ] -> exe_shld ctx dst src cnt cflg dest_size
      | Shrd, [ dst; src; cnt ], [ cflg ] -> exe_shrd ctx dst src cnt cflg dest_size
      | _ -> dep_type_error "<TODO> not implemented yet"
      end
  
  let check_subtype
      (smt_ctx: SmtEmitter.t)
      (sub_exp: t) (sup_exp: t) : bool =
    match sub_exp, sup_exp with
    | _, Top _ -> true
    | Top _, Exp _ -> false
    | Exp sub_e, Exp sup_e ->
      SmtEmitter.check_compliance smt_ctx
        [ Boolean.mk_eq (fst smt_ctx) sub_e sup_e ] = SatYes

end

module TaintType = struct
  exception TaintTypeError of string

  let taint_type_error msg = raise (TaintTypeError ("[Taint Type Error] " ^ msg))

  type t = Z3Expr.expr (* Note: t must be a Boolean *)
  [@@deriving sexp]

  type map_t = (int * t) list (* Check the dict type *)
  [@@deriving sexp]

  type ctx_t = t list (* Used to specify requirements on taint types *)
  [@@deriving sexp]

  let get_taint_var_string = SmtEmitter.expr_var_str_of_taint_var

  let substitute
      (ctx: context)
      (context_var_map: map_t)
      (e: t) : t =
    (* Used as repl_context_var *)
    let var_list, exp_list = List.split context_var_map in
    let var_exp_list =
      List.map (
        fun (var_idx: int) ->
          Boolean.mk_const_s ctx (get_taint_var_string var_idx)
      ) var_list
    in
    Expr.substitute e var_exp_list exp_list

  let get_untaint_exp (ctx: context) : t =
    Boolean.mk_false ctx

  let get_taint_exp (ctx: context) : t =
    Boolean.mk_true ctx

  let set
      (ctx: context)
      (is_overwrite: bool) (orig_t: t) (new_t: t) : t =
    if is_overwrite then new_t
    else Boolean.mk_or ctx [orig_t; new_t]

  let exe (ctx: context) (e_list: t list) : t =
    Boolean.mk_or ctx e_list

  let check_subtype
    (smt_ctx: SmtEmitter.t)
    (must_equal: bool)
    (sub_exp: t) (sup_exp: t) : bool =
  let ctx, _ = smt_ctx in
  let check_target =
    if must_equal then
      (* <TODO> Difference between mk_eq and mk_iff? Which one should we use? *)
      Boolean.mk_eq ctx sub_exp sup_exp
    else
      Boolean.mk_implies ctx sub_exp sup_exp
  in
  SmtEmitter.check_compliance smt_ctx [ check_target ] = SatYes

  let check_untaint
      (smt_ctx: SmtEmitter.t)
      (exp: t) : bool =
    check_subtype smt_ctx false exp (get_untaint_exp (fst smt_ctx))

end


module BasicType = struct
  exception BasicTypeError of string

  let basic_type_error msg = raise (BasicTypeError ("[Basic Type Error] " ^ msg))

  type t = DepType.t * TaintType.t
  [@@deriving sexp]

  type map_t = DepType.map_t * TaintType.map_t
  [@@deriving sexp]

  type ctx_t = DepType.ctx_t * TaintType.ctx_t
  [@@deriving sexp]

  let append_ctx (ctx_list: ctx_t) (new_constraint: DepType.exp_t) : ctx_t =
    let dep_ctx, taint_ctx = ctx_list in
    (new_constraint :: dep_ctx, taint_ctx)

  let substitute
      (dep_substitute: DepType.t -> DepType.t)
      (taint_substitute: TaintType.t -> TaintType.t)
      (e: t) : t =
    (* Used as repl_context_var *)
    let dep, taint = e in
    dep_substitute dep, taint_substitute taint

  let get_const_type (ctx: context) (c: int64) (size: int) : t =
    DepType.get_const_type ctx c size,
    TaintType.get_untaint_exp ctx

  let get_imm_type (ctx: context) (imm: IsaBasic.immediate) : t =
    DepType.get_imm_type ctx imm,
    TaintType.get_untaint_exp ctx

  let get_mem_op_type 
      (ctx: context) 
      (disp: DepType.exp_t) (base: t)
      (index: t) (scale: DepType.exp_t) : t =
    let base_dep, base_taint = base in
    let index_dep, index_taint = index in
    DepType.get_mem_op_type ctx disp base_dep index_dep scale,
    TaintType.exe ctx [ base_taint; index_taint ]

  let get_start_end
      (ctx: context)
      (start_byte: int64) (end_byte: int64) (e: t) : t =
    let dep, taint = e in
    DepType.get_start_end ctx start_byte end_byte dep,
    taint

  let get_start_len 
      (ctx: context) 
      (start_byte: int64) (len_byte: int64) (e: t) : t =
    get_start_end ctx start_byte (Int64.add start_byte len_byte) e

  let get_top_len (len_byte: int64) (e: t) : t =
    let dep, taint = e in
    DepType.get_top_len len_byte dep,
    taint

  let set_start_end
      (ctx: context) (set_default_zero: bool)
      (start_byte: int64) (end_byte: int64) 
      (orig_e: t) (new_e: t) : t =
    let orig_dep, orig_taint = orig_e in
    let new_dep, new_taint = new_e in
    let dep, is_overwrite = 
      DepType.set_start_end ctx set_default_zero start_byte end_byte orig_dep new_dep 
    in
    dep,
    TaintType.set ctx is_overwrite orig_taint new_taint

  let set_start_len
      (ctx: context) (set_default_zero: bool)
      (start_byte: int64) (len_byte: int64) 
      (orig_e: t) (new_e: t) : t =
    set_start_end ctx set_default_zero start_byte (Int64.add start_byte len_byte) orig_e new_e

  let concat_same_taint (smt_ctx: SmtEmitter.t) (high: t) (low: t) : t option =
    let high_dep, high_taint = high in
    let low_dep, low_taint = low in
    if TaintType.check_subtype smt_ctx true high_taint low_taint then
      Some (DepType.concat (fst smt_ctx) high_dep low_dep, low_taint)
    else None

  let get_flag (e: t) : t =
    let dep, taint = e in
    DepType.get_flag dep, taint

  let set_flag (orig_e: t) (new_e: t) : t =
    let orig_dep, _ = orig_e in
    let new_dep, new_taint = new_e in
    DepType.set_flag orig_dep new_dep, new_taint

  let exe_helper 
      (dep_exe_op: context -> 'a -> DepType.t list -> (IsaBasic.flag -> DepType.t) -> int -> DepType.t * ((IsaBasic.flag * DepType.t) list)) 
      (get_flag_config: 'a -> IsaFlagConfig.t)
      (ctx: context) (op: 'a) (e_list: t list) (get_src_flag_func: IsaBasic.flag -> t) (dest_size: int) (* in bits *)
      : t * ((IsaBasic.flag * t) list) =
    let get_src_flag_dep = fun f -> get_src_flag_func f |> fst in
    let get_src_flag_taint = fun f -> get_src_flag_func f |> snd in
    let dep_list, taint_list = List.split e_list in
    let dep, dep_flag_list = dep_exe_op ctx op dep_list get_src_flag_dep dest_size in
    let src_flag_list, update_flag_map = get_flag_config op in
    let src_flag_taint_list = List.map get_src_flag_taint src_flag_list in
    let taint = TaintType.exe ctx (taint_list @ src_flag_taint_list) in
    (dep, taint),
    List.map2 (
      fun (flag, flag_dep) (flag2, self_is_src) -> 
        let flag_taint =
          if flag <> flag2 then basic_type_error "update flag list does not match"
          else if self_is_src then TaintType.exe ctx [ taint; get_src_flag_taint flag ]
          else taint
        in
        flag, (flag_dep, flag_taint)
      ) dep_flag_list update_flag_map

  let exe_bop  = exe_helper DepType.exe_bop  IsaFlagConfig.get_bop_config
  let exe_uop  = exe_helper DepType.exe_uop  IsaFlagConfig.get_uop_config
  let exe_top  = exe_helper DepType.exe_top  IsaFlagConfig.get_top_config

  let check_subtype
      (smt_ctx: SmtEmitter.t)
      (taint_must_equal: bool)
      (sub_exp: t) (sup_exp: t) : bool =
    let sub_dep, sub_taint = sub_exp in
    let sup_dep, sup_taint = sup_exp in
    DepType.check_subtype smt_ctx sub_dep sup_dep &&
    TaintType.check_subtype smt_ctx taint_must_equal sub_taint sup_taint

end
