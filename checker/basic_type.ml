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

  let is_bv (e: t) : bool =
    match e with
    | Exp e -> BitVector.is_bv e
    | Top _ -> true

  let is_bool (e: t) : bool =
    match e with
    | Exp e -> Boolean.is_bool e
    | Top size -> size = 1

  let get_exp_bit_size (e: exp_t) : int =
    Z3.BitVector.get_size (Z3.Expr.get_sort e)

  let get_bit_size (e: t) : int =
    match e with
    | Exp e -> get_exp_bit_size e
    | Top size -> size

  let check_start_end
      (start_byte: int) (end_byte: int)
      (total_bit: int) (write_bit: int option) : (int * int * int) option =
    if start_byte >= 0 && start_byte < end_byte then
      let start_bit = start_byte * 8 in
      let end_bit = end_byte * 8 in
      if end_bit > total_bit then None
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
      (start_byte: int) (end_byte: int)
      (e: t) : t =
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

  let get_start_len (ctx: context) (start_byte: int) (len_byte: int) (e: t) : t =
    get_start_end ctx start_byte (start_byte + len_byte) e

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
      (start_byte: int) (end_byte: int) 
      (orig_e: t) (new_e: t) : 
      t * bool = (* new_e, is_overwrite *)
    if not (is_bv orig_e && is_bv new_e) then dep_type_error "set_start_end of non bv orig_e or new_e" else
    match orig_e, new_e with
    | Exp orig_e, Exp new_e ->
      let e, is_overwrite = set_exp_start_end ctx set_default_zero start_byte end_byte orig_e new_e in
      Exp e, is_overwrite
    | _, Top new_size ->
      begin match check_start_end start_byte end_byte (get_bit_size orig_e) (Some new_size) with
      | Some (start_bit, end_bit, total_bit) -> 
        Top total_bit,
        start_bit = 0 && end_bit = total_bit
      | None ->
        dep_type_error 
          (Printf.sprintf "set_exp_start_end invalid argument: start_byte %d, end_byte %d orig_e %s\n" 
            start_byte end_byte (Sexplib.Sexp.to_string (sexp_of_t orig_e)))
      end
    | Top total_bit, Exp new_e ->
      begin match check_start_end start_byte end_byte total_bit (Some (get_exp_bit_size new_e)) with
      | Some (start_bit, end_bit, total_bit) ->
        if start_bit = 0 && end_bit = total_bit then
          Exp new_e, true (* Full overwrite *)
        else 
          Top total_bit, false
      | None ->
        dep_type_error 
          (Printf.sprintf "set_exp_start_end invalid argument: start_byte %d, end_byte %d orig_e %s\n" 
            start_byte end_byte (Sexplib.Sexp.to_string (sexp_of_t orig_e)))
      end

  let set_start_len
      (ctx: context) (set_default_zero: bool)
      (start_byte: int) (len_byte: int) (orig_e: t) (new_e: t) : t * bool =
    set_start_end ctx set_default_zero start_byte (start_byte + len_byte) orig_e new_e

  let get_flag (e: t) : t =
    if is_bool e then e
    else dep_type_error "get_flag of non bool e"

  let set_flag (orig_e: t) (new_e: t) : t =
    if is_bool orig_e && is_bool new_e then new_e
    else dep_type_error "set_flag of non bool orig_e or new_e"

end

module TaintType = struct
  exception TaintTypeError of string

  let taint_type_error msg = raise (TaintTypeError ("[Taint Type Error] " ^ msg))

  type t = Z3Expr.expr (* Note: t must be a Boolean *)
  [@@deriving sexp]

  let set
      (ctx: context)
      (is_overwrite: bool) (orig_t: t) (new_t: t) : t =
    if is_overwrite then new_t
    else Boolean.mk_or ctx [orig_t; new_t]

end


module BasicType = struct
  exception BasicTypeError of string

  let basic_type_error msg = raise (BasicTypeError ("[Basic Type Error] " ^ msg))

  type t = DepType.t * TaintType.t
  [@@deriving sexp]

  let get_start_end
      (ctx: context)
      (start_byte: int) (end_byte: int) (e: t) : t =
    let dep, taint = e in
    DepType.get_start_end ctx start_byte end_byte dep,
    taint

  let get_start_len 
      (ctx: context) 
      (start_byte: int) (len_byte: int) (e: t) : t =
    get_start_end ctx start_byte (start_byte + len_byte) e

  let set_start_end
      (ctx: context) (set_default_zero: bool)
      (start_byte: int) (end_byte: int) 
      (orig_e: t) (new_e: t) : t =
    let orig_dep, orig_taint = orig_e in
    let new_dep, new_taint = new_e in
    let dep, is_overwrite = 
      DepType.set_start_end ctx set_default_zero start_byte end_byte orig_dep new_dep 
    in
    dep,
    TaintType.set ctx is_overwrite orig_taint new_taint

  let get_flag (e: t) : t =
    let dep, taint = e in
    DepType.get_flag dep, taint

  let set_flag (orig_e: t) (new_e: t) : t =
    let orig_dep, _ = orig_e in
    let new_dep, new_taint = new_e in
    DepType.set_flag orig_dep new_dep, new_taint

end
