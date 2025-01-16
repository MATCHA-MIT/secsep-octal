open Type.Isa_basic
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

  let top_bool_size = -1 (* Intend to distinguish Top bv and Top bool*)
  let top_unknown_size = 0 (* Intend to represent mem data type where size is not gained from basic types *)

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
      (start_byte: int64) (len_byte: int64) (orig_e: t) (new_e: t) : t * bool =
    set_start_end ctx set_default_zero start_byte (Int64.add start_byte len_byte) orig_e new_e

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
  let bop_update_flag_list (op: IsaBasic.bop) : IsaBasic.flag list =
    match op with
    | Add | Sub -> [ CF; PF; AF; ZF; SF; OF ]
    | _ -> dep_type_error "<TODO> not implemented yet"

  let uop_update_flag_list (op: IsaBasic.uop) : IsaBasic.flag list =
    match op with
    | _ -> dep_type_error "<TODO> not implemented yet"

  let top_update_flag_list (op: IsaBasic.top) : IsaBasic.flag list =
    match op with
    | _ -> dep_type_error "<TODO> not implemented yet"

  let get_top_flag_list (flag_list: IsaBasic.flag list) : (IsaBasic.flag * t) list =
    List.map (fun f -> f, get_top_flag ()) flag_list

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

  let get_add_carry (ctx: context) (e0: exp_t) (e1: exp_t) : exp_t =
    BitVector.mk_add_no_overflow ctx e0 e1 false |> Boolean.mk_not ctx

  let get_add_overflow (ctx: context) (e0: exp_t) (e1: exp_t) : exp_t =
    [ BitVector.mk_add_no_overflow ctx e0 e1 true; 
      BitVector.mk_add_no_underflow ctx e0 e1 ]
    |> Boolean.mk_and ctx |> Boolean.mk_not ctx

  let get_sub_carry (ctx: context) (e0: exp_t) (e1: exp_t) : exp_t =
    BitVector.mk_sub_no_underflow ctx e0 e1 false |> Boolean.mk_not ctx

  let get_sub_overflow (ctx: context) (e0: exp_t) (e1: exp_t) : exp_t =
    [ BitVector.mk_sub_no_underflow ctx e0 e1 true; 
      BitVector.mk_sub_no_overflow ctx e0 e1 ]
    |> Boolean.mk_and ctx |> Boolean.mk_not ctx

  let exe_bop (ctx: context) (op: IsaBasic.bop) (e_list: t list) (dest_size: int) : t * ((IsaBasic.flag * t) list) =
    let top_flag_list = bop_update_flag_list op |> get_top_flag_list in
    match extract_exp_or_top e_list with
    | None ->
      Top dest_size, top_flag_list
    | Some exp_list ->
      match op, exp_list with
      | Add, [ e0; e1 ] ->
        Exp (BitVector.mk_add ctx e0 e1),
        set_flag_list top_flag_list [
          CF, Exp (get_add_carry ctx e0 e1);
          OF, Exp (get_add_overflow ctx e0 e1);
        ]
      | _ -> dep_type_error "<TODO> not implemented yet"

  let exe_uop (ctx: context) (op: IsaBasic.uop) (e_list: t list) (dest_size: int) : t * ((IsaBasic.flag * t) list) =
    let top_flag_list = uop_update_flag_list op |> get_top_flag_list in
    match extract_exp_or_top e_list with
    | None ->
      Top dest_size, top_flag_list
    | Some exp_list ->
      match op, exp_list with
      | Inc, [ e ] ->
        let one = BitVector.mk_numeral ctx "1" (get_exp_bit_size e) in
        Exp (BitVector.mk_add ctx e one),
        set_flag_list top_flag_list [
          OF, Exp (get_add_overflow ctx e one);
        ]
      | Dec, [ e ] ->
        let one = BitVector.mk_numeral ctx "1" (get_exp_bit_size e) in
        Exp (BitVector.mk_sub ctx e one),
        set_flag_list top_flag_list [
          OF, Exp (get_sub_overflow ctx e one);
        ]
      | _ -> dep_type_error "<TODO> not implemented yet"

  let exe_top (_: context) (op: IsaBasic.top) (e_list: t list) (dest_size: int) : t * ((IsaBasic.flag * t) list) =
    let top_flag_list = top_update_flag_list op |> get_top_flag_list in
    match extract_exp_or_top e_list with
    | None ->
      Top dest_size, top_flag_list
    | _ -> dep_type_error "<TODO> not implemented yet"

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

  let exe (ctx: context) (e_list: t list) : t =
    Boolean.mk_or ctx e_list

end


module BasicType = struct
  exception BasicTypeError of string

  let basic_type_error msg = raise (BasicTypeError ("[Basic Type Error] " ^ msg))

  type t = DepType.t * TaintType.t
  [@@deriving sexp]

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

  let get_flag (e: t) : t =
    let dep, taint = e in
    DepType.get_flag dep, taint

  let set_flag (orig_e: t) (new_e: t) : t =
    let orig_dep, _ = orig_e in
    let new_dep, new_taint = new_e in
    DepType.set_flag orig_dep new_dep, new_taint

  let exe_helper 
      (dep_exe_op: context -> 'a -> DepType.t list -> int -> DepType.t * ((IsaBasic.flag * DepType.t) list)) 
      (ctx: context) (op: 'a) (e_list: t list) (dest_size: int) : 
      t * ((IsaBasic.flag * t) list) =
    let dep_list, taint_list = List.split e_list in
    let dep, dep_flag_list = dep_exe_op ctx op dep_list dest_size in
    let taint = TaintType.exe ctx taint_list in
    (dep, taint),
    List.map (fun (flag, flag_dep) -> flag, (flag_dep, taint)) dep_flag_list

  let exe_bop = exe_helper DepType.exe_bop
  let exe_uop = exe_helper DepType.exe_uop
  let exe_top = exe_helper DepType.exe_top

end
