open Type.Isa_basic
open Type.Smt_emitter
open Basic_type
open Reg_type_basic
open Z3
open Sexplib.Std

module RegRange = struct
  exception RegRangeError of string
  let reg_range_error msg = raise (RegRangeError ("[Reg Range Error] " ^ msg))

  type t = int64 * int64
  [@@deriving sexp]

  let from_infer_range (r: Type.Reg_range.RegRange.t) : t =
    match r with
    | RangeConst (x, y) -> x, y
    | RangeVar _ -> 0L, 0L
    | RangeExp (_, (x, y)) -> x, y

  let get_empty_range () : t = 0L, 0L

  let write_update (r: t) (off: t) : t =
    let r1, r2 = r in
    let o1, o2 = off in
    if r2 < o1 || o2 < r1 then reg_range_error "cannot merge range that are not adjacent";
    Int64.min r1 o1, Int64.max r2 o2

  let write_update_off_size (r: t) (off_size: int64 * int64) : t =
    let off, size = off_size in
    write_update r (off, Int64.add off size)

  let read_check (r: t) (off: t) : bool =
    let r1, r2 = r in
    let o1, o2 = off in
    r1 <= o1 && o2 <= r2

  let read_check_off_size (r: t) (off_size: int64 * int64) : bool =
    let off, size = off_size in
    read_check r (off, Int64.add off size)

  let check_subtype = read_check
  (* check_subtype sub sup: there should be sup belongs to sub *)
end

module RegType = struct
include RegTypeBasic
  exception RegTypeError of string

  let reg_type_error msg = raise (RegTypeError ("[Reg Type Error] " ^ msg))

  type range_t = RegRange.t
  [@@deriving sexp]

  type entry_t = BasicType.t
  [@@deriving sexp]

  type t = (range_t, entry_t) reg_content
  [@@deriving sexp]

  let get_reg_type_size_expected
      (ctx: context)
      (reg_type: t)
      (r: IsaBasic.register)
      (expected_size: int64 option) (* in bytes *)
      : entry_t =
    let reg_idx = IsaBasic.get_reg_idx r in
    let entry_range, entry_type = List.nth reg_type reg_idx in
    let off, size = IsaBasic.get_reg_offset_size r in
    if not (RegRange.read_check_off_size entry_range (off, size)) then
      reg_type_error "get_reg_type_size_expected: reg valid region check failed"
    else if Option.is_none expected_size || (size = Option.get expected_size) then
      BasicType.get_start_len ctx off size entry_type
    else
      reg_type_error "get_reg_type_size_expected: reg size is unexpected"

  let get_reg_type (ctx: context) (reg_type: t) (r: IsaBasic.register) : entry_t =
    get_reg_type_size_expected ctx reg_type r None

  let set_reg_type (ctx: context) (reg_type: t) (r: IsaBasic.register) (new_type: entry_t) : t =
    let reg_idx = IsaBasic.get_reg_idx r in
    let old_range, old_type = List.nth reg_type reg_idx in
    let off, size = IsaBasic.get_reg_offset_size r in
    let new_range = RegRange.write_update_off_size old_range (off, size) in
    let new_type_taint = snd new_type in
    let new_type = 
      BasicType.set_start_len ctx (IsaBasic.is_partial_update_reg_set_default_zero r) off size old_type new_type
    in
    (* Note: taint of xmm reg should only depends on the incoming new val!!!
      e.g. movq rdx xmm, the new taint of xmm should be taint of rdx, xmm is overwrite by zero extended value of rdx. *)
    let new_type = if IsaBasic.is_xmm r then
      fst new_type, new_type_taint
    else
      new_type
    in
    List.mapi (
      fun i entry_type -> if i = reg_idx then (new_range, new_type) else entry_type
    ) reg_type

  let set_reg_mult_type 
      (ctx: context) (reg_type: t) 
      (r_list: IsaBasic.register list) (new_type: entry_t) : t =
    let helper
        (acc: int64 * t) (r: IsaBasic.register) : int64 * t =
      let acc_off, acc_reg_type = acc in
      let reg_size = IsaBasic.get_reg_size r in
      let acc_end = Int64.add acc_off reg_size in
      let write_type = BasicType.get_start_end ctx acc_off acc_end new_type in
      acc_end,
      set_reg_type ctx acc_reg_type r write_type
    in
    let acc_off, reg_type = List.fold_left helper (0L, reg_type) r_list in
    if DepType.get_bit_size (fst new_type) = (Int64.to_int acc_off) * 8 then
      reg_type
    else reg_type_error "set_reg_mult_type: new_type size and dest size unmatched"

  let check_subtype
      (smt_ctx: SmtEmitter.t)
      (sub_r_type: t) (sup_r_type: t) : bool =
    List.fold_left2 (
      fun (acc: bool) (sub_range, sub_entry) (sup_range, sup_entry) ->
        let curr = BasicType.check_subtype smt_ctx false sub_entry sup_entry in
        (* Printf.printf "reg subtype check: %s(%d) -> %s(%d) : %b\n"
          (BasicType.sexp_of_t sub_entry |> Sexplib.Sexp.to_string_hum)
          (DepType.get_bit_size (fst sub_entry))
          (BasicType.sexp_of_t sup_entry |> Sexplib.Sexp.to_string_hum)
          (DepType.get_bit_size (fst sup_entry))
          curr; *)
        let range_check = RegRange.check_subtype sub_range sup_range in
        acc && curr && range_check
        
    ) true sub_r_type sup_r_type

  let check_taint_eq
      (smt_ctx: SmtEmitter.t)
      (sub_r_type: t) (sup_r_type: t) : bool =
    (* This is only used for check reg taint in call anno, so we do not need to check reg valid range *)
    List.fold_left2 (
      fun (acc: bool) (_, (_, sub_taint)) (_, (_, sup_taint)) ->
        acc &&
        TaintType.check_subtype smt_ctx true sub_taint sup_taint
    ) true sub_r_type sup_r_type

end
