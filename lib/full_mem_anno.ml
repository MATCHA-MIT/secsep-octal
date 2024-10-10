open Isa_basic
open Mem_offset_new
open Taint_exp
open Smt_emitter

module FullMemAnno = struct

  (* base pointer, slot's offset to base pointer, accessing full slot or not *)
  type slot_t = IsaBasic.imm_var_id * MemOffset.t * bool

  (* taint info of the slot *)
  type taint_t = TaintExp.t

  type t = (slot_t option) * (taint_t option)

  let make_empty () : t = (None, None)

  let update_taint (anno: t) (taint: taint_t) : t =
    let slot_info, _ = anno in
    (slot_info, Some taint)

  let get_slot (anno: t) : slot_t option =
    let slot_info, _ = anno in
    slot_info

  let get_taint (anno: t) : taint_t option =
    let _, taint_info = anno in
    taint_info

  let slot_to_string (slot_info: slot_t option) : string =
    match slot_info with
    | None -> "None"
    | Some (base, offset, full) ->
      Printf.sprintf "base ptr: %d, offset: %s, full: %b"
        base
        (MemOffset.to_string offset)
        full
  
  let taint_to_string (taint_info: taint_t option) : string =
    match taint_info with
    | None -> "None"
    | Some taint -> TaintExp.to_string taint

  let to_string (anno: t) : string =
    let slot_info, taint_info = anno in
    Printf.sprintf "slot: %s; taint: %s"
      (slot_to_string slot_info)
      (taint_to_string taint_info)

  let to_ocaml_string (anno: t) : string =
    let slot_info, taint_info = anno in
    let slot_str = match slot_info with
    | None -> "None"
    | Some (base, offset, full) ->
      Printf.sprintf "Some (%d, %s, %b)"
        base
        (MemOffset.to_ocaml_string offset)
        full
    in
    let taint_str = match taint_info with
    | None -> "None"
    | Some taint -> Printf.sprintf "Some %s" (TaintExp.to_ocaml_string taint)
    in
    Printf.sprintf "(%s, %s)" slot_str taint_str

  let check_slot
      (smt_ctx: SmtEmitter.t)
      (addr_off: MemOffset.t)
      (slot_info: slot_t) : bool =
    let _, s_off, is_full = slot_info in
    match MemOffset.offset_full_cmp smt_ctx addr_off s_off CmpEqSubset with
    | Eq -> true
    | Subset -> not is_full
    | _ -> false

end