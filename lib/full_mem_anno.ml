open Isa_basic
open Mem_offset_new
open Taint_exp

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

  let to_ocaml_string (_: t) : string =
    "UNIMPLEMENTED"
    (* let (base, offset, full), taint = anno in *)

end