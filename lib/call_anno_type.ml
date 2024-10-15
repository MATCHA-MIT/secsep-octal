open Isa_basic
open Reg_type_new
open Single_entry_type
open Taint_entry_type
open Mem_type_new
open Mem_offset_new

module CallAnno = struct

  exception CallAnnoError of string
  let transform_error msg = raise (CallAnnoError ("[Call Annotation Error] " ^ msg))

  module TaintRegType = RegType(TaintEntryType(SingleEntryType))

  (* which slot in parent's memory is this slot mapped to *)
  (* base pointer, slot's offset to base pointer, accessing full slot or not *)
  type slot_info = IsaBasic.imm_var_id * MemOffset.t * bool

  (* how is its base pointer passed during the call *)
  type base_info =
    | BaseAsReg of IsaBasic.register (* passed as an arg register *)
    | BaseAsSlot of IsaBasic.imm_var_id * MemOffset.t (* passed as a (full) slot in child's memory *)

  type slot_t = (slot_info * base_info)

  type t' = {
    pr_reg: TaintRegType.t;
    ch_mem: slot_t MemTypeBasic.mem_content;
  }
  type t = t' option

  let cmp_base_info (x: base_info) (y: base_info) : int =
    match x, y with
    | BaseAsReg r1, BaseAsReg r2 -> Int.compare (IsaBasic.get_reg_idx r1) (IsaBasic.get_reg_idx r2)
    | BaseAsSlot (v1, o1), BaseAsSlot (v2, o2) ->
      let cmp1 = Int.compare v1 v2 in
      if cmp1 != 0 then cmp1
      else MemOffset.cmp o1 o2
    | BaseAsReg _, BaseAsSlot _ -> -1
    | BaseAsSlot _, BaseAsReg _ -> 1

  let to_ocaml_string (anno: t) : string =
    match anno with
    | None -> "None"
    | Some _ -> "Some (call anno...)"

  let to_string (anno: t) : string = to_ocaml_string anno

end
