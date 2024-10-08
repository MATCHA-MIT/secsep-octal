open Isa_basic
open Mem_type_new
open Mem_offset_new

module CallAnno = struct

  (* which slot in parent's memory is this slot mapped to *)
  (* base pointer, slot's offset to base pointer, accessing full slot or not *)
  type slot_info = IsaBasic.imm_var_id * MemOffset.t * bool

  (* how is its base pointer passed during the call *)
  type base_info =
    | BaseAsReg of IsaBasic.register (* passed as an arg register *)
    | BaseAsSlot of IsaBasic.imm_var_id * MemOffset.t (* passed as a (full) slot in parent's memory *)

  type t' = (slot_info * base_info) MemTypeBasic.mem_content
  type t = t' option

  let to_ocaml_string (anno: t) : string =
    match anno with
    | None -> "None"
    | Some _ -> "Some (call anno...)"

  let to_string (anno: t) : string = to_ocaml_string anno

end
