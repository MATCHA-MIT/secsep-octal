open Type.Isa_basic
open Type.Set_sexp
open Mem_anno

module StackSpillInfo = struct
  exception StackSpillInfoError of string

  let stack_spill_info_error msg = raise (StackSpillInfoError ("[Stack Spill Info error] " ^ msg))

  type t = IntSet.t
  [@@deriving sexp]

  (* TODO: Get stack spill info from Type.StackSpillInfo.t *)

  let is_spill (spill_info: t) (slot_info: MemAnno.slot_t) : bool =
    let ptr, slot_idx, _, num_slot = slot_info in
    ptr = IsaBasic.rsp_idx &&
    num_slot = 1 &&
    IntSet.mem slot_idx spill_info

end
