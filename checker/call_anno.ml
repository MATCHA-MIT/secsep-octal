open Basic_type
open Mem_anno
open Reg_type
open Mem_type

module CallAnno = struct
  exception CallAnnoError of string

  type t = {
    pr_reg: RegType.t;
    ctx_map: BasicType.map_t;
    mem_map: MemAnno.slot_t MemType.mem_content;
  }



end
