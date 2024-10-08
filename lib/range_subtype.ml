open Single_entry_type
open Mem_offset_new
open Constraint
open Arch_type
open Pretty_print

module RangeSubtype = struct
  exception RangeSubtypeError of string

  let range_subtype_error msg = raise (RangeSubtypeError ("[Range Subtype Error] " ^ msg))

  type type_rel = {
    var_idx: MemRange.range_var_id;
    sol: MemRange.t option;
    subtype_list: MemRange.t list;
    supertype_list: MemRange.range_var_id list;
  }

  type t = type_rel list

  module ArchType = ArchType (SingleEntryType)

  

end
