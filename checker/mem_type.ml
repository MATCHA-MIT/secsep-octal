open Type.Ptr_info
open Basic_type
(* open Mem_anno *)
open Sexplib.Std

module MemOffset = struct
  exception MemOffsetError of string

  let mem_offset_error msg = raise (MemOffsetError ("[Mem Offset Error] " ^ msg))

  type t = DepType.exp_t * DepType.exp_t
  [@@deriving sexp]

end

module MemRange = struct
  exception MemRangeError of string

  let mem_range_error msg = raise (MemRangeError ("[Mem Range Error] " ^ msg))

  type t = MemOffset.t list
  [@@deriving sexp]

end

module MemType = struct
  exception MemTypeError of string

  let mem_type_error msg = raise (MemTypeError ("[Mem Type Error] " ^ msg))

  type mem_slot = MemOffset.t * MemRange.t * BasicType.t
  [@@deriving sexp]

  type mem_part = PtrInfo.t * (mem_slot list)
  [@@deriving sexp]

  type mem_content = mem_part list
  [@@deriving sexp]

  type t = mem_content
  [@@deriving sexp]

end

