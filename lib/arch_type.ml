open Entry_type
open Reg_type
open Mem_type

module ArchType (Entry: EntryType) = struct
  exception ArchTypeError of string
  let arch_type_error msg = raise (ArchTypeError ("[Arch Type Error] " ^ msg))

  type entry_t = Entry.t

  module RegType = RegType (Entry)
  module MemType = MemType (Entry)

  (* type t = {
    reg_type: 
  } *)


end
