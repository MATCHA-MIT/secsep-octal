open Type.Isa
open Mem_anno
open Reg_type
open Flag_type
open Mem_type
open Z3

module ArchType = struct
  exception ArchTypeError of string

  let arch_type_error msg = raise (ArchTypeError ("[Arch Type Error] " ^ msg))

  module Isa = Isa (MemAnno)

  type t = {
    label: Isa.label;
    pc: int;
    dead_pc: int;
    reg_type: RegType.t;
    flag_type: FlagType.t;
    mem_type: MemType.t;
  }

end
