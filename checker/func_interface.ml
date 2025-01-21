open Type.Isa_basic
open Arch_type_basic

module FuncInterface = struct
  exception FuncInterfaceError of string

  let func_interface_error msg = raise (FuncInterfaceError ("[Func Interface Error] " ^ msg))

  type t = {
    func_name: IsaBasic.label;
    in_type: ArchTypeBasic.t;
    out_type: ArchTypeBasic.t;
    (* <TODO>: base_info if needed, if added, must also check its correctness *)
  }
  [@@deriving sexp]

end
