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

  let interface_list_to_file (filename: string) (fi_list: t list) : unit =
    let open Sexplib in
    let channel = open_out filename in
    Sexp.output_hum channel (Std.sexp_of_list sexp_of_t fi_list)

end
