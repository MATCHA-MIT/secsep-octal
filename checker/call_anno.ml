open Basic_type
open Mem_anno
open Reg_type
open Mem_type

module CallAnno = struct
  exception CallAnnoError of string

  let call_anno_error msg = raise (CallAnnoError ("[Call Anno Error] " ^ msg))

  type t = {
    pr_reg: RegType.t;
    ctx_map: BasicType.map_t;
    mem_map: MemAnno.slot_t MemType.mem_content;
  }
  [@@deriving sexp]

  let to_string (anno) : string =
    Sexplib.Sexp.to_string_hum (sexp_of_t anno)

  let to_ocaml_string (_) : string = call_anno_error "Not implemented"

  let get_empty () : t = {
    pr_reg = [];
    ctx_map = ([], []);
    mem_map = [];
  }

end
