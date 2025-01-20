open Type.Isa_basic
open Type.Mem_offset_new
open Type.Taint_exp
open Basic_type
open Sexplib.Std

module MemAnno = struct
  exception MemAnnoError of string

  let mem_anno_error msg = raise (MemAnnoError ("[Mem Anno Error]" ^ msg))

  (* <TODO> I think checker does not need is_full. Double check!!! *)
  type slot_t = int * int * bool * int (* ptr, slot idx, is full, num slots*)
  [@@deriving sexp]

  type taint_t = TaintType.t
  [@@deriving sexp]

  type t = slot_t * taint_t
  [@@deriving sexp]

  (* Dirty code to be compatible with interface in infer *)
  let make_empty () : t = mem_anno_error "make_empty invalid in checker"
  let update_taint (_: t) (_: TaintExp.t) : t = mem_anno_error "update_taint invalid in checker"
  let get_slot (_: t) : (IsaBasic.imm_var_id * MemOffset.t * bool * int) option = mem_anno_error "get_slot invalid in checker"
  let get_taint (_: t) : TaintExp.t option  = mem_anno_error "get_taint invalid in checker"
  let to_string (_: t) : string = mem_anno_error "to_string invalid in checker"
  let to_ocaml_string (_: t) : string = mem_anno_error "to_ocaml_string invalid in checker"

end
