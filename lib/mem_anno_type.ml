open Isa_basic
open Mem_offset_new
open Taint_exp

module type MemAnnoType = sig

  type t

  val make_empty : unit -> t
  val update_taint : t -> TaintExp.t -> t
  val get_slot : t -> (IsaBasic.imm_var_id * MemOffset.t * bool) option
  val get_taint : t -> TaintExp.t option
  val to_string : t -> string
  val to_ocaml_string: t -> string

end
