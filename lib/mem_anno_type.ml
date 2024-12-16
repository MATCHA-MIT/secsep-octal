open Isa_basic
open Mem_offset_new
open Taint_exp
open Sexplib

module type MemAnnoType = sig

  type t
  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t

  val make_empty : unit -> t
  val update_taint : t -> TaintExp.t -> t
  val get_slot : t -> (IsaBasic.imm_var_id * MemOffset.t * bool * int) option
  val get_taint : t -> TaintExp.t option
  val to_string : t -> string
  val to_ocaml_string: t -> string

end
