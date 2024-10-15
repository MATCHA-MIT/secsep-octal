open Single_exp
open Smt_emitter
open Sexplib

module type EntryTypeBasic = sig
  type t
  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t

  val to_string : t -> string
  val to_ocaml_string: t -> string
  val get_single_exp: t -> SingleExp.t (* Used for get address, must be 8-byte dep type *)
  val to_smt_expr: SmtEmitter.t -> t -> SmtEmitter.exp_t
end
