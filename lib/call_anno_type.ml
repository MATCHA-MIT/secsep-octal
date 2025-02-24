open Sexplib

module type CallAnnoType = sig
  
  type t
  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
  val to_string : t -> string
  val to_ocaml_string: t -> string

end
