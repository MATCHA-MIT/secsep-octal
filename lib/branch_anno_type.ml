open Sexplib

module type BranchAnnoType = sig
  
  type t
  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t

end