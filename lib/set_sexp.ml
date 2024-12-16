open Sexplib.Std
open Sexplib

module IntSet = struct
include Set.Make(Int)
  let t_of_sexp (s_exp: Sexp.t) : t = 
    of_list (list_of_sexp int_of_sexp s_exp)

  let sexp_of_t (s: t) : Sexp.t = 
    sexp_of_list sexp_of_int (elements s)
  end

module StringSet = struct
include Set.Make(String)
  let t_of_sexp (s_exp: Sexp.t) : t = 
    of_list (list_of_sexp string_of_sexp s_exp)

  let sexp_of_t (s: t) : Sexp.t = 
    sexp_of_list sexp_of_string (elements s)
  end
