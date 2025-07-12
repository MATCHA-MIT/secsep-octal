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

module Int64PairSet = struct
include Set.Make(
  struct
    let compare = compare
    type t = int64 * int64
  end
)
  type entry_t = int64 * int64
  [@@deriving sexp]

  let t_of_sexp (s_exp: Sexp.t) : t = 
    of_list (list_of_sexp entry_t_of_sexp s_exp)

  let sexp_of_t (s: t) : Sexp.t = 
    sexp_of_list sexp_of_entry_t (elements s)

end

module StrMap = Map.Make(String)
module IntMap = Map.Make(Int)

module type MapEntryType = sig
  type t
  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
end

module IntMapSexp (Entry: MapEntryType) = struct
include IntMap

  type val_t = Entry.t
  [@@deriving sexp]

  type key_val_t = int * val_t
  [@@deriving sexp]

  type t = val_t IntMap.t

  let t_of_sexp (sexp: Sexp.t) : t =
    list_of_sexp key_val_t_of_sexp sexp |> IntMap.of_list

  let sexp_of_t (map: t) : Sexp.t =
    sexp_of_list sexp_of_key_val_t (IntMap.to_list map)

end

module StrMapSexp (Entry: MapEntryType) = struct
include StrMap

  type val_t = Entry.t
  [@@deriving sexp]

  type key_val_t = string * val_t
  [@@deriving sexp]

  type t = val_t StrMap.t

  let t_of_sexp (sexp: Sexp.t) : t =
    list_of_sexp key_val_t_of_sexp sexp |> StrMap.of_list

  let sexp_of_t (map: t) : Sexp.t =
    sexp_of_list sexp_of_key_val_t (StrMap.to_list map)

end
