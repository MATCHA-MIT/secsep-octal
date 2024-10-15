open Isa_basic
open Single_type_infer
open Sexplib.Std

type t = (IsaBasic.label * SingleTypeInfer.ArchType.MemType.t) list
[@@deriving sexp]

let parse (source: string) : t =
  let open Sexplib in
  t_of_sexp (Sexp.of_string source)
