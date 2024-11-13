open Isa_basic
open Mem_type_new
open Sexplib.Std

module TaintApi = struct
  type api_t = IsaBasic.label * ((bool option) list) * ((bool option) MemTypeBasic.mem_content)
  [@@deriving sexp]

  type t = api_t list
  [@@deriving sexp]

  let api_from_file (filename: string) : t =
    let open Sexplib in
    let channel = open_in filename in
    let s_exp = Sexp.input_sexp channel in
    t_of_sexp s_exp
end
