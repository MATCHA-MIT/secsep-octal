open Basic_type

module BranchAnno = struct
  exception BranchAnnoError of string

  let branch_anno_error msg = raise (BranchAnnoError ("[Call Anno Error] " ^ msg))

  type t = BasicType.map_t
  [@@deriving sexp]

  let to_string (anno) : string =
    Sexplib.Sexp.to_string_hum (sexp_of_t anno)

  let to_ocaml_string (_) : string = branch_anno_error "Not implemented"

end
