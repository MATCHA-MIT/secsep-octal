open Basic_type

module BranchAnno = struct
  exception BranchAnnoError of string

  let branch_anno_error msg = raise (BranchAnnoError ("[Call Anno Error] " ^ msg))

  type t = BasicType.map_t
  [@@deriving sexp]

  let to_string (anno) : string =
    Sexplib.Sexp.to_string_hum (sexp_of_t anno)

  let to_ocaml_string (_) : string = branch_anno_error "Not implemented"

  let get_empty () : t = ([], [])

  let is_taint_map_empty (anno: t) : bool =
    snd anno = []

end
