open Basic_type

module BranchAnno = struct
  exception BranchAnnoError of string

  type t = BasicType.map_t
  [@@deriving sexp]

end
