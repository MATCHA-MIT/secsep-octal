open Basic_type
open Type.Set_sexp

module BranchAnno = struct
  exception BranchAnnoError of string

  let branch_anno_error msg = raise (BranchAnnoError ("[Call Anno Error] " ^ msg))

  type t = BasicType.map_t
  [@@deriving sexp]

  let to_string (anno) : string =
    Sexplib.Sexp.to_string_hum (sexp_of_t anno)

  let to_ocaml_string (_) : string = branch_anno_error "Not implemented"

  let get_empty () : t = ([], [])

  let check_br_anno
      (input_var_set: IntSet.t) (anno: t) : bool =
    let dep_map, taint_map = anno in
    let no_input_var = List.split dep_map |> fst |> IntSet.of_list |> IntSet.inter input_var_set |> IntSet.is_empty in
    no_input_var && DepType.check_map_no_top dep_map && taint_map = []

end
