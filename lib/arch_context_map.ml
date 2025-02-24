open Single_entry_type
open Branch_anno
open Arch_type
open Single_inverse_map
open Set_sexp

module ArchContextMap = struct
  exception ArchContextMapError of string

  let arch_context_map_error msg = raise (ArchContextMapError ("[Arch Context Map Error] " ^ msg))

  type map_t = SingleInverseMap.t
  [@@deriving sexp]

  module IntMap = IntMapSexp (
    struct
      type t = map_t * map_t
      [@@deriving sexp]
    end
  )

  type t = IntMap.t
  [@@deriving sexp]

  module ArchType = ArchType (SingleEntryType)

  let init (input_var_set: IntSet.t) (block_subtype_list: ArchType.block_subtype_t list) : t =
    List.fold_left (
      fun (acc: t) (block_subtype: ArchType.block_subtype_t) ->
        let target_block, branch_block_list = block_subtype in
        List.fold_left (
          fun (acc: t) (branch_block: ArchType.t) ->
            let context_map = 
              BranchAnno.get_branch_anno 
                branch_block.reg_type branch_block.mem_type branch_block.local_var_map
                target_block.reg_type target_block.mem_type target_block.useful_var
            in
            let inverse_map = SingleInverseMap.get_inverse_context_map input_var_set context_map in
            IntMap.add branch_block.pc (context_map, inverse_map) acc
        ) acc branch_block_list
    ) IntMap.empty block_subtype_list

  let get_context_map (map: t) (br_pc: int) : map_t =
    match IntMap.find_opt br_pc map with
    | Some (ctx_map, _) -> ctx_map
    | None ->
      Printf.printf "get_context_map lookup br_pc %d\n%s\n" 
        br_pc
        (Sexplib.Sexp.to_string_hum (sexp_of_t map));
      arch_context_map_error "get_context_map cannot find br_pc"

  let get_reverse_map (map: t) (br_pc: int) : map_t =
    match IntMap.find_opt br_pc map with
    | Some (_, reverse_map) -> reverse_map
    | None ->
      Printf.printf "get_reverse_map lookup br_pc %d\n%s\n" 
        br_pc
        (Sexplib.Sexp.to_string_hum (sexp_of_t map));
      arch_context_map_error "get_reverse_map cannot find br_pc"

end
