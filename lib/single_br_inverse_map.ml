open Isa_basic
open Single_entry_type
open Arch_type
open Set_sexp


module SingleBrInverseMap = struct
  exception SingleBrInverseMap of string
  let single_br_inverse_map_error msg = raise (SingleBrInverseMap ("[Single Br Inverse Map Error] " ^ msg))

  module ArchType = ArchType (SingleEntryType)

  module BrSingleMapMap = IntMapSexp (
    struct
      type t = SingleEntryType.local_var_map_t
      [@@deriving sexp]
    end
  )

  type t = BrSingleMapMap.t
  [@@deriving sexp]

  module VarPc = struct
    type t = IsaBasic.imm_var_id * int
    let compare = compare 
  end

  module VarPcSet = Set.Make(VarPc)
  module VarPcMap = Map.Make(VarPc)

  type target_br_map_t = VarPcSet.t IntMap.t
  type br_target_map_t = IntSet.t VarPcMap.t

  let get_full_map
      (block_subtype: ArchType.block_subtype_t) :
      target_br_map_t * br_target_map_t =
    IntMap.empty, VarPcMap.empty

  let init (block_subtype_list: ArchType.block_subtype_t list) : t =
    let helper
        (acc: t)
        (block_subtype: ArchType.block_subtype_t) : t =
      BrSingleMapMap.empty
    in
    List.fold_left helper BrSingleMapMap.empty block_subtype_list

end
