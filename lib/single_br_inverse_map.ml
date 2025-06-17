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

  type target_br_map_t = int IntMap.t
  type br_target_map_t = IntSet.t VarPcMap.t

  let get_full_map
      (block_subtype: ArchType.block_subtype_t) :
      target_br_map_t * br_target_map_t =
    let update_target_br_map
        (orig_br_pc_count: int option) : int option =
      match orig_br_pc_count with
      | None -> Some 1
      | Some s -> Some (s + 1)
    in
    let update_br_target_map
        (target_v: IsaBasic.imm_var_id)
        (orig_target_v_set: IntSet.t option) : IntSet.t option =
      match orig_target_v_set with
      | None -> IntSet.singleton target_v |> Option.some
      | Some s -> IntSet.add target_v s |> Option.some
    in
    let helper_inner
        (target_useful_var: IntSet.t)
        (br_pc: int)
        (acc: target_br_map_t * br_target_map_t) (target_e: SingleEntryType.t) (br_e: SingleEntryType.t) : 'a =
      match target_e, br_e with
      | SingleVar target_v, SingleVar br_v ->
        if IntSet.mem target_v target_useful_var then
          let target_br_map, br_target_map = acc in
          IntMap.update target_v update_target_br_map target_br_map,
          VarPcMap.update (br_v, br_pc) (update_br_target_map target_v) br_target_map
        else acc
        | _ -> acc
    in
    let helper_outer
        (target_block: ArchType.t)
        (acc: target_br_map_t * br_target_map_t) (br_block: ArchType.t) : 
        target_br_map_t * br_target_map_t =
      let helper = helper_inner target_block.useful_var br_block.pc in
      let acc = List.fold_left2 helper acc target_block.reg_type br_block.reg_type in
      ArchType.MemType.fold_left2 helper acc target_block.mem_type br_block.mem_type
    in
    let target_block, br_block_list = block_subtype in
    List.fold_left (helper_outer target_block) (IntMap.empty, VarPcMap.empty) br_block_list

  let init (block_subtype_list: ArchType.block_subtype_t list) : t =
    let helper
        (acc: t)
        (block_subtype: ArchType.block_subtype_t) : t =
      let target_br_map, br_target_map = get_full_map block_subtype in
      let find_target_br_count (v: IsaBasic.imm_var_id) : int =
        match IntMap.find_opt v target_br_map with
        | None -> 0
        | Some count -> count
      in
      let pick_target_v
          (target_v_set: IntSet.t) : int =
        match IntSet.to_list target_v_set with
        | [] -> single_br_inverse_map_error "init: get empty target v set"
        | hd :: [] -> hd
        | target_v_list ->
          List.sort (
            fun x1 x2 -> 
              - (compare (find_target_br_count x1) (find_target_br_count x2))
          ) target_v_list |> List.hd
      in
      let br_target_list = VarPcMap.to_list br_target_map in
      List.fold_left (
        fun (acc: t) ((br_v, br_pc), target_v_set) ->
          let entry = (br_v, SingleEntryType.SingleVar (pick_target_v target_v_set)) in
          BrSingleMapMap.update br_pc (
            fun (x: SingleEntryType.local_var_map_t option) ->
              match x with
              | None -> Some [entry]
              | Some orig_map -> Some (entry :: orig_map)
          ) acc
      ) acc br_target_list
    in
    List.fold_left helper BrSingleMapMap.empty block_subtype_list

end
