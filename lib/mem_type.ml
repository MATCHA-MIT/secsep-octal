open Isa
open Single_exp
open Type_exp


module MemType = struct
  exception MemTypeError of string
  let mem_type_error msg = raise (MemTypeError ("[Mem Type Error] " ^ msg))

  module MemKeySet = Set.Make(Int)

  type t = {
    ptr_list: MemKeySet.t;
    mem_type: (Isa.imm_var_id * ((SingleExp.t * SingleExp.t * TypeExp.t) list)) list
  }

  let rec filter_single_var (addr: SingleExp.t) : MemKeySet.t =
    match addr with
    | SingleVar x -> MemKeySet.singleton x
    | SingleBExp (SingleExp.SingleAdd, l, r) ->
      let left_ptr = filter_single_var l in
      let right_ptr = filter_single_var r in
      MemKeySet.union left_ptr right_ptr
    | _ -> MemKeySet.empty

  let filter_type_single_var (e: TypeExp.t) : MemKeySet.t =
    match e with
    | TypeSingle s -> filter_single_var s
    | TypeRange (l, _, r, _, _) ->
      let left_ptr = filter_single_var l in
      let right_ptr = filter_single_var r in
      MemKeySet.inter left_ptr right_ptr
    | _ -> MemKeySet.empty

  let find_base (e: TypeExp.t) (ptr_list: MemKeySet.t) : Isa.imm_var_id option =
    let p_list = filter_type_single_var e in
    match MemKeySet.to_list (MemKeySet.inter p_list ptr_list) with
    | [] -> None
    | hd :: [] -> Some hd
    | _ -> mem_type_error "find_base find more than one base"

  let get_idx_range (addr_exp: TypeExp.t * int64) (base: Isa.imm_var_id): SingleExp.t * SingleExp.t =
    let addr, size = addr_exp in
    match addr with
    | TypeSingle s ->
      (SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleSub, s, SingleExp.SingleVar base)),
      SingleExp.eval (SingleExp.SingleBExp (
        SingleExp.SingleAdd, 
        SingleExp.SingleBExp (SingleExp.SingleSub, s, SingleExp.SingleVar base), 
        SingleExp.SingleConst size)))
    | TypeRange (s1, true, s2, true, _) ->
      (SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleSub, s1, SingleExp.SingleVar base)),
      SingleExp.eval (SingleExp.SingleBExp (
        SingleExp.SingleAdd,
        SingleExp.SingleBExp (SingleExp.SingleSub, s2, SingleExp.SingleVar base),
        SingleExp.SingleConst size)))
    | _ -> 
      mem_type_error ("get_idx_range not implement for " ^ (TypeExp.string_of_type_exp addr))

  let subset (e1: SingleExp.t * SingleExp.t) (e2: SingleExp.t * SingleExp.t) : bool =
    let l1, r1 = e1 in
    let l2, r2 = e2 in
    SingleExp.must_ge l1 l2 && SingleExp.must_ge r2 r1

  let get_mem_type (mem: t) (addr_exp: TypeExp.t * int64) : (TypeExp.t, TypeExp.t * int64) Either.t =
    let addr, _ = addr_exp in
    let base_opt = find_base addr mem.ptr_list in
    match base_opt with
    | None -> Right addr_exp
    | Some base ->
      let _, offset_list = List.find (fun (i, _) -> i = base) mem.mem_type in
      let addr_range = get_idx_range addr_exp base in
      let find_entry = List.find_map (
        fun (l, r, mem_type) ->
          if subset addr_range (l, r) then Some mem_type
          else None
      ) offset_list in
      begin match find_entry with
      | None -> Right addr_exp
      | Some mem_type -> Left mem_type
      end

  let set_mem_type (mem: t) (addr_exp: TypeExp.t * int64) (new_type: TypeExp.t) : (t, TypeExp.t * int64) Either.t =
    let addr, _ = addr_exp in
    let base_opt = find_base addr mem.ptr_list in
    match base_opt with
    | None -> Right addr_exp
    | Some base ->
      let _, offset_list = List.find (fun (i, _) -> i = base) mem.mem_type in
      let addr_range = get_idx_range addr_exp base in
      let find_entry = List.find_index (
        fun (l, r, _) ->
          if subset addr_range (l, r) then true
          else false
      ) offset_list in
      begin match find_entry with
      | None -> Right addr_exp
      | Some idx -> Left { mem with mem_type = 
          List.map (
            fun (k, o_list) ->
              if k = base then (
                k,
                List.mapi (
                  fun i (l, r, old_type) ->
                    if i = idx then (l, r, new_type) else (l, r, old_type)
                ) o_list
              )
              else (k, o_list)
          ) mem.mem_type
        }
      end

  let init_mem_type : t =
    {
      ptr_list = MemKeySet.empty;
      mem_type = [];
    }

  let format_convert_helper = List.map (fun (l, r, _) -> (l, r, false))

  let update_offset_one_ptr
      (old_mem_type: (SingleExp.t * SingleExp.t * TypeExp.t) list)
      (mem_access_list: (SingleExp.t * SingleExp.t) list) :
      (SingleExp.t * SingleExp.t * bool) list =
    let rec helper
        (acc: (SingleExp.t * SingleExp.t * bool) list)
        (offset: SingleExp.t * SingleExp.t) :
        (SingleExp.t * SingleExp.t * bool) list =
      let left, right = offset in
      match acc with
      | [] -> [ (left, right, true) ]
      | (hd_left, hd_right, hd_updated) :: tl ->
        if SingleExp.must_ge hd_left right then (left, right, true) :: acc
        else if SingleExp.must_ge left hd_right then (hd_left, hd_right, hd_updated) :: (helper tl offset)
        else
          begin match SingleExp.get_less left hd_left, SingleExp.get_greater right hd_right with
          | Some l, Some r -> helper tl (l, r)
          | _ -> mem_type_error "update_offset cannot merge address offset range" 
          end
    in
    let acc = format_convert_helper old_mem_type in
    List.fold_left helper acc mem_access_list

  let rec update_offset_all_ptr
      (old_mem_type: (Isa.imm_var_id * (SingleExp.t * SingleExp.t * TypeExp.t) list) list)
      (mem_access_list: (Isa.imm_var_id * (SingleExp.t * SingleExp.t) list) list) :
      (Isa.imm_var_id * (SingleExp.t * SingleExp.t * bool) list) list =
    match old_mem_type, mem_access_list with
    | (id1, mem1) :: tl1, (id2, mem2) :: tl2 ->
      if id1 = id2 then
        (id1, update_offset_one_ptr mem1 mem2) :: (update_offset_all_ptr tl1 tl2)
      else if id1 < id2 then
        (id1, format_convert_helper mem1) :: (update_offset_all_ptr tl1 mem_access_list)
      else (* id2 < id1 *)
        (id2, update_offset_one_ptr [] mem2) :: (update_offset_all_ptr old_mem_type tl2)
    | _ :: _, [] -> 
      List.map (fun (ptr, offset_list) -> (ptr, format_convert_helper offset_list)) old_mem_type
    | [], (id2, mem2) :: tl2 ->
      (id2, update_offset_one_ptr [] mem2) :: (update_offset_all_ptr [] tl2)
    | [], [] -> []

  let rec find_drop_head
      (old_mem_type: (SingleExp.t * SingleExp.t * TypeExp.t) list)
      (drop_set: TypeExp.TypeVarSet.t)
      (left: SingleExp.t) (right: SingleExp.t) :
      TypeExp.t * ((SingleExp.t * SingleExp.t * TypeExp.t) list) * TypeExp.TypeVarSet.t =
    match old_mem_type with
    | [] -> mem_type_error "find_drop_head type target not found"
    | (l, r, mem_t) :: tl ->
      if SingleExp.cmp left l = 0 && SingleExp.cmp right r = 0 then 
        (mem_t, tl, drop_set)
      else
        begin match mem_t with
        | TypeVar x ->
          find_drop_head tl (TypeExp.TypeVarSet.add x drop_set) left right
        | TypeSingle _ -> find_drop_head tl drop_set left right
        | _ -> mem_type_error "find_drop_head type being processed is not TypeVar or TypeSingle"
        end

  let drop_all
      (old_mem_type: (SingleExp.t * SingleExp.t * TypeExp.t) list)
      (drop_set: TypeExp.TypeVarSet.t) :
      TypeExp.TypeVarSet.t =
    let helper (acc: TypeExp.TypeVarSet.t) (entry: SingleExp.t * SingleExp.t * TypeExp.t) : TypeExp.TypeVarSet.t =
      let _, _, mem_t = entry in
      match mem_t with
      | TypeVar x -> TypeExp.TypeVarSet.add x acc
      | TypeSingle _ -> acc
      | _ -> mem_type_error "drop_all type being processed is not TypeVar or TypeSingle"
    in
    List.fold_left helper drop_set old_mem_type

  let rec update_type_one_ptr
      (old_mem_type: (SingleExp.t * SingleExp.t * TypeExp.t) list)
      (start_var_idx: (TypeExp.type_var_id, Isa.imm_var_id) Either.t)
      (new_var_set: TypeExp.TypeVarSet.t) (drop_var_set: TypeExp.TypeVarSet.t)
      (update_list: (SingleExp.t * SingleExp.t * bool) list) :
      ((SingleExp.t * SingleExp.t * TypeExp.t) list) * ((TypeExp.type_var_id, Isa.imm_var_id) Either.t) * TypeExp.TypeVarSet.t * TypeExp.TypeVarSet.t =
    match update_list with
    | [] -> 
      let d_set = drop_all old_mem_type drop_var_set in
      ([], start_var_idx, new_var_set, d_set)
    | (left, right, false) :: tl ->
      let hd_type, d_mem_type, d_set = find_drop_head old_mem_type drop_var_set left right in
      let d_mem_type, update_var_idx, n_set, d_set = update_type_one_ptr d_mem_type start_var_idx new_var_set d_set tl in
      ((left, right, hd_type) :: d_mem_type, update_var_idx, n_set, d_set)
    | (left, right, true) :: tl ->
      begin match start_var_idx with
      | Left type_var_idx ->
        let d_mem_type, update_var_idx, n_set, d_set = update_type_one_ptr old_mem_type (Left (type_var_idx + 1)) new_var_set drop_var_set tl in
        ((left, right, TypeVar type_var_idx) :: d_mem_type, update_var_idx, TypeExp.TypeVarSet.add type_var_idx n_set, d_set)
      | Right single_var_idx ->
        let d_mem_type, update_var_idx, n_set, d_set = update_type_one_ptr old_mem_type (Right (single_var_idx + 1)) new_var_set drop_var_set tl in
        ((left, right, TypeSingle (SingleVar single_var_idx)) :: d_mem_type, update_var_idx, n_set, d_set)
      end
    
  let rec update_type_all_ptr
      (old_t: t)
      (start_var_idx: (TypeExp.type_var_id, Isa.imm_var_id) Either.t)
      (new_var_set: TypeExp.TypeVarSet.t) (drop_var_set: TypeExp.TypeVarSet.t)
      (update_list: (Isa.imm_var_id * (SingleExp.t * SingleExp.t * bool) list) list) :
      t * ((TypeExp.type_var_id, Isa.imm_var_id) Either.t) * TypeExp.TypeVarSet.t * TypeExp.TypeVarSet.t =
    match old_t.mem_type, update_list with
    | (id1, mem1) :: tl1, (id2, mem2) :: tl2 ->
      if id1 = id2 then
        let mem_type, var_idx, n_var_set, d_var_set = update_type_one_ptr mem1 start_var_idx new_var_set drop_var_set mem2 in
        let result, var_idx2, n_var_set2, d_var_set2 = update_type_all_ptr { old_t with mem_type = tl1 } var_idx n_var_set d_var_set tl2 in
        ({ result with mem_type = (id1, mem_type) :: result.mem_type }, var_idx2, n_var_set2, d_var_set2)
        (* (id1, update_type_one_ptr mem1 mem2) :: (update_type_all_ptr tl1 tl2) *)
      else if id1 < id2 then
        let result, var_idx, n_var_set, d_var_set = update_type_all_ptr { old_t with mem_type = tl1 } start_var_idx new_var_set drop_var_set update_list in
        ({ result with mem_type = (id1, mem1) :: result.mem_type }, var_idx, n_var_set, d_var_set)
        (* (id1, mem1) :: (update_type_all_ptr tl1 mem_access_list) *)
      else (* id2 < id1 *)
        let mem_type, var_idx, n_var_set, d_var_set = update_type_one_ptr [] start_var_idx new_var_set drop_var_set mem2 in
        let result, var_idx2, n_var_set2, d_var_set2 = update_type_all_ptr old_t var_idx n_var_set d_var_set tl2 in
        ({ result with mem_type = (id2, mem_type) :: result.mem_type }, var_idx2, n_var_set2, d_var_set2)
        (* (id2, update_type_one_ptr [] mem2) :: (update_type_all_ptr old_mem_type tl2) *)
    | [], (id2, mem2) :: tl2 ->
      let mem_type, var_idx, n_var_set, d_var_set = update_type_one_ptr [] start_var_idx new_var_set drop_var_set mem2 in
      let result, var_idx2, n_var_set2, d_var_set2 = update_type_all_ptr { old_t with mem_type = [] } var_idx n_var_set d_var_set tl2 in
      ({ result with mem_type = (id2, mem_type) :: result.mem_type }, var_idx2, n_var_set2, d_var_set2)
      (* (id2, update_type_one_ptr [] mem2) :: (update_type_all_ptr [] tl2) *)
    | _, [] -> (old_t, start_var_idx, new_var_set, drop_var_set)

end
