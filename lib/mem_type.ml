open Isa
open Single_exp
open Type_exp


module MemType = struct
  exception MemTypeError of string
  let mem_type_error msg = raise (MemTypeError ("[Mem TYpe Error] " ^ msg))

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

end
