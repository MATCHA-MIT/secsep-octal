open Isa
open Single_exp
open Type_exp
open Type_full_exp
open Mem_offset
open Pretty_print

module type MemEntrytype = sig
  type t
  (* val default_val : t *)
  val partial_read_val: t -> t
  val partial_write_val: t -> t -> t
  val next_var : t -> t
  val to_string : t -> string
end

module MemKeySet = Set.Make(Int)

(* module type MemType = sig
  exception MemTypeError of string
  val mem_type_error : string -> 'a
  type entry_t
  type t
  val get_mem_type_with_key : t -> Isa.imm_var_id * MemOffset.t -> bool -> entry_t
  val set_mem_type_with_key : t -> Isa.imm_var_id * MemOffset.t -> bool -> entry_t -> t
  val get_mem_type_with_addr : t -> TypeExp.t * int64 -> (entry_t * MemOffset.ConstraintSet.t, TypeExp.t * int64) Either.t
  val set_mem_type_with_addr : t -> TypeExp.t * int64 -> entry_t -> (t * MemOffset.ConstraintSet.t, TypeExp.t * int64) Either.t
  val init_mem_type : t
  val pp_ptr_list : int -> Isa.imm_var_id list -> unit
  val pp_ptr_set : int-> MemKeySet.t -> unit
  val pp_mem_key : int -> (Isa.imm_var_id * (MemOffset.t list)) list -> unit
  val pp_base_range : int -> (Isa.imm_var_id * MemOffset.t) list -> unit
  val pp_mem_type : int -> (Isa.imm_var_id * ((MemOffset.t * TypeExp.t) list)) list -> unit
end *)

module MemType (Entry: MemEntrytype) = struct
  exception MemTypeError of string
  let mem_type_error msg = raise (MemTypeError ("[Mem Type Error] " ^ msg))

  type entry_t = Entry.t
  type t = {
    ptr_list: MemKeySet.t;
    mem_type: (Isa.imm_var_id * ((MemOffset.t * entry_t) list)) list
  }

  let get_mem_type_with_key (mem: t) (mem_key: Isa.imm_var_id * MemOffset.t) (full_entry: bool) : entry_t =
    let ptr, offset = mem_key in
    let _, ptr_mem = List.find (fun (x, _) -> x = ptr) mem.mem_type in
    let _, mem_type = List.find (fun (x, _) -> MemOffset.cmp offset x = 0) ptr_mem in
    if full_entry then mem_type else Entry.partial_read_val mem_type

  let set_mem_type_with_key 
      (mem: t) (mem_key: Isa.imm_var_id * MemOffset.t) (full_entry: bool) (new_type: entry_t) : t =
    let ptr, offset = mem_key in
    let helper (acc: bool) (entry: MemOffset.t * entry_t) : bool * (MemOffset.t * entry_t) =
      let entry_offset, old_type = entry in
      match acc, MemOffset.cmp entry_offset offset = 0 with
      | true, true -> mem_type_error "set_mem_type_with_key found more than one match"
      | true, false -> (true, entry)
      | false, true -> 
        if full_entry then (true, (offset, new_type)) else (true, (offset, Entry.partial_write_val old_type new_type))
      | false, false -> (false, entry)
    in
    let helper2 (acc: bool) (entry: Isa.imm_var_id * ((MemOffset.t * entry_t) list)) :
        bool * (Isa.imm_var_id * ((MemOffset.t * entry_t) list)) =
      let entry_ptr, entry_mem = entry in
      if ptr == entry_ptr then
        let found, new_entry_mem = List.fold_left_map helper false entry_mem in
        if found then (true, (entry_ptr, new_entry_mem))
        else mem_type_error "set_mem_type_with_key offset not found"
      else (acc, entry)
    in
    let found, new_mem_type = List.fold_left_map helper2 false mem.mem_type in
    if found then {mem with mem_type = new_mem_type}
    else mem_type_error "set_mem_type_with_key ptr not found"

  let pp_ptr_list (lvl: int) (ptr_list: Isa.imm_var_id list) =
    PP.print_lvl lvl "Ptr list: ";
    List.iter (
      fun x -> Printf.printf "%d " x
    ) ptr_list;
    Printf.printf "\n"

  let pp_ptr_set (lvl: int) (ptr_set: MemKeySet.t) =
    pp_ptr_list lvl (MemKeySet.elements ptr_set)

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

  let get_idx_range (addr_exp: TypeExp.t * int64) (base: Isa.imm_var_id): MemOffset.t * bool =
    let addr, size = addr_exp in
    match addr with
    | TypeSingle s ->
      ((SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleSub, s, SingleExp.SingleVar base)),
      SingleExp.eval (SingleExp.SingleBExp (
        SingleExp.SingleAdd, 
        SingleExp.SingleBExp (SingleExp.SingleSub, s, SingleExp.SingleVar base), 
        SingleExp.SingleConst size))),
      true)
    | TypeRange (s1, true, s2, true, _) ->
      ((SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleSub, s1, SingleExp.SingleVar base)),
      SingleExp.eval (SingleExp.SingleBExp (
        SingleExp.SingleAdd,
        SingleExp.SingleBExp (SingleExp.SingleSub, s2, SingleExp.SingleVar base),
        SingleExp.SingleConst size))),
      false)
    | _ -> 
      mem_type_error ("get_idx_range not implement for " ^ (TypeExp.to_string addr))

  let get_mem_type_with_addr (mem: t) (addr_exp: TypeExp.t * int64) : (entry_t * MemOffset.ConstraintSet.t, TypeExp.t * int64) Either.t =
    let addr, _ = addr_exp in
    let base_opt = find_base addr mem.ptr_list in
    match base_opt with
    | None -> Right addr_exp
    | Some base ->
      let _, offset_list = List.find (fun (i, _) -> i = base) mem.mem_type in
      let addr_range, track_value = get_idx_range addr_exp base in
      let addr_constraint = MemOffset.check_offset addr_range in
      let find_entry = List.find_map (
        fun (offset, mem_type) ->
          if MemOffset.equal addr_range offset then 
            begin if track_value then 
                Some (mem_type, MemOffset.ConstraintSet.empty)
            else
                Some (Entry.partial_read_val mem_type, MemOffset.ConstraintSet.empty)
            end
          else
            let sub, sub_constraint = MemOffset.subset addr_range offset in
            if sub then Some (Entry.partial_read_val mem_type, sub_constraint)
            else None
      ) offset_list in
      begin match find_entry with
      | None -> Right addr_exp
      | Some (mem_type, match_constraint) -> Left (mem_type, MemOffset.ConstraintSet.union addr_constraint match_constraint)
      end

  let set_mem_type_with_addr 
      (mem: t) (addr_exp: TypeExp.t * int64) (new_type: entry_t) : 
      (t * MemOffset.ConstraintSet.t, TypeExp.t * int64) Either.t =
    let addr, _ = addr_exp in
    let base_opt = find_base addr mem.ptr_list in
    match base_opt with
    | None -> Right addr_exp
    | Some base ->
      (* let _, offset_list = List.find (fun (i, _) -> i = base) mem.mem_type in *)
      let addr_range, track_value = get_idx_range addr_exp base in
      let addr_constraint = MemOffset.check_offset addr_range in
      let helper0
          (acc: MemOffset.ConstraintSet.t option) 
          (entry: MemOffset.t * entry_t) : 
          (MemOffset.ConstraintSet.t option) * (MemOffset.t * entry_t) =
        let offset, old_mem_type = entry in
        match acc with
        | Some _ -> (acc, entry)
        | None ->
          if MemOffset.equal addr_range offset then
            begin if track_value then
              (Some MemOffset.ConstraintSet.empty, (offset, new_type))
            else
              (Some MemOffset.ConstraintSet.empty, (offset, Entry.partial_write_val old_mem_type new_type))
            end
          else begin
            let subset, subset_constraint = MemOffset.subset addr_range offset in
            if subset then
              (Some subset_constraint, (offset, Entry.partial_write_val old_mem_type new_type))
            else
              (None, entry)
          end
      in
      let helper
          (acc: MemOffset.ConstraintSet.t option)
          (entry: Isa.imm_var_id * ((MemOffset.t * entry_t) list)) :
          (MemOffset.ConstraintSet.t option) * (Isa.imm_var_id * ((MemOffset.t * entry_t) list)) =
        let k, o_list = entry in
        match acc with
        | Some acc -> (Some acc, entry)
        | None ->
          if k = base then
            let constraint_list, new_o_list = List.fold_left_map helper0 None o_list in
            (
              constraint_list,
              (k, new_o_list)
            )
          else (None, entry)
      in
      let constraint_list_opt, new_mem_type = List.fold_left_map helper None mem.mem_type in
      match constraint_list_opt with
      | None -> Right addr_exp
      | Some constraint_list ->
        Left (
          {mem with mem_type = new_mem_type},
          MemOffset.ConstraintSet.union addr_constraint constraint_list
        )

  let init_mem_type : t =
    {
      ptr_list = MemKeySet.empty;
      mem_type = [];
    }

  let pp_mem_key (lvl: int) (mem_key_list: (Isa.imm_var_id * (MemOffset.t list)) list) =
    PP.print_lvl lvl "Mem key list:\n";
    List.iter (
      fun (id, key_list) ->
        PP.print_lvl (lvl + 1) "<Ptr SymImm %d>\n" id;
        List.iteri (
          fun i (left, right) ->
            PP.print_lvl (lvl + 2) "<Addr Range %d> " i;
            SingleExp.pp_single_exp (lvl + 2) left;
            SingleExp.pp_single_exp (lvl + 2) right;
            Printf.printf "\n"
        ) key_list
    ) mem_key_list

  let pp_addr_exp (lvl: int) (addr_exp: (TypeFullExp.t * int64) list) =
    PP.print_lvl lvl "Addr exp list:\n";
    List.iteri (
      fun i x ->
        let exp, size = x in
        PP.print_lvl (lvl + 1) "<Addr %d>" i;
        TypeFullExp.pp_type_full_exp (lvl + 2) exp;
        PP.print_lvl (lvl + 1) "%Ld" size;
        Printf.printf "\n"
    ) addr_exp

  let pp_base_range (lvl: int) (base_range_list: (Isa.imm_var_id * MemOffset.t) list) =
    PP.print_lvl lvl "Base range list:\n";
    List.iteri (
      fun i x ->
        let base_id, (left, right) = x in
        PP.print_lvl (lvl + 1) "<Addr Range %d> %d" i base_id;
        SingleExp.pp_single_exp (lvl + 2) left;
        SingleExp.pp_single_exp (lvl + 2) right;
        Printf.printf "\n"
    ) base_range_list

  let pp_mem_type (lvl: int) (mem_key_list: (Isa.imm_var_id * ((MemOffset.t * TypeExp.t) list)) list) =
    PP.print_lvl lvl "Mem type:\n";
    List.iter (
      fun (id, key_list) ->
        PP.print_lvl (lvl + 1) "<Ptr SymImm %d>\n" id;
        List.iteri (
          fun i ((left, right), mem_type) ->
            PP.print_lvl (lvl + 2) "<Addr Range %d> " i;
            SingleExp.pp_single_exp (lvl + 2) left;
            SingleExp.pp_single_exp (lvl + 2) right;
            TypeExp.pp_type_exp (lvl + 2) mem_type;
            Printf.printf "\n"
        ) key_list
    ) mem_key_list

end

module MemRangeTypeBase = MemType (TypeExp)

module MemRangeType = struct
include MemRangeTypeBase

  let format_convert_helper = List.map (fun (off, _) -> (off, false))

  let update_offset_one_ptr
      (old_mem_type: (MemOffset.t * TypeExp.t) list)
      (mem_access_list: MemOffset.t list) :
      ((MemOffset.t * bool) list) * MemOffset.ConstraintSet.t =
    let rec helper
        (acc: ((MemOffset.t * bool) list) * MemOffset.ConstraintSet.t)
        (offset: MemOffset.t) :
        ((MemOffset.t * bool) list) * MemOffset.ConstraintSet.t =
      (* let left, right = offset in *)
      let acc_update_list, acc_constraint = acc in
      let range_acc_constraint = MemOffset.ConstraintSet.union (MemOffset.check_offset offset) acc_constraint in
      match acc_update_list with
      | [] -> ([ (offset, true) ], range_acc_constraint)
      | (hd_offset, hd_updated) :: tl ->
        let cmp_result, cmp_constraint = MemOffset.cmp_or_merge offset hd_offset in
        let final_constraint = MemOffset.ConstraintSet.union range_acc_constraint cmp_constraint in
        match cmp_result with
        | Left true -> (* offset.right <= hd_offset.left *)
          ((offset, true) :: acc_update_list, final_constraint)
        | Left false -> (* hd_offset.right <= offset.left *)
          let udpate_tl, rec_constraint = helper (tl, final_constraint) offset in
          ((hd_offset, hd_updated) :: udpate_tl, rec_constraint)
        | Right new_offset -> (* merge offset and hd_offset *)
          helper (tl, final_constraint) new_offset
        (* if SingleExp.must_ge hd_left right then (left, right, true) :: acc
        else if SingleExp.must_ge left hd_right then (hd_left, hd_right, hd_updated) :: (helper tl offset)
        else
          begin match SingleExp.get_less left hd_left, SingleExp.get_greater right hd_right with
          | Some l, Some r -> helper tl (l, r)
          | _ -> 
            Printf.printf "Cannot merge [%s, %s] [%s, %s]\n" 
              (SingleExp.to_string left) 
              (SingleExp.to_string right) 
              (SingleExp.to_string hd_left) 
              (SingleExp.to_string hd_right);
            mem_type_error "update_offset cannot merge address offset range" 
          end *)
    in
    let acc = format_convert_helper old_mem_type in
    List.fold_left helper (acc, MemOffset.ConstraintSet.empty) mem_access_list

  let rec update_offset_all_ptr
      (old_mem_type: (Isa.imm_var_id * (MemOffset.t * TypeExp.t) list) list)
      (mem_access_list: (Isa.imm_var_id * MemOffset.t list) list) :
      ((Isa.imm_var_id * (MemOffset.t * bool) list) list) * MemOffset.ConstraintSet.t =
    (* pp_mem_key 0 mem_access_list; *)
    match old_mem_type, mem_access_list with
    | (id1, mem1) :: tl1, (id2, mem2) :: tl2 ->
      if id1 = id2 then
        let hd_update, hd_constraint = update_offset_one_ptr mem1 mem2 in
        let tl_update, tl_constraint = update_offset_all_ptr tl1 tl2 in
        ((id1, hd_update) :: tl_update, MemOffset.ConstraintSet.union hd_constraint tl_constraint)
        (* (id1, update_offset_one_ptr mem1 mem2) :: (update_offset_all_ptr tl1 tl2) *)
      else if id1 < id2 then
        let tl_update, tl_constraint = update_offset_all_ptr tl1 mem_access_list in
        ((id1, format_convert_helper mem1) :: tl_update, tl_constraint)
        (* (id1, format_convert_helper mem1) :: (update_offset_all_ptr tl1 mem_access_list) *)
      else (* id2 < id1 *)
        let hd_update, hd_constraint = update_offset_one_ptr [] mem2 in
        let tl_udpate, tl_constraint = update_offset_all_ptr old_mem_type tl2 in
        ((id2, hd_update) :: tl_udpate, MemOffset.ConstraintSet.union hd_constraint tl_constraint)
        (* (id2, update_offset_one_ptr [] mem2) :: (update_offset_all_ptr old_mem_type tl2) *)
    | _ :: _, [] -> 
      (List.map (fun (ptr, offset_list) -> (ptr, format_convert_helper offset_list)) old_mem_type, MemOffset.ConstraintSet.empty)
    | [], (id2, mem2) :: tl2 ->
      let hd_update, hd_constraint = update_offset_one_ptr [] mem2 in
      let tl_update, tl_constraint = update_offset_all_ptr [] tl2 in
      ((id2, hd_update) :: tl_update, MemOffset.ConstraintSet.union hd_constraint tl_constraint)
      (* (id2, update_offset_one_ptr [] mem2) :: (update_offset_all_ptr [] tl2) *)
    | [], [] -> ([], MemOffset.ConstraintSet.empty)

  let rec find_drop_head
      (old_mem_type: (MemOffset.t * TypeExp.t) list)
      (drop_set: TypeExp.TypeVarSet.t)
      (offset: MemOffset.t) :
      (* (left: SingleExp.t) (right: SingleExp.t) : *)
      TypeExp.t * ((MemOffset.t * TypeExp.t) list) * TypeExp.TypeVarSet.t =
    match old_mem_type with
    | [] -> mem_type_error "find_drop_head type target not found"
    | (hd_offset, mem_t) :: tl ->
      if MemOffset.cmp offset hd_offset = 0 then (* SingleExp.cmp left l = 0 && SingleExp.cmp right r = 0 *) 
        (mem_t, tl, drop_set)
      else
        begin match mem_t with
        | TypeVar x ->
          find_drop_head tl (TypeExp.TypeVarSet.add x drop_set) offset
        | TypeSingle _ -> find_drop_head tl drop_set offset
        | _ -> mem_type_error "find_drop_head type being processed is not TypeVar or TypeSingle"
        end

  let drop_all
      (old_mem_type: (MemOffset.t * TypeExp.t) list)
      (drop_set: TypeExp.TypeVarSet.t) :
      TypeExp.TypeVarSet.t =
    let helper (acc: TypeExp.TypeVarSet.t) (entry: MemOffset.t * TypeExp.t) : TypeExp.TypeVarSet.t =
      let _, mem_t = entry in
      match mem_t with
      | TypeVar x -> TypeExp.TypeVarSet.add x acc
      | TypeSingle _ -> acc
      | _ -> mem_type_error "drop_all type being processed is not TypeVar or TypeSingle"
    in
    List.fold_left helper drop_set old_mem_type

  let rec update_type_one_ptr
      (old_mem_type: (MemOffset.t * TypeExp.t) list)
      (start_var_idx: (TypeExp.type_var_id, Isa.imm_var_id) Either.t)
      (new_var_set: TypeExp.TypeVarSet.t) (drop_var_set: TypeExp.TypeVarSet.t)
      (update_list: (MemOffset.t * bool) list) :
      ((MemOffset.t * TypeExp.t) list) * ((TypeExp.type_var_id, Isa.imm_var_id) Either.t) * TypeExp.TypeVarSet.t * TypeExp.TypeVarSet.t =
    match update_list with
    | [] -> 
      let d_set = drop_all old_mem_type drop_var_set in
      ([], start_var_idx, new_var_set, d_set)
    | (offset, false) :: tl ->
      let hd_type, d_mem_type, d_set = find_drop_head old_mem_type drop_var_set offset in
      let d_mem_type, update_var_idx, n_set, d_set = update_type_one_ptr d_mem_type start_var_idx new_var_set d_set tl in
      ((offset, hd_type) :: d_mem_type, update_var_idx, n_set, d_set)
    | (offset, true) :: tl ->
      begin match start_var_idx with
      | Left type_var_idx ->
        let d_mem_type, update_var_idx, n_set, d_set = update_type_one_ptr old_mem_type (Left (type_var_idx + 1)) new_var_set drop_var_set tl in
        ((offset, TypeVar type_var_idx) :: d_mem_type, update_var_idx, TypeExp.TypeVarSet.add type_var_idx n_set, d_set)
      | Right single_var_idx ->
        let d_mem_type, update_var_idx, n_set, d_set = update_type_one_ptr old_mem_type (Right (single_var_idx + 1)) new_var_set drop_var_set tl in
        ((offset, TypeSingle (SingleVar single_var_idx)) :: d_mem_type, update_var_idx, n_set, d_set)
      end
    
  let rec update_type_all_ptr
      (old_t: (Isa.imm_var_id * ((MemOffset.t * TypeExp.t) list)) list)
      (start_var_idx: (TypeExp.type_var_id, Isa.imm_var_id) Either.t)
      (new_var_set: TypeExp.TypeVarSet.t) (drop_var_set: TypeExp.TypeVarSet.t)
      (update_list: (Isa.imm_var_id * (MemOffset.t * bool) list) list) :
      ((Isa.imm_var_id * ((MemOffset.t * TypeExp.t) list)) list) * ((TypeExp.type_var_id, Isa.imm_var_id) Either.t) * TypeExp.TypeVarSet.t * TypeExp.TypeVarSet.t =
    match old_t, update_list with
    | (id1, mem1) :: tl1, (id2, mem2) :: tl2 ->
      if id1 = id2 then
        let mem_type, var_idx, n_var_set, d_var_set = update_type_one_ptr mem1 start_var_idx new_var_set drop_var_set mem2 in
        let result, var_idx2, n_var_set2, d_var_set2 = update_type_all_ptr tl1 var_idx n_var_set d_var_set tl2 in
        ((id1, mem_type) :: result, var_idx2, n_var_set2, d_var_set2)
        (* (id1, update_type_one_ptr mem1 mem2) :: (update_type_all_ptr tl1 tl2) *)
      else if id1 < id2 then
        let result, var_idx, n_var_set, d_var_set = update_type_all_ptr tl1 start_var_idx new_var_set drop_var_set update_list in
        ((id1, mem1) :: result, var_idx, n_var_set, d_var_set)
        (* (id1, mem1) :: (update_type_all_ptr tl1 mem_access_list) *)
      else (* id2 < id1 *)
        let mem_type, var_idx, n_var_set, d_var_set = update_type_one_ptr [] start_var_idx new_var_set drop_var_set mem2 in
        let result, var_idx2, n_var_set2, d_var_set2 = update_type_all_ptr old_t var_idx n_var_set d_var_set tl2 in
        ((id2, mem_type) :: result, var_idx2, n_var_set2, d_var_set2)
        (* (id2, update_type_one_ptr [] mem2) :: (update_type_all_ptr old_mem_type tl2) *)
    | [], (id2, mem2) :: tl2 ->
      let mem_type, var_idx, n_var_set, d_var_set = update_type_one_ptr [] start_var_idx new_var_set drop_var_set mem2 in
      let result, var_idx2, n_var_set2, d_var_set2 = update_type_all_ptr [] var_idx n_var_set d_var_set tl2 in
      ((id2, mem_type) :: result, var_idx2, n_var_set2, d_var_set2)
      (* (id2, update_type_one_ptr [] mem2) :: (update_type_all_ptr [] tl2) *)
    | _, [] -> (old_t, start_var_idx, new_var_set, drop_var_set)

  let update_ptr_list
      (ptr_set: MemKeySet.t)
      (update_list: (Isa.imm_var_id * (MemOffset.t * bool) list) list) :
      MemKeySet.t =
    List.fold_left (
      fun acc (x, _) -> MemKeySet.add x acc
    ) ptr_set update_list

  let update_mem_type
      (old_t: t)
      (start_var_idx: (TypeExp.type_var_id, Isa.imm_var_id) Either.t)
      (new_var_set: TypeExp.TypeVarSet.t) (drop_var_set: TypeExp.TypeVarSet.t)
      (update_list: (Isa.imm_var_id * (MemOffset.t * bool) list) list) :
      t * ((TypeExp.type_var_id, Isa.imm_var_id) Either.t) * TypeExp.TypeVarSet.t * TypeExp.TypeVarSet.t =
    let new_mem_type, x, y, z = update_type_all_ptr old_t.mem_type start_var_idx new_var_set drop_var_set update_list in
    ({
      ptr_list = update_ptr_list old_t.ptr_list update_list;
      mem_type = new_mem_type;
    }, x, y, z)

  (* Find base for previously unknown addresses *)
  let repl_addr_exp (addr_exp_list: (TypeFullExp.t * int64) list) (subtype_sol: (TypeExp.type_var_id * TypeFullExp.type_sol) list) : (TypeFullExp.t * int64) list =
    let helper (x: TypeFullExp.t * int64) : TypeFullExp.t * int64 =
      let e, size = x in 
      let new_e = TypeFullExp.repl_all_sol subtype_sol e in (new_e, size)
    in
    List.map helper addr_exp_list

  let rec filter_single_var (e: SingleExp.t) : SingleExp.SingleVarSet.t * SingleExp.SingleVarSet.t =
    match e with
    | SingleVar x -> (SingleExp.SingleVarSet.singleton x, SingleExp.SingleVarSet.empty)
    | SingleBExp (SingleExp.SingleAdd, l, r) ->
      let left_ptr, left_no_ptr = filter_single_var l in
      let right_ptr, right_no_ptr = filter_single_var r in
      (SingleExp.SingleVarSet.union left_ptr right_ptr, SingleExp.SingleVarSet.union left_no_ptr right_no_ptr)
    | SingleBExp (SingleExp.SingleMul, l, r) ->
      let left_ptr, left_no_ptr = filter_single_var l in
      let right_ptr, right_no_ptr = filter_single_var r in
      (SingleExp.SingleVarSet.empty, SingleExp.SingleVarSet.union (SingleExp.SingleVarSet.union left_ptr right_ptr) (SingleExp.SingleVarSet.union left_no_ptr right_no_ptr))
    | _ -> (SingleExp.SingleVarSet.empty, SingleExp.SingleVarSet.empty)

  let filter_type_single_var (e: TypeExp.t) : SingleExp.SingleVarSet.t * SingleExp.SingleVarSet.t =
    match e with
    | TypeSingle s -> filter_single_var s
    | TypeRange (l, _, r, _, _) ->
      let left_ptr, left_no_ptr = filter_single_var l in
      let right_ptr, right_no_ptr = filter_single_var r in
      let diff1 = SingleExp.SingleVarSet.diff left_ptr right_ptr in
      let diff2 = SingleExp.SingleVarSet.diff right_ptr left_ptr in
      (SingleExp.SingleVarSet.inter left_ptr right_ptr,
      SingleExp.SingleVarSet.union (SingleExp.SingleVarSet.union diff1 diff2) (SingleExp.SingleVarSet.union left_no_ptr right_no_ptr))
    | _ -> (SingleExp.SingleVarSet.empty, SingleExp.SingleVarSet.empty)

  let try_solve_base 
      (acc: SingleExp.SingleVarSet.t * SingleExp.SingleVarSet.t)
      (base_list: (SingleExp.SingleVarSet.t, Isa.imm_var_id) Either.t) : 
      (SingleExp.SingleVarSet.t * SingleExp.SingleVarSet.t) * ((SingleExp.SingleVarSet.t, Isa.imm_var_id) Either.t) =
    let ptr_list, no_ptr_list = acc in
    match base_list with
    | Left base_list ->
      let base_list = SingleExp.SingleVarSet.diff base_list no_ptr_list in
      let inter_list = SingleExp.SingleVarSet.inter base_list ptr_list in
      let diff_list = SingleExp.SingleVarSet.diff base_list ptr_list in
      begin match SingleExp.SingleVarSet.elements inter_list, SingleExp.SingleVarSet.elements diff_list with
      | [], [] -> mem_type_error "try_solve_base get empty base candidate"
      | [], hd :: [] -> ((SingleExp.SingleVarSet.add hd ptr_list, no_ptr_list), Right hd)
      | [], _ :: _ -> (acc, Left diff_list)
      | hd :: [], _ -> ((ptr_list, SingleExp.SingleVarSet.union diff_list no_ptr_list), Right hd)
      | _ :: _, _ -> mem_type_error "try_solve_base add more than one ptrs"
      end
    | Right _ -> (acc, base_list)
  
  let rec solve_base 
      (acc: SingleExp.SingleVarSet.t * SingleExp.SingleVarSet.t)
      (base_list: ((SingleExp.SingleVarSet.t, Isa.imm_var_id) Either.t) list)
      (iter: int) : 
      (SingleExp.SingleVarSet.t * SingleExp.SingleVarSet.t) * Isa.imm_var_id list =
    let to_id (x: (SingleExp.SingleVarSet.t, Isa.imm_var_id) Either.t) : Isa.imm_var_id =
      match x with
      | Left _ -> mem_type_error "solve_base cannot find solution"
      | Right id -> id
    in
    if iter = 0 then (acc, List.map to_id base_list)
    else
      let new_acc, new_list = List.fold_left_map try_solve_base acc base_list in
      solve_base new_acc new_list (iter - 1)

  let get_base 
      (ptr_list: SingleExp.SingleVarSet.t) 
      (no_ptr_list: SingleExp.SingleVarSet.t) 
      (addr_list: (TypeFullExp.t * int64) list) : 
      (SingleExp.SingleVarSet.t * SingleExp.SingleVarSet.t) * Isa.imm_var_id list =
    let ptr_set_list, no_ptr_set_list = List.split (List.map (fun ((e, _), _) -> filter_type_single_var e) addr_list) in
    let no_ptr_set = List.fold_left (fun acc x -> SingleExp.SingleVarSet.union acc x) SingleExp.SingleVarSet.empty no_ptr_set_list in
    let ptr_set_list = List.map (
      fun x : (SingleExp.SingleVarSet.t, Isa.imm_var_id) Either.t -> 
        Left (SingleExp.SingleVarSet.diff x no_ptr_set)) 
      ptr_set_list in
    solve_base (ptr_list, no_ptr_list) ptr_set_list 1
    
  let pp_base (lvl: int) (base_list: Isa.imm_var_id list) =
    PP.print_lvl lvl "Base list:\n";
    List.iteri (
      fun i x ->
        PP.print_lvl (lvl + 1) "<Addr Range %d> %d\n" i x
    ) base_list

  let filter_addr_list (addr_list: (TypeFullExp.t * int64) list) : (TypeFullExp.t * int64) list =
    List.filter (
      fun ((e, _), _) ->
        match e with
        | TypeExp.TypeSingle _ | TypeExp.TypeRange _ -> true
        | _ -> false
    ) addr_list

  let get_addr_base_range 
      (ptr_list: SingleExp.SingleVarSet.t)
      (no_ptr_list: SingleExp.SingleVarSet.t)
      (addr_list: (TypeFullExp.t * int64) list) : 
      (SingleExp.SingleVarSet.t * SingleExp.SingleVarSet.t) * ((Isa.imm_var_id * MemOffset.t) list) =
    let addr_list = filter_addr_list addr_list in
    let (new_ptr_list, new_no_ptr_list), base_list = get_base ptr_list no_ptr_list addr_list in
    let helper (base_id: Isa.imm_var_id) (mem_access: TypeFullExp.t * int64) : Isa.imm_var_id * MemOffset.t =
      let (addr, _), size = mem_access in
      match addr with
      | TypeSingle x -> (base_id, 
          (SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleSub, x, SingleExp.SingleVar base_id)),
          SingleExp.eval (SingleExp.SingleBExp (
            SingleExp.SingleAdd, 
            SingleExp.SingleBExp (SingleExp.SingleSub, x, SingleExp.SingleVar base_id), 
            SingleExp.SingleConst size))))
      | TypeRange (l, _, r, _, _) -> (base_id,
          (SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleSub, l, SingleExp.SingleVar base_id)),
          SingleExp.eval (SingleExp.SingleBExp (
            SingleExp.SingleAdd,
            SingleExp.SingleBExp (SingleExp.SingleSub, r, SingleExp.SingleVar base_id),
            SingleExp.SingleConst size))))
      | _ -> mem_type_error "get_addr_base_range cannot handle this case"
    in
    ((new_ptr_list, new_no_ptr_list), List.map2 helper base_list addr_list)

  let reshape_mem_key_list (mem_access_list: (Isa.imm_var_id * MemOffset.t) list) :
      (Isa.imm_var_id * (MemOffset.t list)) list =
    let helper (acc: (Isa.imm_var_id * (MemOffset.t list)) list) (mem_access: Isa.imm_var_id * MemOffset.t) : 
        (Isa.imm_var_id * (MemOffset.t list)) list =
      let id, offset = mem_access in
      let helper0 (id_mem_key_list: Isa.imm_var_id * (MemOffset.t list)) : 
            (Isa.imm_var_id * (MemOffset.t list), Isa.imm_var_id * (MemOffset.t list)) Either.t =
        let curr_id, mem_key_list = id_mem_key_list in
        if curr_id = id then Either.left (curr_id, offset :: mem_key_list)
        else Either.right id_mem_key_list
      in
      let left_list, right_list = List.partition_map helper0 acc in
      match left_list with
      | [] -> (id, [ offset ]) :: right_list
      | hd :: [] -> hd :: right_list
      | _ -> mem_type_error "get_mem_key_list merged with more than one key"
    in
    List.fold_left helper [] mem_access_list

  let pp_update_list (lvl: int) (update_list: ((Isa.imm_var_id * (MemOffset.t * bool) list) list)) =
    PP.print_lvl lvl "Update list:\n";
    List.iter (
      fun (id, key_list) ->
        PP.print_lvl (lvl + 1) "<Ptr SymImm %d>\n" id;
        List.iteri (
          fun i ((left, right), b) ->
            PP.print_lvl (lvl + 2) "<Addr Range %d> " i;
            SingleExp.pp_single_exp (lvl + 2) left;
            SingleExp.pp_single_exp (lvl + 2) right;
            Printf.printf " %b\n" b
        ) key_list
    ) update_list
end

