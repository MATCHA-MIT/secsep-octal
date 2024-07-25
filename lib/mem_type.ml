open Isa
open Single_exp
open Type_exp
open Type_full_exp
open Mem_offset
open Smt_emitter
open Taint_dep_exp
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
  (* MemOffset.t represents the absolute range, rather than the relative offset to the base *)
  type 'a mem_content = (Isa.imm_var_id * ((MemOffset.t * 'a) list)) list
  type t = {
    ptr_list: MemKeySet.t;
    mem_type: entry_t mem_content
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

  
  (* The following two functions are re-defined later??? *)
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

  let is_addr_single_exp (addr: TypeExp.t) : bool =
    match addr with
    | TypeSingle _
    | TypeRange _ -> true
    | _ -> false

  (* Given an address, get the range of the corresponding range *)
  let get_abs_offset (addr_exp: TypeExp.t * int64) : MemOffset.t * bool =
    let addr, size = addr_exp in
    match addr with
    | TypeSingle s -> (
        (
          s,
          SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleAdd, s, SingleExp.SingleConst size))
        ),
        true
      )
    | TypeRange (s1, true, s2, true, _) -> (
        (
          s1,
          SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleAdd, s2, SingleExp.SingleConst size))
        ),
        false
      )
    | _ -> 
      mem_type_error ("get_abs_offset not implement for " ^ (TypeExp.to_string addr))

  (* TODO: Double check this new implementation!!! *)
  let get_mem_entry_with_addr (smt_ctx: SmtEmitter.t) (mem: t) (addr_exp: TypeExp.t * int64)
    : ((Isa.imm_var_id * MemOffset.t * bool) * entry_t * MemOffset.ConstraintSet.t, TypeExp.t * int64) Either.t =
    let addr, _ = addr_exp in
    (* if there exists TypeVar, return *)
    if not (is_addr_single_exp addr) then Right addr_exp else
    let base_opt = find_base addr mem.ptr_list in
    let addr_offset, is_single_slot = get_abs_offset addr_exp in
    let addr_constraint = MemOffset.check_offset smt_ctx addr_offset in
    match base_opt with
    | None -> begin (* Try all slots and see if the new address fits in one determinedly *)
        let addr_offset, is_single_slot = get_abs_offset addr_exp in
        let helper (mem_of_base: Isa.imm_var_id * ((MemOffset.t * entry_t) list)) =
          let base, base_mem = mem_of_base in
          match List.filter_map (fun (offset, offset_type) -> 
            if MemOffset.equal addr_offset offset then begin 
              if is_single_slot then 
                  Some ((base, offset, true), offset_type, MemOffset.ConstraintSet.empty)
              else
                  Some ((base, offset, false), Entry.partial_read_val offset_type, MemOffset.ConstraintSet.empty)
            end else begin
              match MemOffset.subset smt_ctx addr_offset offset with
              | MemOffset.SatYes -> Some ((base, offset, false), Entry.partial_read_val offset_type, MemOffset.ConstraintSet.empty)
              | MemOffset.SatCond constraints ->
                Printf.printf "get_mem_entry_with_addr: new constraints of\n";
                MemOffset.pp_offset 0 addr_offset;
                Printf.printf " in ";
                MemOffset.pp_offset 0 offset;
                Printf.printf "\n";
                MemOffset.pp_constraint_set 0 constraints;
                Printf.printf "\n";
                Some ((base, offset, false), Entry.partial_read_val offset_type, constraints)
              | MemOffset.SatNo -> None
            end
          ) base_mem with
          | [] -> None
          | res -> Some res
        in
        match List.filter_map helper mem.mem_type with
        | [] -> Right addr_exp
        | [(offset_info, mem_type, match_constraint)] :: [] ->
            Left (offset_info, mem_type, MemOffset.ConstraintSet.union addr_constraint match_constraint) (* found unique match *)
        | _ -> mem_type_error "get_mem_entry_with_addr found more than one match"
      end
    | Some base ->
      let _, offset_list = List.find (fun (i, _) -> i = base) mem.mem_type in (* TODO: find_opt *)
      let find_entry = List.find_map (
        fun (offset, mem_type) ->
          if MemOffset.equal addr_offset offset then 
            begin if is_single_slot then 
                Some (offset, true, mem_type, MemOffset.ConstraintSet.empty)
            else
                Some (offset, false, Entry.partial_read_val mem_type, MemOffset.ConstraintSet.empty)
            end
          else
            match MemOffset.subset smt_ctx addr_offset offset with
            | MemOffset.SatYes -> Some (offset, false, Entry.partial_read_val mem_type, MemOffset.ConstraintSet.empty)
            | MemOffset.SatCond constraints ->
                Printf.printf "get_mem_entry_with_addr: new constraints (base found) of\n";
                MemOffset.pp_offset 0 addr_offset;
                Printf.printf " in ";
                MemOffset.pp_offset 0 offset;
                Printf.printf "\n";
                MemOffset.pp_constraint_set 0 constraints;
                Printf.printf "\n";
                Some (offset, false, Entry.partial_read_val mem_type, constraints)
            | MemOffset.SatNo -> None
      ) offset_list in
      begin match find_entry with
      | None -> Right addr_exp
      | Some (offset, is_single_full_slot, mem_type, match_constraint) ->
          Left ((base, offset, is_single_full_slot), mem_type, MemOffset.ConstraintSet.union addr_constraint match_constraint)
      end

  let get_mem_type_with_addr (smt_ctx: SmtEmitter.t) (mem: t) (addr_exp: TypeExp.t * int64) : (entry_t * MemOffset.ConstraintSet.t, TypeExp.t * int64) Either.t =
    match get_mem_entry_with_addr smt_ctx mem addr_exp with
    | Left (_, entry_type, constraint_set) -> Left (entry_type, constraint_set)
    | Right x -> Right x
    (* let addr, _ = addr_exp in
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
      end *)

  let set_mem_entry_with_addr 
      (smt_ctx: SmtEmitter.t) (mem: t) (addr_exp: TypeExp.t * int64) (new_type: entry_t)
      : ((Isa.imm_var_id * MemOffset.t * bool) * t * MemOffset.ConstraintSet.t, TypeExp.t * int64) Either.t =
    let addr, _ = addr_exp in
    (* if there exists TypeVar, return *)
    if not (is_addr_single_exp addr) then Right addr_exp else
    let base_opt = find_base addr mem.ptr_list in
    let addr_offset, is_single_slot = get_abs_offset addr_exp in
    let addr_constraint = MemOffset.check_offset smt_ctx addr_offset in
    match base_opt with
    | None -> begin
        let addr_offset, is_single_slot = get_abs_offset addr_exp in
        let helper acc (mem_of_base: Isa.imm_var_id * ((MemOffset.t * entry_t) list)) =
          match acc with
          | Some _ -> (acc, mem_of_base)
          | None -> begin
              let base, base_mem = mem_of_base in
              let acc, new_base_mem = List.fold_left_map (fun acc entry ->
                let offset, offset_type = entry in
                match acc with
                | Some _ -> acc, entry
                | None -> begin
                    if MemOffset.equal addr_offset offset then begin
                      if is_single_slot then 
                          Some (base, offset, true, MemOffset.ConstraintSet.empty), (offset, new_type)
                      else
                          Some (base, offset, false, MemOffset.ConstraintSet.empty), (offset, Entry.partial_write_val offset_type new_type)
                    end else begin
                      match MemOffset.subset smt_ctx addr_offset offset with
                      | MemOffset.SatYes -> Some (base, offset, false, MemOffset.ConstraintSet.empty), (offset, Entry.partial_write_val offset_type new_type)
                      | MemOffset.SatCond constraints ->
                        Printf.printf "set_mem_entry_with_addr: new constraints\n";
                        MemOffset.pp_constraint_set 0 constraints;
                        Printf.printf "\n";
                        Some (base, offset, false, constraints), (offset, Entry.partial_write_val offset_type new_type)
                      | MemOffset.SatNo -> None, entry
                    end
                  end
              ) None base_mem
              in
              (acc, (base, new_base_mem))
            end
        in
        match List.fold_left_map helper None mem.mem_type with
        | None, _ -> Right addr_exp
        | Some (base, offset, is_single_full_slot, constraints), new_mem_type ->
            Left ((base, offset, is_single_full_slot), {mem with mem_type = new_mem_type}, constraints)
      end
    | Some base ->
      (* let _, offset_list = List.find (fun (i, _) -> i = base) mem.mem_type in *)
      let helper0
          (acc: (MemOffset.t * bool * MemOffset.ConstraintSet.t) option) 
          (entry: MemOffset.t * entry_t) : 
          ((MemOffset.t * bool * MemOffset.ConstraintSet.t) option) * (MemOffset.t * entry_t) =
        let offset, offset_type = entry in
        match acc with
        | Some _ -> (acc, entry)
        | None ->
          if MemOffset.equal addr_offset offset then
            begin if is_single_slot then
              (Some (offset, true, MemOffset.ConstraintSet.empty), (offset, new_type))
            else
              (Some (offset, false, MemOffset.ConstraintSet.empty), (offset, Entry.partial_write_val offset_type new_type))
            end
          else begin
            match MemOffset.subset smt_ctx addr_offset offset with
            | MemOffset.SatYes -> (Some (offset, false, MemOffset.ConstraintSet.empty), (offset, Entry.partial_write_val offset_type new_type))
            | MemOffset.SatCond constraints ->
              Printf.printf "set_mem_entry_with_addr: new constraints (base found)\n";
              MemOffset.pp_constraint_set 0 constraints;
              Printf.printf "\n";
              (Some (offset, false, constraints), (offset, Entry.partial_write_val offset_type new_type))
            | MemOffset.SatNo -> (None, entry)
          end
      in
      let helper
          (acc: (Isa.imm_var_id * MemOffset.t * bool * MemOffset.ConstraintSet.t) option)
          (entry: Isa.imm_var_id * ((MemOffset.t * entry_t) list)) :
          ((Isa.imm_var_id * MemOffset.t * bool * MemOffset.ConstraintSet.t) option) * (Isa.imm_var_id * ((MemOffset.t * entry_t) list)) =
        let k, o_list = entry in
        match acc with
        | Some acc -> (Some acc, entry)
        | None ->
          if k = base then
            let new_acc, new_o_list = List.fold_left_map helper0 None o_list in
            begin match new_acc with
            | Some (offset, is_single_full_slot, constraint_list) ->
              (Some (k, offset, is_single_full_slot, constraint_list), (k, new_o_list))
            | None -> (None, entry)
            end
            (* let (offset, is_single_full_slot, constraint_list), new_o_list = List.fold_left_map helper0 None o_list in
            (
              constraint_list,
              (k, new_o_list)
            ) *)
          else (None, entry)
      in
      let new_acc, new_mem_type = List.fold_left_map helper None mem.mem_type in
      match new_acc with
      | None -> Right addr_exp
      | Some (ptr, offset, is_single_full_slot, constraint_list) ->
        Left (
          (ptr, offset, is_single_full_slot),
          {mem with mem_type = new_mem_type},
          MemOffset.ConstraintSet.union addr_constraint constraint_list
        )

  let set_mem_type_with_addr 
      (smt_ctx: SmtEmitter.t) (mem: t) (addr_exp: TypeExp.t * int64) (new_type: entry_t) : 
      (t * MemOffset.ConstraintSet.t, TypeExp.t * int64) Either.t =
    match set_mem_entry_with_addr smt_ctx mem addr_exp new_type with
    | Left (_, new_mem, constraint_set) -> Left (new_mem, constraint_set)
    | Right x -> Right x
    (* let addr, _ = addr_exp in
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
        ) *)

  let init_mem_type : t =
    {
      ptr_list = MemKeySet.empty;
      mem_type = [];
    }

  let init_mem_type_from_layout 
      (start_var: entry_t) (ptr_list: MemKeySet.t) (mem_layout: 'a mem_content) : entry_t * t =
    let helper (acc: entry_t) (entry: MemOffset.t * 'a) : entry_t * (MemOffset.t * entry_t) =
      let offset, _ = entry in (Entry.next_var acc, (offset, acc))
    in
    let helper2 
        (acc: entry_t) (entry: Isa.imm_var_id * ((MemOffset.t * 'a) list)) :
        entry_t * (Isa.imm_var_id * ((MemOffset.t * entry_t) list)) =
      let ptr, offset_list = entry in
      let new_acc, new_offset_list = List.fold_left_map helper acc offset_list in
      (new_acc, (ptr, new_offset_list))
    in
    let next_var, new_mem = List.fold_left_map helper2 start_var mem_layout in
    (next_var, { ptr_list = ptr_list; mem_type = new_mem })

  let to_absolute_offset (m: t) : t =
    let helper (mem_of_base: Isa.imm_var_id * ((MemOffset.t * 'a) list)) =
      let base, base_mem = mem_of_base in
      let new_base_mem = List.map (fun ((l, r), mem_type) -> ((
          SingleExp.eval (SingleBExp (SingleAdd, SingleVar base, l)),
          SingleExp.eval (SingleBExp (SingleAdd, SingleVar base, r))
        ), mem_type)
      ) base_mem in
      (base, new_base_mem)
    in
    { m with mem_type = List.map helper m.mem_type }

  (* Only update ptr val in base, but type single var idx are still not correct in init_mem *)
  let update_mem_entry_base_id (m: t) (start_idx: Isa.imm_var_id) : t =
    let new_ptr_list = MemKeySet.of_list (List.map (fun x -> x + start_idx) (MemKeySet.elements m.ptr_list)) in
    let rec single_const_add_base (x: SingleExp.t) : SingleExp.t =
      match x with
      | SingleVar v -> SingleVar (v + start_idx)
      | SingleBExp (op, e1, e2) -> SingleBExp (op, single_const_add_base e1, single_const_add_base e2)
      | SingleUExp (op, e) -> SingleUExp (op, single_const_add_base e)
      | _ -> x
    in
    let helper (entry: MemOffset.t * entry_t) : MemOffset.t * entry_t =
      let (l, r), t = entry in ((single_const_add_base l, single_const_add_base r), t)
    in
    let helper2 (entry: Isa.imm_var_id * ((MemOffset.t * entry_t) list)) : Isa.imm_var_id * ((MemOffset.t * entry_t) list) =
      let ptr, entry_list = entry in (ptr + start_idx, List.map helper entry_list)
    in
    { ptr_list = new_ptr_list; mem_type = List.map helper2 m.mem_type }

  let sort_mem_type (m: t) : t =
    let compare 
        (e1: Isa.imm_var_id * ((MemOffset.t * entry_t) list)) 
        (e2: Isa.imm_var_id * ((MemOffset.t * entry_t) list)) : int =
      let ptr1, _ = e1 in
      let ptr2, _ = e2 in
      if ptr1 < ptr2 then -1
      else if ptr1 = ptr2 then 0
      else 1
    in
    { m with mem_type = List.sort compare m.mem_type }

  let pp_mem_key (lvl: int) (mem_key_list: (Isa.imm_var_id * (MemOffset.t list)) list * (MemOffset.t list)) =
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
    ) (fst mem_key_list);
    if (snd mem_key_list) <> [] then begin
      PP.print_lvl (lvl + 1) "Mem key list (base unknown):\n";
      List.iter (fun key ->
        MemOffset.pp_offset (lvl + 2) key;
        Printf.printf "\n";
      ) (snd mem_key_list)
    end

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

module MemTaintDepType = MemType (TaintDepExp)

module MemRangeType = struct
include MemRangeTypeBase

  let format_convert_helper = List.map (fun (off, _) -> (off, false))

  (* TODO: In mem_access_list, for address that cannot separate offset, we can consider pass in the full address *)
  let update_offset_one_ptr
      (smt_ctx: SmtEmitter.t)
      (old_mem_type: (MemOffset.t * TypeExp.t) list)
      (mem_access_list: MemOffset.t list) :
      ((MemOffset.t * bool) list) * MemOffset.ConstraintSet.t * (MemOffset.t list) = 
    let rec helper
        (acc: ((MemOffset.t * bool) list) * MemOffset.ConstraintSet.t * (MemOffset.t list))
        (range: MemOffset.t) :
        ((MemOffset.t * bool) list) * MemOffset.ConstraintSet.t * (MemOffset.t list) =
      (* let left, right = offset in *)
      let acc_update_list, acc_constraint, acc_undeter_access = acc in
      let range_acc_constraint = MemOffset.ConstraintSet.union (MemOffset.check_offset smt_ctx range) acc_constraint in
      match acc_update_list with
      | [] -> ([ (range, true) ], range_acc_constraint, acc_undeter_access)
      | (hd_range, hd_updated) :: tl ->
        let result = MemOffset.cmp_or_merge smt_ctx range hd_range in
        match result with
        | Some (cmp_result, cmp_constraint) -> begin
            let final_constraint = MemOffset.ConstraintSet.union range_acc_constraint cmp_constraint in
            match cmp_result with
            | Left true -> (* range.right <= hd_range.left *)
              ((range, true) :: acc_update_list, final_constraint, acc_undeter_access)
            | Left false -> (* hd_range.right <= range.left *)
              let udpate_tl, rec_constraint, acc_undeter_access = helper (tl, final_constraint, acc_undeter_access) range in
              ((hd_range, hd_updated) :: udpate_tl, rec_constraint, acc_undeter_access)
            | Right new_range -> (* merge range and hd_range *)
              if MemOffset.cmp hd_range new_range = 0 then (acc_update_list, final_constraint, acc_undeter_access)
              else helper (tl, final_constraint, acc_undeter_access) new_range
          end
        | None -> (acc_update_list, acc_constraint, range :: acc_undeter_access)
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
    List.fold_left helper (acc, MemOffset.ConstraintSet.empty, []) mem_access_list

  (*
   * For memory type, each address has an explicit base
   * For a new access, its base may be unknown. We need to find its base through comparison with exisiting offsets of bases.
   * This function deals with access that has its base confirmed.
   *)
  let rec update_offset_all_ptr
      (smt_ctx: SmtEmitter.t)
      (old_mem_type: (Isa.imm_var_id * (MemOffset.t * TypeExp.t) list) list)
      (mem_access_list: (Isa.imm_var_id * MemOffset.t list) list)
      : ((Isa.imm_var_id * (MemOffset.t * bool) list) list) * MemOffset.ConstraintSet.t * ((Isa.imm_var_id option * MemOffset.t) list) =
    (* pp_mem_key 0 mem_access_list; *)
    let helper_add_base_id_to_range (base_id: Isa.imm_var_id) (offset_list: MemOffset.t list) =
      List.map (fun x -> (Some base_id, x)) offset_list
    in
    match old_mem_type, mem_access_list with
    | (id1, mem1) :: tl1, (id2, mem2) :: tl2 ->
      if id1 = id2 then
        let hd_update, hd_constraint, hd_undeter_access = update_offset_one_ptr smt_ctx mem1 mem2 in
        let tl_update, tl_constraint, tl_undeter_access = update_offset_all_ptr smt_ctx tl1 tl2 in
        (
          (id1, hd_update) :: tl_update,
          MemOffset.ConstraintSet.union hd_constraint tl_constraint, 
          (helper_add_base_id_to_range id2 hd_undeter_access) @ tl_undeter_access
        )
        (* (id1, update_offset_one_ptr mem1 mem2) :: (update_offset_all_ptr tl1 tl2) *)
      else if id1 < id2 then
        let tl_update, tl_constraint, tl_undeter_access = update_offset_all_ptr smt_ctx tl1 mem_access_list in
        ((id1, format_convert_helper mem1) :: tl_update, tl_constraint, tl_undeter_access)
        (* (id1, format_convert_helper mem1) :: (update_offset_all_ptr tl1 mem_access_list) *)
      else (* id2 < id1 *)
        let hd_update, hd_constraint, hd_undeter_access = update_offset_one_ptr smt_ctx [] mem2 in
        let tl_udpate, tl_constraint, tl_undeter_access = update_offset_all_ptr smt_ctx old_mem_type tl2 in
        (
          (id2, hd_update) :: tl_udpate,
          MemOffset.ConstraintSet.union hd_constraint tl_constraint,
          (helper_add_base_id_to_range id2 hd_undeter_access) @ tl_undeter_access
        )
        (* (id2, update_offset_one_ptr [] mem2) :: (update_offset_all_ptr old_mem_type tl2) *)
    | _ :: _, [] -> 
      (List.map (fun (ptr, offset_list) -> (ptr, format_convert_helper offset_list)) old_mem_type, MemOffset.ConstraintSet.empty, [])
    | [], (id2, mem2) :: tl2 ->
      let hd_update, hd_constraint, hd_undeter_access = update_offset_one_ptr smt_ctx [] mem2 in
      let tl_update, tl_constraint, tl_undeter_access = update_offset_all_ptr smt_ctx [] tl2 in
      (
        (id2, hd_update) :: tl_update,
        MemOffset.ConstraintSet.union hd_constraint tl_constraint,
        (helper_add_base_id_to_range id2 hd_undeter_access) @ tl_undeter_access
      )
      (* (id2, update_offset_one_ptr [] mem2) :: (update_offset_all_ptr [] tl2) *)
    | [], [] -> ([], MemOffset.ConstraintSet.empty, [])

  (*
   * This function deals with access that has unknown (implicit) base.
   * Each access goes through all offsets of bases to find a match.
   * After a match is found, it is then treated like a normal access with explicit base.
   *)
  let match_base_for_ptr_without_base
      (smt_ctx: SmtEmitter.t)
      (old_mem_type: (Isa.imm_var_id * (MemOffset.t * TypeExp.t) list) list)
      (mem_access_list: MemOffset.t list) (* access without explicit base *)
      : ((Isa.imm_var_id * MemOffset.t) list) * ((Isa.imm_var_id option * MemOffset.t) list) =
    let flattened_mem_type = 
      List.fold_left (fun acc (base_id, offset_list) ->
        let l = List.map (fun (offset, mem_type) -> (base_id, offset, mem_type)) offset_list in
        l @ acc
      ) [] old_mem_type
    in
    let found, not_found = List.partition_map (fun mem_access ->
      let helper_check_def (_, offset, _) =
        match MemOffset.cmp_or_merge smt_ctx mem_access offset with
        | Some (cmp_result, _) -> begin
            match cmp_result with
            | Left _ -> false
            | Right _ -> true 
          end
        | None -> false
      in
      match List.find_opt helper_check_def flattened_mem_type with
      | Some (base_id, _, _) ->
        Printf.printf "searching base for %s: found %d\n" (MemOffset.to_string mem_access) base_id;
        Left (base_id, mem_access)
      | None ->
        Printf.printf "searching base for %s: not found\n" (MemOffset.to_string mem_access);
        Right (None, mem_access)
    ) mem_access_list
    in
    (found, not_found)

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
      (candidate: (SingleExp.SingleVarSet.t, Isa.imm_var_id) Either.t) : 
      (SingleExp.SingleVarSet.t * SingleExp.SingleVarSet.t) * ((SingleExp.SingleVarSet.t, Isa.imm_var_id) Either.t) =
    let ptr_list, no_ptr_list = acc in
    match candidate with
    | Left base_list ->
      let base_list = SingleExp.SingleVarSet.diff base_list no_ptr_list in
      let inter_list = SingleExp.SingleVarSet.inter base_list ptr_list in
      let diff_list = SingleExp.SingleVarSet.diff base_list ptr_list in
      begin match SingleExp.SingleVarSet.elements inter_list, SingleExp.SingleVarSet.elements diff_list with
      | [], [] -> (acc, Left base_list) (* base not found, leave ptr/no-ptr set as they were *)
      | [], hd :: [] -> ((SingleExp.SingleVarSet.add hd ptr_list, no_ptr_list), Right hd)
      | [], _ :: _ -> (acc, Left diff_list)
      | hd :: [], _ -> ((ptr_list, SingleExp.SingleVarSet.union diff_list no_ptr_list), Right hd)
      | _ :: _, _ -> mem_type_error "try_solve_base add more than one ptrs"
      end
    | Right _ -> (acc, candidate)
  
  let rec solve_base 
      (acc: SingleExp.SingleVarSet.t * SingleExp.SingleVarSet.t)
      (base_list: ((SingleExp.SingleVarSet.t, Isa.imm_var_id) Either.t) list)
      (iter: int) : 
      (SingleExp.SingleVarSet.t * SingleExp.SingleVarSet.t) * Isa.imm_var_id option list =
    let to_id (x: (SingleExp.SingleVarSet.t, Isa.imm_var_id) Either.t) : Isa.imm_var_id option =
      match x with
      | Left s ->
          if MemKeySet.cardinal s = 0 then None (* expect to be implicit base case, e.g. logical-and is involved *)
          else mem_type_error "solve_base cannot find solution: multiple base candidates"
      | Right id -> Some id
    in
    if iter = 0 then (acc, List.map to_id base_list)
    else
      let new_acc, new_list = List.fold_left_map try_solve_base acc base_list in
      solve_base new_acc new_list (iter - 1)

  let get_base 
      (ptr_list: SingleExp.SingleVarSet.t) 
      (no_ptr_list: SingleExp.SingleVarSet.t) 
      (addr_list: (TypeFullExp.t * int64) list) : 
      (SingleExp.SingleVarSet.t * SingleExp.SingleVarSet.t) * Isa.imm_var_id option list =
    let ptr_set_list, no_ptr_set_list = List.split (List.map (fun ((e, _), _) -> filter_type_single_var e) addr_list) in
    let no_ptr_set = List.fold_left (fun acc x -> SingleExp.SingleVarSet.union acc x) SingleExp.SingleVarSet.empty no_ptr_set_list in
    let ptr_set_list = List.map2 (
      fun x (addr, size) : (SingleExp.SingleVarSet.t, Isa.imm_var_id) Either.t ->
        let out_list = SingleExp.SingleVarSet.diff x no_ptr_set in
        if SingleExp.SingleVarSet.cardinal out_list = 0 then
          Printf.printf "Warning: no base candidate for addr %s %Ld\n" (TypeFullExp.to_string addr) size;
        Left out_list
      ) ptr_set_list addr_list
    in
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
      (SingleExp.SingleVarSet.t * SingleExp.SingleVarSet.t) * ((Isa.imm_var_id option * MemOffset.t) list) =
    let addr_list = filter_addr_list addr_list in
    Printf.printf "\nget_addr_base_range: input addr list:\n";
    List.iter (fun ((e, _), _) -> TypeExp.pp_type_exp 0 e; Printf.printf "\n"; ) addr_list;
    Printf.printf "\n";
    let (new_ptr_list, new_no_ptr_list), base_list = get_base ptr_list no_ptr_list addr_list in
    let helper (base_id: Isa.imm_var_id option) (mem_access: TypeFullExp.t * int64) : Isa.imm_var_id option * MemOffset.t =
      let (addr, _), size = mem_access in
      match addr with
      | TypeSingle x -> (
          base_id,
          (
            x,
            SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleAdd, x, SingleExp.SingleConst size))
          )
        )
      | TypeRange (l, _, r, _, _) -> (
          base_id,
          (
            l,
            SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleAdd, r, SingleExp.SingleConst size))
          )
        )
      | _ -> mem_type_error "get_addr_base_range cannot handle this case"
    in
    ((new_ptr_list, new_no_ptr_list), List.map2 helper base_list addr_list)

  let reshape_mem_key_list
      (mem_access_list: (Isa.imm_var_id option * MemOffset.t) list)
      : (Isa.imm_var_id * (MemOffset.t list)) list * (MemOffset.t list) =
    let rec helper_insert
        (l: (Isa.imm_var_id * (MemOffset.t list)) list)
        (mem_access: Isa.imm_var_id * MemOffset.t) =
      let id, offset = mem_access in
      match l with
      | [] -> [(id, [offset])]
      | (hd_id, hd_offset_list) :: tl ->
        if hd_id < id then (hd_id, hd_offset_list) :: (helper_insert tl mem_access)
        else if hd_id = id then (hd_id, offset :: hd_offset_list) :: tl
        else (id, [offset]) :: tl
    in
    let helper
        (acc: (Isa.imm_var_id * (MemOffset.t list)) list * (MemOffset.t list))
        (mem_access: Isa.imm_var_id option * MemOffset.t) =
      let acc_mem, acc_no_base = acc in
      let id_o, offset = mem_access in
      match id_o with
      | None -> acc_mem, offset :: acc_no_base
      | Some id -> (helper_insert acc_mem (id, offset)), acc_no_base
    in
    (* let helper (acc: (Isa.imm_var_id * (MemOffset.t list)) list) (mem_access: Isa.imm_var_id * MemOffset.t) : 
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
    in *)
    List.fold_left helper ([], []) mem_access_list

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

