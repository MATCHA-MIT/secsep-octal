open Pretty_print
open Smt_emitter
open Isa
open Single_exp
open Entry_type
open Mem_offset
open Constraint
(* open Arch_type *)

module MemKeySet = struct
include Set.Make(Int)
  
  exception MemKeySetError of string
  let mem_key_set_error msg = raise (MemKeySetError ("[Mem Key Set Error] " ^ msg))

  let pp_list (lvl: int) (ptr_list: Isa.imm_var_id list) =
    PP.print_lvl lvl "Ptr list: ";
    List.iter (
      fun x -> Printf.printf "%d " x
    ) ptr_list;
    Printf.printf "\n"

  let pp_set (lvl: int) (ptr_set: t) =
    pp_list lvl (elements ptr_set)

  let rec filter_single_var (addr: SingleExp.t) : t =
    match addr with
    | SingleVar x -> singleton x
    | SingleBExp (SingleExp.SingleAdd, l, r) ->
      let left_ptr = filter_single_var l in
      let right_ptr = filter_single_var r in
      union left_ptr right_ptr
    | _ -> empty
  
  let find_base (e: SingleExp.t) (ptr_list: t) : Isa.imm_var_id option =
    let p_list = filter_single_var e in
    match to_list (inter p_list ptr_list) with
    | [] -> None
    | hd :: [] -> Some hd
    | _ -> mem_key_set_error "find_base find more than one base"

end

module MemType (Entry: EntryType) = struct
  exception MemTypeError of string
  let mem_type_error msg = raise (MemTypeError ("[Mem Type Error] " ^ msg))

  type entry_t = Entry.t
  type 'a mem_content = (Isa.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list)) list
  type t = entry_t mem_content

  let pp_mem_type (lvl: int) (mem: t) =
    PP.print_lvl lvl "<MemType>\n";
    List.iter (
      fun (ptr, off_list) ->
        PP.print_lvl (lvl + 1) "<Ptr %d>\n" ptr;
        List.iter (
          fun (off, _, entry) ->
            PP.print_lvl (lvl + 2) "%s\t%s\n" (MemOffset.to_string off) (Entry.to_string entry)
        ) off_list
    ) mem

  let fold_left
      (func: 'acc -> 'a -> 'acc)
      (acc: 'acc)
      (mem: 'a mem_content) : 'acc =
    let helper_inner
        (acc: 'acc) (entry: MemOffset.t * MemRange.t * 'a) : 'acc =
      let _, _, e = entry in
      func acc e
    in
    let helper_outer
        (acc: 'acc) (entry: Isa.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list)) : 'acc =
      let _, l = entry in
      List.fold_left helper_inner acc l
    in
    List.fold_left helper_outer acc mem

  let fold_left_map
      (func: 'acc -> 'a -> ('acc * 'a))
      (acc: 'acc)
      (mem: 'a mem_content) : 'acc * ('a mem_content) =
    let helper_inner
        (acc: 'acc) (entry: MemOffset.t * MemRange.t * 'a): 
        'acc * (MemOffset.t * MemRange.t * 'a) =
      let off, range, e = entry in
      let acc, e = func acc e in
      acc, (off, range, e)
    in
    let helper_outer
        (acc: 'acc) (entry: Isa.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list)) : 
        'acc * (Isa.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list)) =
      let id, l = entry in
      let acc, l = List.fold_left_map helper_inner acc l in
      acc, (id, l)
    in
    List.fold_left_map helper_outer acc mem

  let fold_left2 
      (func: 'acc -> 'a -> 'b -> 'acc)
      (acc: 'acc)
      (mem1: 'a mem_content)
      (mem2: 'b mem_content) : 'acc =
    let helper_inner
        (acc: 'acc)
        (entry1: MemOffset.t * MemRange.t * 'a)
        (entry2: MemOffset.t * MemRange.t * 'b) : 'acc =
      let off1, _, e1 = entry1 in
      let off2, _, e2 = entry2 in
      if MemOffset.cmp off1 off2 = 0 then func acc e1 e2
      else mem_type_error "[fold_left2] mem offset does not match"
    in
    let helper_outer
        (acc: 'acc) 
        (entry1: Isa.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list))
        (entry2: Isa.imm_var_id * ((MemOffset.t * MemRange.t * 'b) list)) : 'acc =
      let v1, l1 = entry1 in
      let v2, l2 = entry2 in
      if v1 = v2 then List.fold_left2 helper_inner acc l1 l2
      else mem_type_error "[fold_left2] ptr does not match"
    in
    List.fold_left2 helper_outer acc mem1 mem2

  let add_base_to_offset (mem_layout: 'a mem_content) : 'a mem_content =
    List.map (
      fun (base, off_list) ->
        (base,
        List.map (
          fun ((l, r), range, entry) ->
            (SingleExp.eval (SingleBExp (SingleAdd, SingleVar base, l)), 
            SingleExp.eval (SingleBExp (SingleAdd, SingleVar base, r))),
            range, entry
        ) off_list
        )
    ) mem_layout

  let init_mem_type_from_layout
      (start_var: entry_t) (mem_layout: 'a mem_content) : entry_t * t =
    let helper (acc: entry_t) (entry: 'a) : entry_t * 'a =
      let _ = entry in
      Entry.next_var acc, acc
    in
    fold_left_map helper start_var mem_layout
    (* let _ = mem_layout in start_var, [] *)

  (* let get_mem_entry_one_ptr_helper
      (smt_ctx: SmtEmitter.t)
      (mem: (MemOffset.t * MemRange.t * 'a) list)
      (addr_offset: MemOffset.t) :
      ((MemOffset.t * MemRange.t * 'a) * bool * MemOffset.ConstraintSet.t) option =
    let helper 
        (acc: ((MemOffset.t * MemRange.t * 'a) * bool * MemOffset.ConstraintSet.t) option)
        (entry: MemOffset.t * MemRange.t * 'a) :
        ((MemOffset.t * MemRange.t * 'a) * bool * MemOffset.ConstraintSet.t) option =
      let entry_offset, _, _ = entry in
      match acc with
      | None ->
        if MemOffset.equal addr_offset entry_offset then begin
          Some (entry, true, MemOffset.ConstraintSet.empty)
        end else begin
          match MemOffset.
        end *)

  (* let get_mem_entry 
      (smt_ctx: SmtEmitter.t) 
      (mem: t) (ptr_list: MemKeySet.t) 
      (addr_offset: MemOffset.t) :
      ((Isa.imm_var_id * MemOffset.t * MemRange.t * entry_t) * MemOffset.ConstraintSet.t) option =
    let addr_l, addr_r = addr_offset in
    let base_opt = MemKeySet.find_base addr_l ptr_list in
    (* The accessed address range [l, r] must satisfy l < r *)
    (* Maybe we do not need this! *)
    let addr_constraint = MemOffset.check_offset smt_ctx addr_offset in
    match base_opt with
    |  *)

  let get_mem_type
      (smt_ctx: SmtEmitter.t)
      (mem: t)
      (addr_offset: MemOffset.t) :
      (MemOffset.t * MemRange.t * entry_t) option =
    let _, _, _ = smt_ctx, mem, addr_offset in
    None
    (* TODO!!! *)

  let set_mem_type
      (smt_ctx: SmtEmitter.t)
      (mem: t)
      (addr_offset: MemOffset.t)
      (new_type: entry_t) :
      (t * (Constraint.t list)) option =
    let _, _, _, _ = smt_ctx, mem, addr_offset, new_type in
    None
    (* TODO!!! *)

end
