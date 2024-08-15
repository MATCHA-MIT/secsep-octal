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
