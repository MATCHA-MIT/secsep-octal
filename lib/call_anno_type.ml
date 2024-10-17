open Isa_basic
open Reg_type_new
open Taint_exp
open Single_entry_type
open Taint_entry_type
open Mem_type_new
open Mem_offset_new
open Sexplib.Std

module CallAnno = struct

  exception CallAnnoError of string
  let transform_error msg = raise (CallAnnoError ("[Call Annotation Error] " ^ msg))

  module TaintEntryType = TaintEntryType(SingleEntryType)
  module TaintRegType = RegType(TaintEntryType)

  (* which slot in parent's memory is this slot mapped to *)
  (* base pointer, slot's offset to base pointer, accessing full slot or not *)
  type slot_info = IsaBasic.imm_var_id * MemOffset.t * bool
  [@@deriving sexp]

  (* how is its base pointer passed during the call *)
  type base_info =
    | BaseAsReg of IsaBasic.register (* passed as an arg register *)
    | BaseAsSlot of IsaBasic.imm_var_id * MemOffset.t (* passed as a (full) slot in child's memory *)
    | BaseAsGlobal (* Base is a global variable, no need to pass through pointers *)
  [@@deriving sexp]

  type slot_t = (slot_info * base_info)
  [@@deriving sexp]

  type t' = {
    pr_reg: TaintRegType.t;
    ch_mem: slot_t MemTypeBasic.mem_content;
  }
  [@@deriving sexp]

  type t = t' option
  [@@deriving sexp]

  let cmp_base_info (x: base_info) (y: base_info) : int =
    match x, y with
    | BaseAsReg r1, BaseAsReg r2 -> Int.compare (IsaBasic.get_reg_idx r1) (IsaBasic.get_reg_idx r2)
    | BaseAsSlot (v1, o1), BaseAsSlot (v2, o2) ->
      let cmp1 = Int.compare v1 v2 in
      if cmp1 != 0 then cmp1
      else MemOffset.cmp o1 o2
    | BaseAsReg _, BaseAsSlot _ -> -1
    | BaseAsSlot _, BaseAsReg _ -> 1
    | BaseAsGlobal, BaseAsGlobal -> 0
    | _, BaseAsGlobal -> -1
    | BaseAsGlobal, _ -> 1

  let to_ocaml_string (anno: t) : string =
    match anno with
    | None -> "None"
    | Some _ -> "Some (call anno...)"

  let to_string (anno: t) : string = to_ocaml_string anno

  let get_call_anno
      (pr_reg: TaintRegType.t) 
      (call_mem_read_hint_option: slot_info MemTypeBasic.mem_content option)
      (base_info: base_info MemTypeBasic.mem_content) : t =
    match call_mem_read_hint_option with
    | None -> None
    | Some call_mem_read_hint ->
      Some {
        pr_reg = pr_reg;
        ch_mem = MemTypeBasic.map2 (fun x y -> (x, y)) call_mem_read_hint base_info;
      }

  let update_taint (update_taint: TaintExp.t -> TaintExp.t) (anno: t) : t =
    let update_taint_entry (entry: TaintEntryType.t) : TaintEntryType.t =
      let single, taint = entry in
      single, update_taint taint
    in
    match anno with
    | None -> None
    | Some anno' ->
      Some { anno' with pr_reg = List.map update_taint_entry anno'.pr_reg }

end
