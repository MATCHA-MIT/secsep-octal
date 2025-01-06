open Isa_basic
open Ptr_info
open External_layouts
open Stack_spill_info
open Single_subtype
open Sexplib.Std

exception BaseFuncInterfaceError of string
let base_func_interface_error msg = raise (BaseFuncInterfaceError ("[Base Func Interface Error] " ^ msg))

type entry_t = IsaBasic.label * SingleSubtype.ArchType.MemType.t * StackSpillInfo.t
[@@deriving sexp]

type t = entry_t list
[@@deriving sexp]

type mem_entry_t = IsaBasic.label * SingleSubtype.ArchType.MemType.t
[@@deriving sexp]

type mem_t = mem_entry_t list
[@@deriving sexp]

let parse_mem (source: string) : mem_t =
  let open Sexplib in
  mem_t_of_sexp (Sexp.of_string source)

let parse (source: string) : t =
  let open Sexplib in
  t_of_sexp (Sexp.of_string source)

let add_stack_layout (interface: mem_t) (stack_layout: StackLayout.t) : t =
  List.map (
    fun (one_func: mem_entry_t) ->
      let label1, mem = one_func in
      let one_stack = List.find (fun one_stack -> String.equal (fst one_stack) label1) stack_layout in
      let _, stack_spill_info = one_stack in
      let stack, spill_info = StackLayout.split_layout stack_spill_info in
      label1, (PtrInfo.get_default_info IsaBasic.rsp_idx, stack) :: mem, StackSpillInfo.init spill_info
  ) interface

let add_global_symbol_layout
    (func_interface: entry_t)
    (imm_var_map: IsaBasic.imm_var_map)
    (used_symbols: IsaBasic.label list)
    (global_symbol_layout: GlobalSymbolLayout.t)
    : entry_t =
  let func_label, func_mem, func_spill_info = func_interface in
  let symbol_layouts = List.map (fun symbol ->
    let var_id = IsaBasic.StrM.find symbol imm_var_map in
    let layout = List.find_map (fun layout ->
      let label = fst layout in
      if String.equal label symbol then
        Some (var_id, snd layout)
      else
        None
    ) global_symbol_layout in
    if Option.is_none layout then
      base_func_interface_error (Printf.sprintf "cannot find layout of global symbol %s for func %s\n" symbol func_label);
    Printf.printf "adding global symbol %s to %s\n%!" symbol func_label;
    let var_id, slots = Option.get layout in
    let slots = List.map (fun (slot: Taint_entry_type.TaintEntryType.t Mem_type_new.MemTypeBasic.mem_slot) ->
      let off, range, (se, _) = slot in
      (off, range, se)
    ) slots in
    (* If symbol start with "."*)
    PtrInfo.get_read_write_info var_id true (not (IsaBasic.is_compiler_gen_rom symbol)), slots
  ) used_symbols in
  func_label, (func_mem @ symbol_layouts), func_spill_info

let add_global_symbol_taint
    (func_label: IsaBasic.label)
    (mem_api: Taint_api.TaintApi.mem_t)
    (imm_var_map: IsaBasic.imm_var_map)
    (used_symbols: IsaBasic.label list)
    (global_symbol_layout: GlobalSymbolLayout.t)
    : Taint_api.TaintApi.mem_t =
  let symbol_layouts = List.map (fun symbol ->
    let var_id = IsaBasic.StrM.find symbol imm_var_map in
    let layout = List.find_map (fun layout ->
      let label = fst layout in
      if String.equal label symbol then
        Some (var_id, snd layout)
      else
        None
    ) global_symbol_layout in
    if Option.is_none layout then
      base_func_interface_error (Printf.sprintf "cannot find layout of global symbol %s for func %s\n" symbol func_label);
    Printf.printf "adding global symbol %s's taint to %s\n%!" symbol func_label;
    let var_id, slots = Option.get layout in
    let slots = List.map (fun (slot: Taint_entry_type.TaintEntryType.t Mem_type_new.MemTypeBasic.mem_slot) ->
      let off, range, (_, taint) = slot in
      let bool_val = match taint with
      | TaintConst b -> Some b
      | _ -> base_func_interface_error "unexpected taint var"
      in
      (off, range, bool_val)
    ) slots in
    (* If symbol start with "."*)
    PtrInfo.get_read_write_info var_id true (not (IsaBasic.is_compiler_gen_rom symbol)), slots
  ) used_symbols in
  mem_api @ symbol_layouts
