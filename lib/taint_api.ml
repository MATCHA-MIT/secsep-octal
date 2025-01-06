open Isa
open Ptr_info
open Single_entry_type
open Taint_exp
open Taint_entry_type
open Mem_offset_new
open Reg_type_new
open Mem_type_new
open Full_mem_anno
open External_layouts
open Arch_type
open Sexplib.Std

module TaintApi = struct
  exception TaintApiError of string
  let taint_api_error msg = raise (TaintApiError ("[Taint Api Error] " ^ msg))

  module MemAnno = FullMemAnno
  module Isa = Isa (MemAnno)
  module SingleArchType = ArchType (SingleEntryType)
  module TaintRegType = RegType (TaintEntryType)
  module TaintMemType = MemType (TaintEntryType)

  type mem_t = ((bool option) MemTypeBasic.mem_content)
  [@@deriving sexp]

  type api_t = Isa.label * ((bool option) list) * mem_t
  [@@deriving sexp]

  type t = api_t list
  [@@deriving sexp]

  let api_from_file (filename: string) : t =
    let open Sexplib in
    let channel = open_in filename in
    let s_exp = Sexp.input_sexp channel in
    t_of_sexp s_exp

  let add_global_symbol_taint_helper
      (func_label: Isa.label)
      (mem_api: mem_t)
      (imm_var_map: Isa.imm_var_map)
      (used_symbols: Isa.label list)
      (global_symbol_layout: GlobalSymbolLayout.t)
      : mem_t =
    let symbol_layouts = List.map (fun symbol ->
      let var_id = Isa.StrM.find symbol imm_var_map in
      let layout = List.find_map (fun layout ->
        let label = fst layout in
        if String.equal label symbol then
          Some (var_id, snd layout)
        else
          None
      ) global_symbol_layout in
      if Option.is_none layout then
        taint_api_error (Printf.sprintf "cannot find layout of global symbol %s for func %s\n" symbol func_label);
      Printf.printf "adding global symbol %s's taint to %s\n%!" symbol func_label;
      let var_id, slots = Option.get layout in
      let slots = 
        List.map (fun (slot: Taint_entry_type.TaintEntryType.t Mem_type_new.MemTypeBasic.mem_slot) ->
          let off, range, (_, taint) = slot in
          let bool_val = match taint with
          | TaintConst b -> Some b
          | _ -> taint_api_error "unexpected taint var"
          in
          (MemOffset.add_base (SingleVar var_id) off, MemRange.add_base (SingleVar var_id) range, bool_val)
        ) slots 
      in
      (* If symbol start with "."*)
      PtrInfo.get_read_write_info var_id true (not (Isa.is_compiler_gen_rom symbol)), slots
    ) used_symbols in
    mem_api @ symbol_layouts

  let add_global_symbol_taint 
      (prog: Isa.prog) (global_symbol_layout: External_layouts.GlobalSymbolLayout.t)
      (api_list: t) : t =
    
    let result =
    List.map (
      fun (api: api_t) ->
        let label, reg_api, mem_api = api in
        let func = List.find (fun (f: Isa.func) -> String.equal f.name label) prog.funcs in
        let mem_api = add_global_symbol_taint_helper
          func.name
          mem_api
          prog.imm_var_map
          func.related_gsymbols
          global_symbol_layout
        in
        (label, reg_api, mem_api) 
    ) api_list
    in
    Printf.printf "add_global_symbol_taint\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_t result));
    result

  let get_taint_api
      (api_list: t) (func_name: Isa.label) : ((bool option) list option) * (mem_t option) =
    match List.find_opt (fun (label, _, _) -> label = func_name) api_list with
    | Some (_, reg_api, mem_api) -> Some reg_api, Some mem_api
    | None -> None, None

  let merge_reg_type_api
      (reg_type: 'a list) (reg_api: (bool option) list option) :
      (('a * (bool option)) list) =
    match reg_api with
    | Some reg_api ->
      List.map2 (fun x y -> x, y) reg_type reg_api
    | None ->
      List.map (fun x -> x, None) reg_type

  let merge_mem_type_api
      (mem_type: 'a MemTypeBasic.mem_content) (mem_api: mem_t option) :
      ('a * (bool option)) MemTypeBasic.mem_content =
    match mem_api with
    | None -> MemTypeBasic.map (fun x -> x, None) mem_type
    | Some mem_api ->
      (* Printf.printf "Get mem_api\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_mem_t mem_api));
      Printf.printf "MemType\n%s\n" (Sexplib.Sexp.to_string_hum (SingleArchType.MemType.sexp_of_t mem_type)); *)
      List.map (
        fun (part_mem: 'a MemTypeBasic.mem_part) ->
          let ptr, off_list = part_mem in
          let find_ptr_api =
            List.find_opt (
              fun (api_part_mem: (bool option) MemTypeBasic.mem_part) ->
                let api_ptr, _ = api_part_mem in ptr = api_ptr
            ) mem_api
          in
          match find_ptr_api with
          | None ->
            ptr,
            List.map (fun (off, range, entry) -> off, range, (entry, None)) off_list
          | Some (_, ptr_api_off_list) ->
            ptr,
            List.map (
              fun (off, range, entry) ->
                let find_entry =
                  List.find_map (
                    fun (ptr_off, _, ptr_entry) ->
                      if MemOffset.cmp off ptr_off = 0 then ptr_entry
                      else None
                  ) ptr_api_off_list
                in
                off, range, (entry, find_entry)
            ) off_list
      ) mem_type

  let get_taint_arch_type
      (api_list: t) 
      (acc_taint_exp: TaintExp.t) (single_arch_type: SingleArchType.t) :
      TaintExp.t * TaintRegType.t * TaintMemType.t =
    let inner_helper
        (acc: TaintExp.t) (entry: SingleEntryType.t * (bool option)) : 
        TaintExp.t * TaintEntryType.t =
      let single_entry, taint_instance_opt = entry in
      match taint_instance_opt with
      | None -> TaintExp.next_var acc, (single_entry, acc)
      | Some taint -> acc, (single_entry, TaintConst taint)
    in

    let reg_api, mem_api = get_taint_api api_list single_arch_type.label in

    let reg_type_api = merge_reg_type_api single_arch_type.reg_type reg_api in
    let mem_type_api = merge_mem_type_api single_arch_type.mem_type mem_api in

    let acc_taint_exp, reg_type = List.fold_left_map inner_helper acc_taint_exp reg_type_api in
    let acc_taint_exp, mem_type = MemTypeBasic.fold_left_map inner_helper acc_taint_exp mem_type_api in

    acc_taint_exp, reg_type, mem_type

end
