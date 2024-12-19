open Taint_exp
open Taint_entry_type
open Taint_api
open Taint_type_infer
open Mem_offset_new
open Sexplib.Std


module TaintInstantiate = struct
  exception TaintInstantiateError of string

  let taint_instantiate_error msg = raise (TaintInstantiateError ("[Taint Instantiate Error] " ^ msg))

  module Isa = TaintTypeInfer.Isa
  module FuncInterface = TaintTypeInfer.FuncInterface

  type call_site_map_t = (Isa.label * TaintExp.TaintVarSet.t * ((Isa.label * TaintExp.local_var_map_t) list)) list
  [@@deriving sexp]

  type instance_map_t = (Isa.label * TaintExp.local_var_map_t) list
  [@@deriving sexp]

  (* 1. Init taint var map only contains taint var that are not used for callee saved register type *)
  let get_one_func_taint_instantiate_set
      (func_interface: FuncInterface.t) :
      TaintExp.TaintVarSet.t =
    let var_set, _ =
      List.fold_left (
        fun (acc: TaintExp.TaintVarSet.t * int) (entry: TaintEntryType.t) ->
          let acc_set, acc_idx = acc in
          let _, taint = entry in
          if Isa.is_reg_idx_callee_saved acc_idx then
            acc_set, acc_idx + 1
          else
            TaintExp.TaintVarSet.union (TaintExp.get_var_set taint) acc_set, acc_idx + 1
      ) (TaintExp.TaintVarSet.empty, 0) func_interface.in_reg
    in
    TaintTypeInfer.ArchType.MemType.fold_left (
      fun (acc: TaintExp.TaintVarSet.t) (entry: TaintEntryType.t) ->
        let _, taint = entry in
        TaintExp.TaintVarSet.union (TaintExp.get_var_set taint) acc
    ) var_set func_interface.in_mem

  let update_one_taint_instance
      (instantiate_set: TaintExp.TaintVarSet.t)
      (map: TaintExp.local_var_map_t)
      (instance_map: TaintExp.local_var_map_t) :
      TaintExp.local_var_map_t =
    let helper
        (map: TaintExp.local_var_map_t)
        (instance_entry: int * TaintExp.t) :
        TaintExp.local_var_map_t =
      let var_idx, var_instance = instance_entry in
      if TaintExp.TaintVarSet.mem var_idx instantiate_set then
        let found, map =
          List.fold_left_map (
            fun (acc: bool) (entry: TaintExp.taint_var_id * TaintExp.t) ->
              if acc then acc, entry
              else
                let idx, instance = entry in
                if var_idx = idx then
                  true,
                  match instance, var_instance with
                  | TaintConst true, TaintConst true -> idx, TaintExp.TaintConst true
                  | TaintConst false, TaintConst false -> idx, TaintConst false
                  | _ -> idx, TaintVar idx
                else false, entry
          ) false map
        in
        if found then map
        else
          match var_instance with
          | TaintConst _ -> (var_idx, var_instance) :: map
          | _ -> (var_idx, TaintVar var_idx) :: map
      else
        map
    in
    List.fold_left helper map instance_map

  let gen_call_site_map
      (func_interface_list: FuncInterface.t list)
      (infer_state_list: TaintTypeInfer.t list) : call_site_map_t =
    let map =
      List.map (
        fun (x: TaintTypeInfer.t) ->
          let fi = FuncInterface.find_fi func_interface_list x.func_name |> Option.get in
          x.func_name, get_one_func_taint_instantiate_set fi, []
      ) infer_state_list
    in
    let helper
        (caller_name: Isa.label)
        (acc: call_site_map_t) (inst: Isa.instruction) :
        call_site_map_t =
      match inst with
      | Call (calee_name, call_anno_opt) ->
        begin match call_anno_opt with
        | None -> taint_instantiate_error "get_call_site_map get None call anno"
        | Some call_anno ->
          List.map (
            fun (func_name, var_set, call_site_list) ->
              if func_name = calee_name then
                func_name, var_set, (caller_name, call_anno.taint_var_map) :: call_site_list
              else
                func_name, var_set, call_site_list
          ) acc
        end
      | _ -> acc
    in
    List.fold_left (
      fun (acc: call_site_map_t) (infer_state: TaintTypeInfer.t) ->
        List.fold_left (
          fun (acc: call_site_map_t) (block: Isa.basic_block) ->
            List.fold_left (helper infer_state.func_name) acc block.insts
        ) acc infer_state.func
    ) map infer_state_list
  
  let gen_call_site_map_from_taint_api
      (func_interface_list: FuncInterface.t list)
      (taint_api: TaintApi.t) : call_site_map_t =
    let helper 
        (acc: TaintExp.local_var_map_t) 
        (entry: TaintEntryType.t) 
        (instance: bool option) : TaintExp.local_var_map_t =
      let _, taint = entry in
      match taint, instance with
      | TaintExp _, Some _ -> acc (* TODO: Double check!!! *)
        (* List.fold_left (
          fun acc x -> TaintExp.add_local_var acc (TaintVar x) (TaintConst b)
        ) acc (TaintExp.TaintVarSet.to_list var_set) *)
      | _, Some b -> TaintExp.add_local_var acc taint (TaintConst b) 
      | _ -> acc
    in
    List.map (
      fun (func_name, reg_api, mem_api) ->
        let interface = 
          List.find (fun (x: FuncInterface.t) -> x.func_name = func_name) func_interface_list 
        in

        let map = List.fold_left2 helper [] interface.in_reg reg_api in

        let str_of_bool_opt (x: bool option) =
          match x with
          | None -> "None"
          | Some b -> Printf.sprintf "Some %b" b
        in
        let abs_mem_api = TaintTypeInfer.ArchType.MemType.add_base_to_offset mem_api in
        Printf.printf "\nfunc: %s\n" func_name;
        Printf.printf "in_mem:\n";
        Mem_type_new.MemTypeBasic.pp_mem_type_generic 1 interface.in_mem None;
        (* Printf.printf "taint_api:\n";
        Mem_type_new.MemTypeBasic.pp_mem_type_generic 1 abs_mem_api (Some str_of_bool_opt); *)

        (* construct same layout using None as the placeholder *)
        (* then according to the taint api, find the slot and change its value *)
        let sorted_mem_api = List.map (fun in_mem_part ->
          let base_id, in_mem_slots = in_mem_part in
          let api_part = List.find (fun api ->
            let api_base_id, _ = api in
            base_id = api_base_id
          ) abs_mem_api
          in
          let result = List.map (fun slot ->
            let off, range, _ = slot in
            (off, range, None)
          ) in_mem_slots
          in
          let result = List.fold_left (fun acc api_slot ->
            let api_off, _, api_value = api_slot in
            let found, acc = List.fold_left_map (fun found slot ->
              let off, range, _ = slot in
              if MemOffset.cmp off api_off = 0 then
                true, (off, range, api_value)
              else
                found, slot
            ) false acc
            in
            if not found then
              taint_instantiate_error (
                Printf.sprintf "cannot find slot %s in %s's in_mem\n" (MemOffset.to_string api_off) func_name
              )
            else
              acc
          ) result (snd api_part) in
          (base_id, result)
        ) interface.in_mem in
        Printf.printf "sorted taint_api:\n";
        Mem_type_new.MemTypeBasic.pp_mem_type_generic 1 sorted_mem_api (Some str_of_bool_opt);
        Printf.printf "%!";
        let map = TaintTypeInfer.ArchType.MemType.fold_left2 helper map interface.in_mem sorted_mem_api in

        func_name, get_one_func_taint_instantiate_set interface, [ "", map ]
    ) taint_api

  let merge_call_site_map
      (map1: call_site_map_t) (map2: call_site_map_t) : call_site_map_t =
    List.fold_left (
      fun (map1: call_site_map_t) (name2, var_set2, call_site_list2) ->
        let found, map1 =
        List.fold_left_map (
          fun (acc: bool) (name1, var_set1, call_site_list1) ->
            if (not acc) && name1 = name2 then
              true, (name1, var_set1, call_site_list2 @ call_site_list1)
            else
              acc, (name1, var_set1, call_site_list1)
        ) false map1
        in
        if found then map1
        else (name2, var_set2, call_site_list2) :: map1
    ) map1 map2

  let get_instance_map
      (instance_map: instance_map_t)
      (func_name: Isa.label) : TaintExp.local_var_map_t =
    let map_opt =
      List.find_map (
        fun (name, map) ->
          if name = func_name then Some map
          else None
      ) instance_map
    in
    match map_opt with
    | Some map -> map
    | None -> 
      if func_name = "" then () else
      Printf.printf "Warning: get_instance_map cannot find map for instance %s\n" func_name;
      []

  let gen_instance_map
      (other_instance_map: instance_map_t)
      (instantiate_set: TaintExp.TaintVarSet.t)
      (call_site_list: (Isa.label * TaintExp.local_var_map_t) list):
      TaintExp.local_var_map_t =
    let map = 
      List.fold_left (
        fun (acc: TaintExp.local_var_map_t) (caller_name, call_map) ->
          let repl_map = get_instance_map other_instance_map caller_name in
          let call_map =
            List.map (
              fun (id, exp) -> id, TaintExp.repl_context_var_no_error repl_map exp
            ) call_map
          in
          update_one_taint_instance instantiate_set acc call_map
      ) [] call_site_list
    in
    List.filter (
      fun (_, exp) ->
        match exp with
        | TaintExp.TaintConst _ -> true
        | _ -> false
    ) map

  let get_all_instance_map
      (call_site_map: call_site_map_t) :
      instance_map_t =
    List.fold_left (
      fun (acc: instance_map_t) (func_name, instantiate_set, call_site_list) ->
        (func_name, (gen_instance_map acc instantiate_set call_site_list)) :: acc
    ) [] (List.rev call_site_map)

  let update_all
      (instance_map_list: instance_map_t)
      (infer_state_list: TaintTypeInfer.t list) :
      (FuncInterface.t list) * (TaintTypeInfer.t list) =
    List.map2 (
      fun (_, map) (infer_state: TaintTypeInfer.t) ->
      let state = TaintTypeInfer.update_with_taint_sol map infer_state in
      TaintTypeInfer.get_func_interface state, state
    ) instance_map_list infer_state_list |> List.split

  let instantiate
      (prog: Isa.prog)
      (taint_api: TaintApi.t)
      (func_interface_list: FuncInterface.t list)
      (infer_state_list: TaintTypeInfer.t list)
      (global_symbol_layout: External_layouts.GlobalSymbolLayout.t) :
      (FuncInterface.t list) * (TaintTypeInfer.t list) =
    let open Sexplib in

    (* add taint initialization of global symbols *)
    let taint_api = List.map (fun (api: TaintApi.api_t) ->
      let label, reg_api, mem_api = api in
      let func = List.find (fun (f: Isa.func) -> String.equal f.name label) prog.funcs in
      let mem_api = Base_func_interface.add_global_symbol_taint
        func.name
        mem_api
        prog.imm_var_map
        func.related_gsymbols
        global_symbol_layout
      in
      (label, reg_api, mem_api)
    ) taint_api
    in

    let call_site_map_api = gen_call_site_map_from_taint_api func_interface_list taint_api in
    let call_site_map = gen_call_site_map func_interface_list infer_state_list in
    let call_site_map = merge_call_site_map call_site_map call_site_map_api in
    Sexp.output_hum stdout (sexp_of_call_site_map_t call_site_map);
    Printf.printf "\n";
    let instance_map_list = get_all_instance_map call_site_map in
    Sexp.output_hum stdout (sexp_of_instance_map_t instance_map_list);
    Printf.printf "\n";
    update_all instance_map_list infer_state_list

end


