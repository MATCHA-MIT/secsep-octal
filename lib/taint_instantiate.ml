open Taint_exp
open Taint_type_infer


module TaintInstantiate = struct
  exception TaintInstantiateError of string

  let taint_instantiate_error msg = raise (TaintInstantiateError ("[Taint Instantiate Error] " ^ msg))

  module Isa = TaintTypeInfer.Isa
  module FuncInterface = TaintTypeInfer.FuncInterface

  type call_site_map_t = (Isa.label * TaintExp.TaintVarSet.t * ((Isa.label * TaintExp.local_var_map_t) list)) list
  type instance_map_t = (Isa.label * TaintExp.local_var_map_t) list

  (* 1. Init taint var map only contains taint var that are not used for callee saved register type *)
  let get_one_func_taint_instantiate_set
      (func_interface: FuncInterface.t) :
      TaintExp.TaintVarSet.t =
    let var_set, _ =
      List.fold_left (
        fun (acc: TaintExp.TaintVarSet.t * int) (entry: TaintTypeInfer.TaintEntryType.t) ->
          let acc_set, acc_idx = acc in
          let _, taint = entry in
          if Isa.is_reg_idx_callee_saved acc_idx then
            acc_set, acc_idx + 1
          else
            TaintExp.TaintVarSet.union (TaintExp.get_var_set taint) acc_set, acc_idx + 1
      ) (TaintExp.TaintVarSet.empty, 0) func_interface.in_reg
    in
    TaintTypeInfer.ArchType.MemType.fold_left (
      fun (acc: TaintExp.TaintVarSet.t) (entry: TaintTypeInfer.TaintEntryType.t) ->
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
        (entry: int * TaintExp.t) :
        TaintExp.local_var_map_t =
      let var_idx, var_instance = entry in
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
      (prog: Isa.prog) (func_interface_list: FuncInterface.t list) : call_site_map_t =
    let map =
      List.map2 (
        fun (x: Isa.func) (interface: FuncInterface.t) ->
        x.name, get_one_func_taint_instantiate_set interface, []
      ) prog.funcs func_interface_list
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
      fun (acc: call_site_map_t) (func: Isa.func) ->
        List.fold_left (
          fun (acc: call_site_map_t) (block: Isa.basic_block) ->
            List.fold_left (helper func.name) acc block.insts
        ) acc func.body
    ) map prog.funcs
  
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
      Printf.printf "Warning: get_instance_map cannot find map for instance %s" func_name;
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
    ) [] call_site_map

    

end


