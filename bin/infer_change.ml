open Read_file
open Type

let usage_msg = "infer_change -name <program_name>"
let program_name = ref ""

let speclist = [
  ("-name", Arg.Set_string program_name, "Set program name")
]

let () =
  let start_time = Sys.time () in

  Arg.parse speclist (fun _ -> ()) usage_msg;

  (* 1. Parse single infer output *)
  let single_infer_result = 
    Single_type_infer.SingleTypeInfer.state_list_from_file (get_related_filename !program_name "out" "single_infer") 
  in

  (* 2. Infer change var *)
  let change_list = Single_change_var_infer.SingleChangeVarInfer.infer single_infer_result in

  (* 3. Combine it with taint infer output *)
  let tti_list = Taint_type_infer.TaintTypeInfer.state_list_from_file
    (get_related_filename !program_name "out" "taint_infer")
  in
  let tti_list = List.map2 (
    fun (tti: Taint_type_infer.TaintTypeInfer.t) (change: Single_change_var_infer.SingleChangeVarInfer.t) ->
      { tti with func_type = Single_change_var_infer.SingleChangeVarInfer.update_arch_type change tti.input_single_var_set tti.func_type }
  ) tti_list change_list in
  Taint_type_infer.TaintTypeInfer.state_list_to_file (get_related_filename !program_name "out" "taint_infer") tti_list;

  let interface_list = Taint_type_infer.TaintTypeInfer.FuncInterface.interface_list_from_file
      (get_related_filename !program_name "out" "interface")
  in
  let get_in_change_var_helper (func_name: string) = 
    List.find_map (
      fun (tti: Taint_type_infer.TaintTypeInfer.t) -> 
        if tti.func_name = func_name then 
          Some (fst (List.hd tti.func_type).change_var)
        else None
    ) tti_list
  in
  let interface_list = Single_change_var_infer.SingleChangeVarInfer.update_func_interface get_in_change_var_helper interface_list in
  Taint_type_infer.TaintTypeInfer.FuncInterface.interface_list_to_file (get_related_filename !program_name "out" "interface") interface_list;

  let end_time = Sys.time () in
  Stat.stat.time <- end_time -. start_time;
  Stat.statistics_to_file (get_related_filename !program_name "out" "change_infer.stat")
