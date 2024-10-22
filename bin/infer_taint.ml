open Read_file
open Type

let usage_msg = "infer_taint -name <proram_name>"
let program_name = ref ""

let speclist = [
  ("-name", Arg.Set_string program_name, "Set program name")
]

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  let range_infer_result = 
    Range_type_infer.RangeTypeInfer.state_list_from_file (get_range_infer_filename !program_name) 
  in
  let func_interface_list, taint_infer_result =
    Taint_type_infer.TaintTypeInfer.infer range_infer_result
  in
  Taint_type_infer.TaintTypeInfer.state_list_to_file (get_taint_infer_filename !program_name) taint_infer_result;
  Taint_type_infer.TaintTypeInfer.FuncInterface.interface_list_to_file (get_interface_list_filename !program_name) func_interface_list
