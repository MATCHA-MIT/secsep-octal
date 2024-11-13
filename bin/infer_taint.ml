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
    Range_type_infer.RangeTypeInfer.state_list_from_file (get_output_filename !program_name "out" "range_infer") 
  in
  let general_interface_list = 
    Func_interface.FuncInterfaceConverter.TaintFuncInterface.interface_list_from_file "./interface/general_func_interface.func_interface" 
  in
  let func_interface_list, taint_infer_result =
    Taint_type_infer.TaintTypeInfer.infer range_infer_result general_interface_list
  in
  Taint_type_infer.TaintTypeInfer.state_list_to_file (get_output_filename !program_name "out" "taint_infer") taint_infer_result;
  Taint_type_infer.TaintTypeInfer.FuncInterface.interface_list_to_file (get_output_filename !program_name "out" "interface") func_interface_list
