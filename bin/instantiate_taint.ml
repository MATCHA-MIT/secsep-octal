open Read_file
open Type

let usage_msg = "instantiate_taint -name <program_name>"
let program_name = ref ""

let speclist = [
  ("-name", Arg.Set_string program_name, "Set program name")
]

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  let taint_api =
    Taint_api.TaintApi.api_from_file (get_output_filename !program_name "interface" "taint_api")
  in
  let func_interface_list =
    Taint_type_infer.TaintTypeInfer.FuncInterface.interface_list_from_file (get_output_filename !program_name "out" "interface")
  in
  let taint_infer_result =
    Taint_type_infer.TaintTypeInfer.state_list_from_file (get_output_filename !program_name "out" "taint_infer")
  in
  let func_interface_list, instantiate_result = 
    Taint_instantiate.TaintInstantiate.instantiate taint_api func_interface_list taint_infer_result 
  in
  Taint_type_infer.TaintTypeInfer.state_list_to_file (get_output_filename !program_name "out" "instantiate_taint_infer") instantiate_result;
  Taint_type_infer.TaintTypeInfer.FuncInterface.interface_list_to_file (get_output_filename !program_name "out" "instantiate_interface") func_interface_list
