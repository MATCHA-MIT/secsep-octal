open Read_file
open Type

let usage_msg = "transform -name <proram_name>"
let program_name = ref ""

let speclist = [
  ("-name", Arg.Set_string program_name, "Set program name")
]

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  let taint_infer_result_fi = 
    Taint_type_infer.TaintTypeInfer.FuncInterface.fi_list_from_file (get_interface_list_filename !program_name) 
  in
  let taint_infer_result_states = 
    Taint_type_infer.TaintTypeInfer.state_list_from_file (get_taint_infer_filename !program_name) 
  in
  let test_func_state = List.hd taint_infer_result_states in
  let init_mem_unity = Transform.Transform.get_func_init_mem_unity test_func_state in
  let _ =
    Transform.Transform.transform_one_function taint_infer_result_fi test_func_state init_mem_unity
  in
  ()
  (* in
  Taint_type_infer.TaintTypeInfer.state_list_to_file (get_taint_infer_filename !program_name) range_infer_result *)
