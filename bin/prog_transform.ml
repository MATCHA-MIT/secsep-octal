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
    Taint_type_infer.TaintTypeInfer.FuncInterface.fi_list_from_file (get_output_filename !program_name "out" "taint_infer") 
  in
  let taint_infer_result_states = 
    Taint_type_infer.TaintTypeInfer.state_list_from_file (get_output_filename !program_name "out" "taint_infer") 
  in
  let _ =
    Transform.Transform.transform_functions taint_infer_result_fi taint_infer_result_states
  in
  ()
  (* in
  Taint_type_infer.TaintTypeInfer.state_list_to_file (get_output_filename !program_name "out" "taint_infer") range_infer_result *)
